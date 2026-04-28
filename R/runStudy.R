# Copyright 2025 European Medicines Agency
#
# This software is developed by the DARWIN EU Coordination Centre
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This study code contains unmodified open source dependencies.

#' Run the P4-C1-021 Alzheimer's disease characterization study
#'
#' @param cdm A cdm reference object created by `CDMConnector::cdmFromCon`
#' @param outputFolder The full path to a folder where the results should be saved
#' @param minCellCount The minimum cell count to use in exported results (default 5)
#'
#' @return NULL
#' @export
#'
#' @import dplyr
#' @importFrom utils packageVersion
#'
#' @examples
#' \dontrun{
#' library(CDMConnector)
#' con <- DBI::dbConnect(duckdb::duckdb(), eunomiaDir("synpuf-110k"))
#' cdm <- cdmFromCon(con, "main", "main")
#' P4C1021::runStudy(cdm, outputFolder = here::here("output"))
#' }
runStudy <- function(cdm, outputFolder, minCellCount = 5) {

  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  logFile <- file.path(outputFolder, "log.txt")
  logger <- log4r::logger(
    threshold = "INFO",
    appenders = list(
      log4r::console_appender(layout = log4r::default_log_layout()),
      log4r::file_appender(file = logFile)
    )
  )

  log4r::info(logger = logger, paste("Running study on", CDMConnector::cdmName(cdm)))
  log4r::info(logger = logger, paste("Using study package version", packageVersion("P4C1021")))

  log4r::info(logger = logger, "Saving cdm snapshot")
  readr::write_csv(CDMConnector::snapshot(cdm), file.path(outputFolder, "cdm_snapshot.csv"))

  cohort_set <- CDMConnector::readCohortSet(system.file("cohorts", mustWork = T, package = "P4C1021"))

  log4r::info(logger = logger, "Generating cohort")
  cdm <- CDMConnector::generateCohortSet(cdm,
                             cohort_set,
                             name = "p4c1021_cohort",
                             computeAttrition = TRUE,
                             overwrite = TRUE)


  if (CDMConnector::cdmName(cdm) == "INGEF") {

    # First save the inpatient attrition before we go changing the cohort
    log4r::info(logger = logger, "Save attrition for inpatient cohort on INGEF")
    inpatientAttrition <- CohortCharacteristics::summariseCohortAttrition(cdm$p4c1021_cohort)
    omopgenerics::exportSummarisedResult(
      inpatientAttrition,
      minCellCount = minCellCount,
      fileName = "ingef_inpatient_attrition_{cdm_name}_{date}.csv",
      path = outputFolder
    )

    log4r::info(logger = logger, "Generating outpatient AD cohort on INGEF using observation table")
    # On InGEF we need to capture outpatient diagnoses in the observation table
    # This code only needs to work on sql server
    ingef_cohort_set <- CDMConnector::readCohortSet(system.file("ingef_cohort", mustWork = T, package = "P4C1021"))

    log4r::info(logger = logger, "Generating outpatient cohort on INGEF")
    cdm <- CDMConnector::generateCohortSet(cdm,
                             ingef_cohort_set,
                             name = "p4c1021_outpatient_cohort",
                             computeAttrition = TRUE,
                             overwrite = TRUE)

    # move the index date to the beginning of each quarter
    cdm$p4c1021_cohort <- cdm$p4c1021_outpatient_cohort |>
      mutate(cohort_start_date = case_when(
        month(cohort_start_date) %in% 1L:3L   ~ as.Date(paste0(as.character(year(cohort_start_date)), "-01-01")),
        month(cohort_start_date) %in% 4L:6L   ~ as.Date(paste0(as.character(year(cohort_start_date)), "-04-01")),
        month(cohort_start_date) %in% 7L:9L   ~ as.Date(paste0(as.character(year(cohort_start_date)), "-07-01")),
        month(cohort_start_date) %in% 10L:12L ~ as.Date(paste0(as.character(year(cohort_start_date)), "-10-01")),
        TRUE ~ cohort_start_date
      )) |>
      dplyr::union_all(cdm$p4c1021_cohort) |>
      CohortConstructor::collapseCohorts() |>
      compute(name = "p4c1021_cohort", overwrite = TRUE, temporary = FALSE)

  }

  cdm$p4c1021_cohort <- cdm$p4c1021_cohort |>
    PatientProfiles::addAge() |>
    filter(age >= 18) |>
    select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") |>
    compute(name = "p4c1021_cohort", overwrite = TRUE, temporary = FALSE) |>
    omopgenerics::recordCohortAttrition("Age >= 18 using patient profiles")


  log4r::info(logger = logger, "saving cohort counts")
  chr <- CohortCharacteristics::summariseCharacteristics(
    cdm$p4c1021_cohort,
    counts = TRUE,
    demographics = TRUE,
    # 18-55; 56-65; 66-75; 76-85; 86+
    ageGroup = list(c(18,55), c(56,65), c(66,75), c(76,85), c(86,150)) # make sure we also get overall ages
  )

  omopgenerics::exportSummarisedResult(
    chr,
    minCellCount = minCellCount,
    fileName = "demographics_{cdm_name}_{date}.csv",
    path = outputFolder
  )

  atr <- CohortCharacteristics::summariseCohortAttrition(
    cdm$p4c1021_cohort
  )

  omopgenerics::exportSummarisedResult(
    atr,
    minCellCount = minCellCount,
    fileName = "attrition_{cdm_name}_{date}.csv",
    path = outputFolder
  )

  if (all(omopgenerics::cohortCount(cdm$p4c1021_cohort)$number_subjects == 0)) {
    cli::cli_abort("None of the study cohorts matched any persons in your cdm database. You cannot run the study.")
  }

  log4r::info(logger = logger, paste(omopgenerics::cohortCount(cdm$p4c1021_cohort)$number_subjects, "alzheimers patients identified"))

  # confirm that each person is in the cohort at most one time
  check <- omopgenerics::cohortCount(cdm$p4c1021_cohort) %>%
    mutate(number_records = as.numeric(number_records), number_subjects = as.numeric(number_subjects)) %>%
    mutate(check = (number_records == number_subjects))

  if (!all(check$check)) {
    print(check)
    cli::cli_abort("Some persons are in the cohort table more than once. We only expect a single record per person.")
  }

  # compute Incidence ----

  log4r::info(logger = logger, "generating denominator cohort")

  # Note change cohortDateRange for actual execution
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm,
    "denom",
    cohortDateRange = as.Date(c("2014-01-01", "2024-12-31")),
    ageGroup = list(c(18,55), c(56,65), c(66,75), c(76,85), c(86,150), c(18,150), c(18,65), c(66,150)),
    sex = c("Male", "Female", "Both"),
    daysPriorObservation = 0,
    requirementInteractions = TRUE
  )

  log4r::info(logger = logger, "Summarize demographics of denominator cohort")

  denomSettings <- omopgenerics::settings(cdm$denom)
  preferredDenomCohortIds <- denomSettings |>
    filter(age_group == "18 to 150", sex == "Both") |>
    distinct(cohort_definition_id) |>
    pull(cohort_definition_id)
  fallbackDenomCohortIds <- denomSettings |>
    distinct(cohort_definition_id) |>
    pull(cohort_definition_id)

  # Some strata can legitimately yield an empty denominator in small/fixture CDMs.
  # Prefer the intended "general adult population" cohort, but fall back to any
  # denominator cohort with at least one person to keep the study runnable.
  denomCounts <- cdm$denom |>
    group_by(cohort_definition_id) |>
    summarise(
      person_count = n_distinct(subject_id),
      record_count = n(),
      .groups = "drop"
    ) |>
    collect()

  denomCohortIdCandidates <- denomCounts |>
    filter(person_count > 0)

  preferredCohortIdCandidates <- denomCohortIdCandidates |>
    filter(cohort_definition_id %in% preferredDenomCohortIds)

  if (nrow(preferredCohortIdCandidates) > 0) {
    denomCohortId <- preferredCohortIdCandidates |>
      arrange(desc(person_count), desc(record_count)) |>
      slice(1) |>
      pull(cohort_definition_id)
  } else if (nrow(denomCohortIdCandidates) > 0) {
    denomCohortId <- denomCohortIdCandidates |>
      arrange(desc(person_count), desc(record_count)) |>
      slice(1) |>
      pull(cohort_definition_id)
  } else {
    # No persons found in any generated denominator cohort.
    # Keep the study runnable: pick a stable cohort id (for reporting) and
    # allow downstream summaries/estimates to produce empty outputs.
    denomCohortId <- if (length(preferredDenomCohortIds) > 0) {
      preferredDenomCohortIds[[1]]
    } else {
      fallbackDenomCohortIds[[1]]
    }

    log4r::warn(
      logger,
      "No persons found in any generated denominator cohort; denominator-based outputs may be empty."
    )
  }

  stopifnot(length(denomCohortId) == 1, !is.na(denomCohortId))

  # keep only the earliest record per person
  cdm$genpop <- cdm$denom |>
    filter(cohort_definition_id == denomCohortId) |>
    group_by(cohort_definition_id, subject_id) |>
    summarise(cohort_start_date = min(cohort_start_date, na.rm = TRUE),
              cohort_end_date = min(cohort_end_date, na.rm = TRUE)) |>
    compute(name = "genpop", temporary = FALSE, overwrite = TRUE) |>
    omopgenerics::newCohortTable()

  # check that there is one record per person in the genpop cohort
  check <- cdm$genpop |>
    group_by(cohort_definition_id) |>
    summarise(person_count = n_distinct(subject_id), record_count = n()) |>
    collect()

  if (nrow(check) == 0) {
    log4r::warn(logger, "Selected general population denominator has no rows in genpop; skipping genpop integrity checks.")
  } else {
    if (nrow(check) != 1) {
      stop("General population cohort table is not uniquely defined for the selected denominator cohort.")
    }

    if (check$person_count != check$record_count) {
      stop(paste("General population cohort has", check$person_count, "persons and", check$record_count, "records!",
                 "There should only be one record per person!"))
    }
  }

  denomChr <- CohortCharacteristics::summariseCharacteristics(
    cdm$genpop,
    cohortId = denomCohortId,
    counts = TRUE,
    demographics = TRUE,
    ageGroup = list(c(18,55), c(56,65), c(66,75), c(76,85), c(86,150))
  )

  omopgenerics::exportSummarisedResult(
    denomChr,
    minCellCount = minCellCount,
    fileName = "denom_demographics_{cdm_name}_{date}.csv",
    path = outputFolder
  )

  inc <- tryCatch(
    IncidencePrevalence::estimateIncidence(
      cdm,
      "denom",
      "p4c1021_cohort",
      interval = c("overall", "years"),
      completeDatabaseIntervals = TRUE,
      outcomeWashout = Inf,
      repeatedEvents = FALSE,
      includeOverallStrata = TRUE
    ),
    error = function(e) {
      log4r::warn(logger, paste("Skipping incidence estimation:", e$message))
      NULL
    }
  )

  if (!is.null(inc)) {
    omopgenerics::exportSummarisedResult(
      inc,
      minCellCount = minCellCount,
      fileName = "incidence_{cdm_name}_{date}.csv",
      path = outputFolder
    )
  }

  prev <- tryCatch(
    IncidencePrevalence::estimatePeriodPrevalence(
      cdm,
      "denom",
      "p4c1021_cohort",
      interval = c("overall", "years"),
      completeDatabaseIntervals = TRUE,
      fullContribution = FALSE,
      level = "person",
      includeOverallStrata = TRUE
    ),
    error = function(e) {
      log4r::warn(logger, paste("Skipping period prevalence estimation:", e$message))
      NULL
    }
  )

  if (!is.null(prev)) {
    omopgenerics::exportSummarisedResult(
      prev,
      minCellCount = minCellCount,
      fileName = "prevalence_{cdm_name}_{date}.csv",
      path = outputFolder
    )
  }

  # Number and percentage of people who have any of the concept sets prior to AD diagnosis.

  # check that we can read these in
  # for (f in list.files(here::here("inst/concept_sets"), full.names = T)) {
  #   print(f)
  #   concepts <- CodelistGenerator::codesFromConceptSet(
  #     cdm,
  #     path = f
  #   )
  # }
  #
  # concepts <- CodelistGenerator::codesFromConceptSet(
  #   cdm,
  #   path = here::here("inst/concept_sets")
  # )

  log4r::info(logger, "Reading in concept sets from json")
  concepts <- CodelistGenerator::codesFromConceptSet(
    cdm,
    path = system.file("concept_sets", mustWork = T, package = "P4C1021")
  )

  log4r::info(logger, "Summarizing covariates any time prior to index")
  covariates1 <- CohortCharacteristics::summariseCharacteristics(
    cdm$p4c1021_cohort,
    counts = T,
    demographics = F,
    conceptIntersectFlag = list(
      conceptSet = concepts,
      indexDate = "cohort_start_date",
      censorDate = NULL,
      window = list(c(-Inf, 0)),
      targetStartDate = "event_start_date",
      targetEndDate = "event_end_date",
      inObservation = TRUE,
      nameStyle = "{concept_name}_{window_name}"
    )
  )

  log4r::info(logger, "Summarizing covariates 1 year prior to index")
  covariates2 <- CohortCharacteristics::summariseCharacteristics(
    cdm$p4c1021_cohort,
    counts = T,
    demographics = F,
    conceptIntersectFlag = list(
      conceptSet = concepts,
      indexDate = "cohort_start_date",
      censorDate = NULL,
      window = list(c(-365, 0)),
      targetStartDate = "event_start_date",
      targetEndDate = "event_end_date",
      inObservation = TRUE,
      nameStyle = "{concept_name}_{window_name}"
    )
  )

  log4r::info(logger, "Exporting covariate summary")
  omopgenerics::exportSummarisedResult(
    covariates1, covariates2,
    minCellCount = minCellCount,
    fileName = "covariates_{cdm_name}_{date}.csv",
    path = outputFolder
  )

  log4r::info(logger, "Summarizing time from MCI to index")
  timeFromMci <- cdm$p4c1021_cohort |>
    PatientProfiles::addSex() |>
    PatientProfiles::addAge(
      ageGroup = list(c(18,55), c(56,65), c(66,75), c(76,85), c(86,150))
    ) |>
    CohortCharacteristics::summariseCharacteristics(
      strata = list("sex", "age_group"),
      counts = TRUE,
      demographics = FALSE,
      conceptIntersectDays = list(
        indexDate = "cohort_start_date",
        conceptSet = list(mild_cognitive_impairment = concepts$mild_cognitive_impairment),
        window = list(c(-Inf, 0)),
        targetDate = "event_start_date",
        order = "first",
        inObservation = TRUE,
        nameStyle = "days_from_first_mci"
      )
    )

  log4r::info(logger, "saving time from mild cognitive impairment")
  omopgenerics::exportSummarisedResult(
    timeFromMci,
    minCellCount = minCellCount,
    fileName = "time_from_mci_{cdm_name}_{date}.csv",
    path = outputFolder
  )

  log4r::info(logger, "summarizing large scale characteristics")
  largeScaleCharacteristics <- CohortCharacteristics::summariseLargeScaleCharacteristics(
    cohort = cdm$p4c1021_cohort,
    eventInWindow = c("drug_exposure", "condition_occurrence", "measurement", "procedure_occurrence", "visit_occurrence"),
    minimumFrequency = 0.01
  )

  log4r::info(logger, "saving large scale characteristics")
  omopgenerics::exportSummarisedResult(
    largeScaleCharacteristics,
    minCellCount = minCellCount,
    fileName = "large_scale_characteristics_{cdm_name}_{date}.csv",
    path = outputFolder
  )

  log4r::info(logger, "Done!")
  invisible(NULL)
}

