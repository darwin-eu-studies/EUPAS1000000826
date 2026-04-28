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

library(shiny)
library(shinydashboard)
library(dplyr)
library(bslib)

if (exists("shinySettings", where = .GlobalEnv)) {
  if (!file.exists(shinySettings$dataFolder)) stop("Could not find results data!")
  dataFolder <- shinySettings$dataFolder
  useCachedData <- shinySettings$useCachedData
} else {
  dataFolder <- "data"
  useCachedData <- TRUE
}

if (!file.exists("data.rds") || isFALSE(useCachedData)) {
  readResults <- function(regex) {
    allFiles <- list.files(dataFolder, recursive = T, full.names = T)
    stringr::str_subset(allFiles, regex) |>
      purrr::map(omopgenerics::importSummarisedResult) |>
      purrr::reduce(omopgenerics::bind) %>%
      {
        if ("cdm_name" %in% colnames(.)) {
          mutate(., cdm_name = case_when(
            cdm_name == "INGEF" ~ "INGEF RDB",
            cdm_name == "CPRD_GOLD" ~ "CPRD GOLD",
            cdm_name == "IQVIA-DA" ~ "IQVIA DA Germany",
            T ~ cdm_name
          ))
        }
      }
      # filter out unknown age groups (persons who were considered 18 by Atlas but 17 by patient profiles)
      # filter(!(variable_name == "Age group" & variable_level == "Unknown")) |> 
      # filter(!(strata_name == "age_group" & strata_level == "None")) 
  }

  demographics <- readResults("demographics")
  attrition <- readResults("attrition")
  incidence <- readResults("incidence")
  prevalence <- readResults("prevalence")
  comorbidities <- readResults("covariates")
  timeFromMci <- readResults("time_from_mci")
  largeScaleCharacteristics <- readResults("large_scale_characteristics")

  dbinfo <- list.files(dataFolder, recursive = T, full.names = T, pattern = "cdm_snapshot") |>
    purrr::map(readr::read_csv) |>
    purrr::map(~dplyr::mutate_all(., as.character)) |>
    purrr::reduce(dplyr::bind_rows)

  data <- list(
    demographics = demographics,
    attrition = attrition,
    incidence = incidence,
    prevalence = prevalence,
    comorbidities = comorbidities,
    timeFromMci = timeFromMci,
    largeScaleCharacteristics = largeScaleCharacteristics,
    dbinfo = dbinfo
  )
  readr::write_rds(data, "data.rds")
} else {
  data <- readr::read_rds("data.rds")
  purrr::walk(names(data), ~assign(., data[[.]], envir = .GlobalEnv))
}

# -	The study period for NAJS starts from 01-01-2018
# -	The study period for InGef starts from 01-01-2016 ends Dec 31 2023

incidence <- incidence |> 
  filter(!(cdm_name == 'NAJS' & stringr::str_detect(additional_level, "years") & stringr::str_detect(additional_level, paste0(2014:2017, collapse = "|")))) |> 
  filter(!(cdm_name == 'INGEF RDB' & stringr::str_detect(additional_level, "years") & stringr::str_detect(additional_level, paste0(c(2014:2015, 2024), collapse = "|")))) |> 
  filter(!(cdm_name == 'DK-DHR' & stringr::str_detect(additional_level, "years") & stringr::str_detect(additional_level, paste0(2000:2013, collapse = "|")))) 

prevalence <- prevalence |> 
  filter(!(cdm_name == 'NAJS' & stringr::str_detect(additional_level, "years") & stringr::str_detect(additional_level, paste0(2014:2017, collapse = "|")))) |> 
  filter(!(cdm_name == 'INGEF RDB' & stringr::str_detect(additional_level, "years") & stringr::str_detect(additional_level, paste0(c(2014:2015, 2024), collapse = "|")))) |> 
  filter(!(cdm_name == 'DK-DHR' & stringr::str_detect(additional_level, "years") & stringr::str_detect(additional_level, paste0(2000:2013, collapse = "|")))) 

# only use data pre index date for lsc
largeScaleCharacteristics <- largeScaleCharacteristics |> 
  # distinct(variable_level) |> 
  filter(variable_level %in% c("-inf to -366", "-365 to -31", "-30 to -1", "0 to 0"))

invisible(lapply(list.files("utils", full.names = TRUE), source))


