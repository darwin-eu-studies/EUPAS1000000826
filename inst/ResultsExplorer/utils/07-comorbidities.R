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
library(dplyr)
library(gt)

diagnosticProcedures <- c(
"Brain MRI" = "Mri",
"Brain PET-F18" = "Brain pet"
)

prespecifiedClinicalFeatures <- c(
  "Neuropsychiatric symptoms" = "Neuropsychiatric symptoms",
  "Alterations in activities of daily living" = "Alterations in activities of daily living",
  "Caregiver support" = "Caregiver support",
  "Daily activities limitation" = "Daily activities limitation",
  "MCI before AD diagnosis" = "Mild cognitive impairment"
)

alzheimersDiseaseDrugs <- c(
  "Memantine" = "Memantine",
  "Donepezil" = "Donepezil",
  "Rivastigmine"  = "Rivastigmine",
  "Galantamine" = "Galantamin" # possible spelling error
)

medicationsForComorbidities <- c(
  "Antiplatelets"  = "Antiplatelets stroke myocardial infarction atrial fibrillation heart failure" ,
  "Anticoagulants/Antithrombotics" = "Anticoagulants antithromb stroke myocardial infarction atrial fibrillation heart failure",
  "Oral glucose-lowering drugs" = "Oral glucose lowering drugs",
  "Insulin" = "Insulin",
  "Antihypertensives" = "Antihypertensives hypertension",
  "Antiarrhythmics/rhythm control drugs" = "Rhythm control drugs stroke myocardial infarction atrial fibrillation heart failure",
  "Lipid lowering drugs" = "Lipid lowering drugs hypercholesterolemia hypertriglyceridemia",
  "Heart failure treatment" = "Heart failure treatment stroke myocardial infarction atrial fibirllation heart failure"
)

conditions <- c(
  "Down's syndrome" = "Downs syndrome",
  "Stroke" = "Stroke",
  "Atrial fibrillation" = "Atrial fibrillation",
  "Myocardial infarction" = "Myocardial infarction",
  "Heart failure" = "Heart failure",
  "Hypertension" = "Hypertension",
  "Diabetes"  = "Diabetes",
  "Hypercholesterolemia" = "Hypercholesterolemia",
  "Hypertriglyceridemia" = "Hypertriglyceridemia"
)

other <- c(
  "Alzheimer s disease" = "Alzheimer s disease",
  "Diabetes drugs"  =  "Diabetes drugs",
  "Acetylcholinesterase inhibitors" = "Acetylcholinesterase inhibitors",
  "Antipsychotics" = "Antipsychotics",
  "Metabolic syndrome", "Metabolic syndrome",
  "Heart failure treatment" = "Heart failure treatment stroke myocardial infarction atrial fibirllation heart failure",
  "Alzheimer disease" =   "Alzheimer disease"
)


allComorbidities <- c(
  diagnosticProcedures,
  prespecifiedClinicalFeatures,
  alzheimersDiseaseDrugs,
  medicationsForComorbidities,
  conditions,
  other
)

# ---- UI ----
comorbiditiesUI <- function(id, title) {
  ns <- NS(id)
  fluidPage(
    h3(title),
    selectInput(ns("time_window"), "Time window", choices = c("All time prior" = "Concepts flag -inf to 0", "One year prior" = "Concepts flag -365 to 0")),
    gt::gt_output(ns("table")),
    downloadButton(ns("download_table"), "Download table (.docx)")
  )
}

# ---- SERVER ----

# tabname can be one of : "comorbidities", "diagnostic_procedures", "clinical_profile"
comorbiditiesServer <- function(id, tabName = "comorbidities") {
  moduleServer(id, function(input, output, session) {

    table_gt <- reactive({
      comorbidities |>
        filter(variable_name == input$time_window) |>
        mutate(variable_level = factor(
          variable_level,
          levels = allComorbidities,
          labels = names(allComorbidities)
        )) |>
        arrange(
          result_id,
          cdm_name,
          group_name,
          group_level,
          strata_name, strata_level, variable_name, variable_level
        ) |>
        mutate(variable_name = case_when(
          variable_level %in% names(diagnosticProcedures) ~ stringr::str_replace(variable_name, "Concepts flag", "Diagnostic Procedures"),
          variable_level %in% names(prespecifiedClinicalFeatures) ~ stringr::str_replace(variable_name, "Concepts flag", "Prespecified Clinical Features"),
          variable_level %in% names(alzheimersDiseaseDrugs) ~ stringr::str_replace(variable_name, "Concepts flag", "Alzheimers Disease Drugs"),
          variable_level %in% names(medicationsForComorbidities) ~ stringr::str_replace(variable_name, "Concepts flag", "Medications for Comorbidities"),
          variable_level %in% names(conditions) ~ stringr::str_replace(variable_name, "Concepts flag", "Conditions"),
          TRUE ~ variable_name
        )) %>%
        {
          if (tabName == "diagnostic_procedures") {
            filter(., stringr::str_detect(variable_name, "Diagnostic Procedures"))
          } else if (tabName == "comorbidities") {
            filter(., stringr::str_detect(variable_name, "Conditions"))
          } else if (tabName == "clinical_profile")  {
            filter(., stringr::str_detect(variable_name, "Prespecified Clinical Features|Alzheimers Disease Drugs|Medications for Comorbidities"))
          }
        } %>%
        mutate(variable_name = stringr::str_remove_all(variable_name, " -inf to 0| -365 to 0")) |> 
        filter(!is.na(variable_level)) |>
        mutate(cdm_name = factor(cdm_name, levels = c("NAJS", "DK-DHR", "INGEF RDB", "IQVIA DA Germany", "IPCI", "CPRD GOLD"))) |> 
        arrange(cdm_name) |> 
        visOmopResults::visOmopTable(
          estimateName = c("N (%)" = "<count> (<percentage>)"),
          header = "cdm_name",
          hide = c("cohort_name")
        ) |> 
        gt_style_darwin() |> 
        rename_spanner()
    })

    output$table <- gt::render_gt({
      table_gt()
    })

    # Download table as docx
    output$download_table <- downloadHandler(
      filename = function() {
        "comorbiditiesTable.docx"
      },
      content = function(file) {
        gtsave(table_gt(), file)
      }
    )

  })
}

# ---- RUN ----
# shinyApp(
#   ui = fluidPage(comorbiditiesUI("comorbidities")),
#   server = function(input, output, session) {
#     comorbiditiesServer("comorbidities")
#   }
# )
