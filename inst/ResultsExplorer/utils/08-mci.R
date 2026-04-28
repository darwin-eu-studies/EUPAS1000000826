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

opt <- list("actions-box" = TRUE, size = 10, "selected-text-format" = "count > 3")

# ---- UI ----
mciUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Time from first MCI to first AD diagnosis"),
    fluidRow(
      column(2, pickerInput(ns("strata"), "Stratify by:", choices = c("Overall" = "overall", "Age Group"= "age_group", "Sex" = "sex"), selected = "overall", multiple = T, options = opt))
    ),
    gt::gt_output(ns("table")) |> shinycssloaders::withSpinner(type = 6),
    downloadButton(ns("download_table"), "Download table (.docx)")
  )
}

# ---- SERVER ----
mciServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    table_gt <- reactive({
      # these values are recorded as negative so we need to flip them
      comorbidities |> 
        filter(stringr::str_detect(variable_level, "Mild cognitive impairment")) |> 
        filter(stringr::str_detect(variable_name, "Concepts flag -inf to 0")) |> 
        omopgenerics::bind(timeFromMci) |> 
        filter(!(strata_name == "age_group" & strata_level == "None")) |> 
        mutate(estimate_value = stringr::str_remove_all(estimate_value, "-")) |> 
        filter(!variable_name %in% c("Number records", "Number subjects")) |> 
        filter(strata_name %in% input$strata) |> 
        mutate(cdm_name = factor(cdm_name, levels = c("NAJS", "DK-DHR", "INGEF RDB", "IQVIA DA Germany", "IPCI", "CPRD GOLD"))) |> 
        arrange(cdm_name) |> 
        visOmopResults::visOmopTable(
          estimateName = c(
            "MCI, N (%)" = "<count> (<percentage>)",
            "Duration in days from MCI occurrence to AD diagnosis (min-max)" = "<max> - <min>",
            "Duration in days from MCI occurrence to AD diagnosis (median, p25-p75)" = "<median>, <q75> - <q25>",
            "Duration in days from MCI occurrence to AD diagnosis (mean, SD)" = "<mean>, <sd>"),
          header = "cdm_name",
          hide = c("cohort_name", "variable_name", "variable_level")
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
        "MCI-table.docx"
      },
      content = function(file) {
        gtsave(table_gt(), file)
      }
    )
    
  })
}

# ---- RUN ----
# shinyApp(
#   ui = fluidPage(mciUI("mci")),
#   server = function(input, output, session) {
#     mciServer("mci")
#   }
# )
