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

# Module: Demographics Tab ----

library(shiny)
library(bslib)
library(dplyr)
library(shinyWidgets)

# requires demographics summarisedResult
opt <- list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")

demographicsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(2, 
        pickerInput(ns("cdm_filter"), "Data source", 
                    choices = unique(demographics$cdm_name), 
                    selected = unique(demographics$cdm_name), 
                    multiple = T, 
                    options = opt))
    ),
    fluidRow(
      div(
        style = "overflow-x: auto;",
        gt::gt_output(ns("table")) |> shinycssloaders::withSpinner(type = 6)
      )
    ),
    fluidRow(
      column(2, downloadButton(ns("download"), "Download Table")
      )
    )
  )
}

# need to recompute person counts in the demographics table.
recomputeTotals <- function(df) {
  totals <- df |> 
    filter(variable_name == "Age group", estimate_name == "count") |> 
    transmute(cdm_name, group_level, n = as.numeric(estimate_value)) |> 
    group_by(cdm_name, group_level) |> 
    summarise(n = sum(n), .groups = "drop") 
  
  df |> 
    left_join(totals, by = c("cdm_name", "group_level")) |> 
    mutate(estimate_value = ifelse(variable_name == "Number subjects", as.character(n), estimate_value)) |> 
    select(-"n")
    
}

demographicsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- reactive({
      req(length(input$cdm_filter) > 0)
      demographics |> 
        filter(.data$cdm_name %in% input$cdm_filter)
    })

    demographics_table <- reactive({
      filtered_data() |>
        mutate(group_level = case_when(
          stringr::str_detect(group_level, "denominator") ~ "General adult population",
          stringr::str_detect(group_level, "alzheimer") ~ "Individuals with Alzheimer's disease",
          T ~ "Error!"
        )) |>
        mutate(group_level = factor(group_level, levels = c("General adult population", "Individuals with Alzheimer's disease"))) |> 
        filter(!variable_name %in% c(
          "Number records",
          "Cohort start date",
          "Days in cohort",
          "Prior observation",
          "Future observation",
          "Cohort end date"
        )) |>
        mutate(estimate_type = ifelse(variable_name == "Age" & estimate_name %in% c("q25", "median", "q75"), "integer", estimate_type)) |>  # EMA request
        filter(!(estimate_name %in% c("min", "max"))) |> # EMA request
        # recomputeTotals() |> 
        mutate(cdm_name = factor(cdm_name, levels = c("NAJS", "DK-DHR", "INGEF RDB", "IQVIA DA Germany", "IPCI", "CPRD GOLD"))) |> 
        mutate(variable_name = factor(variable_name, levels = c("Number subjects", "Age", "Age group", "Sex"))) |> 
        arrange(cdm_name, group_level, variable_name) |> 
        visOmopResults::visOmopTable(
          estimateName = c(
            "N(%)" = "<count> (<percentage>%)",
            "N" = "<count>",
            "mean (sd)" = "<mean> (<sd>)"
          ),
          header = "cdm_name"
        ) |> 
        gt_style_darwin() |> 
        rename_spanner()
    })

    output$table <- gt::render_gt({
      demographics_table()
    })

    output$download <- downloadHandler(
      filename = "demographics_table.docx",
      content = function(file) {
        gt::gtsave(demographics_table(), file)
      }
    )
  })
}

# shinyApp(
#   ui = fluidPage(demographicsUI("demographics")),
#   server = function(input, output, session) {
#     demographicsServer("demographics")
#   }
# )
