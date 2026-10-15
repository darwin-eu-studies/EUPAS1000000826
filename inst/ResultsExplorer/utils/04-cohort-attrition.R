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

# Cohort Attrition module -----
library(shiny)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(gt)
library(gtExtras)
library(shinyWidgets)
library(DiagrammeR)

# ---- UI ----
cohortAttritionUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      h3("Cohort Attrition"),
      fluidRow(
        column(2, pickerInput(ns("cdm"), "Data source", choices = unique(attrition$cdm_name), multiple = F)),
        column(2, pickerInput(ns("cohort"), "Cohort name", choices = unique(attrition$group_level), multiple = F)),
      ),
      tabsetPanel(
        id = ns("tabsetPanel"),
        type = "tabs",
        tabPanel(
          "Table",
          gt_output(ns("table")),
          h4("Download table"),
          downloadButton(ns("download_table"), "Download table (.docx)")
        ),
        tabPanel(
          "Figure",
          uiOutput(ns("attr_diagram"), height = "600px") %>% withSpinner()
        )
      )
    )
  )
}

# ---- SERVER ----
cohortAttritionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filtered_data <- reactive({
      req(input$cdm, input$cohort)
      attrition %>%
        dplyr::filter(cdm_name == input$cdm) %>%
        dplyr::filter(group_level == input$cohort) 
    })
    
    table_gt <- reactive({
      filtered_data() |> 
        mutate(variable_name = stringr::str_replace_all(variable_name, "_", " ") %>%
                 stringr::str_to_title()) |> 
      CohortCharacteristics::tableCohortAttrition(
        type = "gt",
        header = "variable_name",
        groupColumn = c("cdm_name", "cohort_name"),
        hide = c("variable_level", "reason_id", "estimate_name", "table_name", "cohort_definition_id"),
        .options = list()
      ) |> gt_style_darwin()
    })
    
    output$table <- render_gt({
      table_gt()
    })
    
    # Download table as docx
    output$download_table <- downloadHandler(
      filename = function() {
        "cohortAttritionTable.docx"
      },
      content = function(file) {
        gtsave(table_gt(), file)
      }
    )
    
    output$attr_diagram <- renderUI({
      filtered_data() |> 
        CohortCharacteristics::plotCohortAttrition(type = "html")
    })
  })
}

# ---- RUN ----
# shinyApp(
#   ui = fluidPage(cohortAttritionUI("cohortAttrition")),
#   server = function(input, output, session) {
#     cohortAttritionServer("cohortAttrition")
#   }
# )
