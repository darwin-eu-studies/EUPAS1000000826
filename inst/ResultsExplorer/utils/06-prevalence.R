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

# Prevalence module
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(IncidencePrevalence)
library(ggplot2)
library(shinycssloaders)
library(gt)
library(gtExtras)
library(shinyWidgets)


# ---- UI ----
# picker options
opt <- list("actions-box" = TRUE, size = 10, "selected-text-format" = "count > 3")

prevalenceYearChoices <- visOmopResults::splitAdditional(prevalence) |>
  distinct(prevalence_start_date) |>
  pull()
prevalenceYearChoices <- prevalenceYearChoices[prevalenceYearChoices != "overall"]
prevalenceYearChoices <- setNames(prevalenceYearChoices, stringr::str_extract(prevalenceYearChoices, "^[0-9]+"))

prevalenceAgeGroupChoices <- setNames(c("18 to 55","18 to 65","56 to 65","66 to 150","66 to 75","76 to 85","86 to 150","18 to 150"),
                                      c("18 to 55","18 to 65","56 to 65","\u2265 66","66 to 75","76 to 85","\u2265 86","\u2265 18"))



prevalenceUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      h3("Prevalence estimates"),
      p("Prevalence estimates are shown below, please select configuration to filter them:"),
      fluidRow(
        column(2, pickerInput(ns("cdm"), "Data source", choices = unique(prevalence$cdm_name), selected = unique(prevalence$cdm_name), multiple = T, options = opt)),
        column(2, pickerInput(ns("age_group"), "Age group", choices = prevalenceAgeGroupChoices, selected = unique(settings(prevalence)$denominator_age_group), multiple = T, options = opt)),
        column(2, pickerInput(ns( "sex"), "Sex", choices = unique(settings(prevalence)$denominator_sex), selected = "Both", multiple = T, options = opt)),
        column(2, pickerInput(ns("interval"), "Interval", choices = c("years", "overall"), selected = "years", multiple = F, options = opt)),
        column(2, pickerInput(ns( "years"), "Years", choices = prevalenceYearChoices, selected = prevalenceYearChoices, multiple = T, options = opt))
      ),
      tabsetPanel(
        id = ns("tabsetPanel"),
        type = "tabs",
        tabPanel(
          "Table of estimates",
          gt::gt_output(ns("report_table")) |> withSpinner(type = 6),
          h4("Download table"),
          downloadButton(ns("download_table"), "Download table (.docx)")
        ),
        tabPanel(
          "Plot of estimates",
          p("Plotting options"),
          fluidRow(
            column(3, selectInput(ns("plot_facet"), "Facet by",
              choices = c("cdm_name", "denominator_age_group", "denominator_sex", "None")
            )),
            column(3, selectInput(ns("plot_colour"), "Colour by",
              choices = c("denominator_age_group", "cdm_name", "denominator_sex", "None")
            )),
            column(3, selectInput(ns("plot_ribbon"), "Ribbon", choices = c(FALSE, TRUE)))
          ),
          plotlyOutput(ns("prevalence_plot"), height = "600px") %>% withSpinner(),
          h4("Download figure"),
          fluidRow(
            column(2, textInput(ns("download_height"), "Height (cm)", 10, width = "100px")),
            column(2, textInput(ns("download_width"), "Width (cm)", 20, width = "100px")),
            column(3, textInput(ns("download_dpi"), "Resolution (dpi)", 300, width = "100px")),
          ),
          downloadButton(ns("download_plot"), "Download plot")
        ),
        tabPanel(
          "Attrition",
          gt::gt_output(ns("attrition_table")) |> withSpinner(type = 6),
          h4("Download attrition table"),
          downloadButton(ns("download_attrition_table"), "Download attrition table (.docx)")
        )
      )
    )
  )
}

# ---- SERVER ----
prevalenceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filtered_data <- reactive({
      req(input$cdm, input$age_group, input$sex)
      prevalence %>%
        filter(cdm_name %in% input$cdm) |> 
        visOmopResults::filterSettings(
          denominator_age_group %in% input$age_group,
          denominator_sex %in% input$sex
        )  %>%
        {if (input$interval == "years") {
          visOmopResults::filterAdditional(.,
                                           prevalence_start_date %in% input$years,
                                           analysis_interval %in% input$interval)
        } else {
          visOmopResults::filterAdditional(., analysis_interval %in% input$interval)
        }}
    })

    # Table
    table_gt <- reactive({
      filtered_data() |>
        mutate(estimate_value = case_when(
          estimate_name %in% c("prevalence", "prevalence_95CI_upper", "prevalence_95CI_lower") ~ as.character(100*as.numeric(estimate_value)),
          T ~ estimate_value
        )) |> 
        IncidencePrevalence::tablePrevalence(
          type = "gt",
          header = c("estimate_name"),
          groupColumn = c("cdm_name", "outcome_cohort_name"),
          settingsColumn = c("denominator_age_group", "denominator_sex"),
          hide = c("denominator_cohort_name")
        ) |> gt_style_darwin()
    })

    output$report_table <- gt::render_gt({
      table_gt() 
    })

    # Download table as docx
    output$download_table <- downloadHandler(
      filename = function() {
        "prevalenceEstimatesTable.docx"
      },
      content = function(file) {
        gt_tbl <- table_gt()
        gtsave(gt_tbl, file)
      }
    )

    # Attrition Table
    attrition_table_gt <- reactive({
      req(input$cdm, input$age_group, input$sex)
      prevalence %>%
        filter(cdm_name %in% input$cdm) |> 
        visOmopResults::filterSettings(
          denominator_age_group %in% input$age_group,
          denominator_sex %in% input$sex
        )  %>%
        mutate(cdm_name = factor(cdm_name, levels = c("NAJS", "DK-DHR", "INGEF RDB", "IQVIA DA Germany", "IPCI", "CPRD GOLD"))) |> 
        arrange(cdm_name) |> 
        IncidencePrevalence::tablePrevalenceAttrition(
          type = "gt",
          header = c("variable_name"),
          groupColumn = c("cdm_name"),
          settingsColumn = c("denominator_age_group", "denominator_sex"),
          hide = c("denominator_cohort_name", "estimate_name", "reason_id", "variable_level")
        )
    })

    output$attrition_table <- gt::render_gt({
      attrition_table_gt() |> gt_style_darwin()
    })

    output$download_attrition_table <- downloadHandler(
      filename = function() {
        "prevalenceAttritionTable.docx"
      },
      content = function(file) {
        gt_tbl <- attrition_table_gt()
        gtsave(gt_tbl, file)
      }
    )

    # Plot
    plot_prevalence <- reactive({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No results for selected inputs"))

      attr(df, "settings") <- settings(df) |>
        mutate(denominator_age_group = factor(denominator_age_group,
                                              levels = c("18 to 55","18 to 65","56 to 65","66 to 150","66 to 75","76 to 85","86 to 150","18 to 150"),
                                              labels = c("18 to 55","18 to 65","56 to 65","\u2265 66",    "66 to 75","76 to 85","\u2265 86",    "\u2265 18")))
      
      df |>
        IncidencePrevalence::plotPrevalence(
          line = TRUE,
          point = TRUE,
          ribbon = as.logical(input$plot_ribbon),
          facet = if (input$plot_facet == "None") NULL else input$plot_facet,
          colour = if (input$plot_colour == "None") NULL else input$plot_colour
        ) +
        ggtitle("Prevalence estimates") +
        geom_line() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        guides(fill = "none")
    })

    output$prevalence_plot <- renderPlotly({
      if (input$interval == "years") {
        ggplotly(plot_prevalence())
      } else {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No plot available when interval = 'overall'", size = 6) +
          theme_void()
        ggplotly(p)
      }
    })

    # Download
    output$download_plot <- downloadHandler(
      filename = function() "prevalenceEstimatesPlot.png",
      content = function(file) {
        ggsave(
          filename = file,
          plot = plot_prevalence(),
          width = as.numeric(input$download_width),
          height = as.numeric(input$download_height),
          dpi = as.numeric(input$download_dpi),
          units = "cm"
        )
      }
    )
  })
}

# ---- RUN ----
# shinyApp(
#   ui = fluidPage(prevalenceUI("prevalence")),
#   server = function(input, output, session) {
#     prevalenceServer("prevalence")
#   }
# )
