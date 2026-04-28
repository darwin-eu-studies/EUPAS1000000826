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

# Incidence module ----
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
opt <- list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")

# settings(incidence)

incidenceYearChoices <- visOmopResults::splitAdditional(incidence) |>
  distinct(incidence_start_date) |>
  pull()

incidenceYearChoices <- incidenceYearChoices[incidenceYearChoices != "overall"]
incidenceYearChoices <- setNames(incidenceYearChoices, stringr::str_extract(incidenceYearChoices, "^[0-9]+"))

incidenceAgeGroupChoices <- setNames(c("18 to 55","18 to 65","56 to 65","66 to 150","66 to 75","76 to 85","86 to 150","18 to 150"),
                                     c("18 to 55","18 to 65","56 to 65","\u2265 66","66 to 75","76 to 85","\u2265 86","\u2265 18"))
  

incidenceUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      h3("Incidence estimates"),
      p("Incidence estimates are shown below, please select configuration to filter them:"),
      fluidRow(
        column(2, pickerInput(ns("cdm"), "Data source", choices = unique(incidence$cdm_name), selected = unique(incidence$cdm_name), multiple = T, options = opt)),
        column(2, pickerInput(ns("age_group"), "Age group", choices = incidenceAgeGroupChoices, selected = unique(settings(incidence)$denominator_age_group), multiple = T, options = opt)),
        column(2, pickerInput(ns("sex"), "Sex", choices = unique(settings(incidence)$denominator_sex), selected = "Both", multiple = T, options = opt)),
        column(2, pickerInput(ns("interval"), "Interval", choices = c("years", "overall"), selected = "years", multiple = F, options = opt)),
        column(2, pickerInput(ns("years"), "Years", choices = incidenceYearChoices, selected = incidenceYearChoices, multiple = T, options = opt))
      ),
      tabsetPanel(
        id = ns("tabsetPanel"),
        type = "tabs",
        tabPanel(
          "Table of estimates",
          uiOutput(ns("report_table")) |> withSpinner(type = 6),
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
          plotlyOutput(ns("incidence_plot"), height = "600px") %>% withSpinner(type = 6),
          h4("Download figure"),
          fluidRow(
            column(2, textInput(ns("download_height"), "Height (cm)", 10)),
            column(2, textInput(ns("download_width"), "Width (cm)", 20)),
            column(3, textInput(ns("download_dpi"), "Resolution (dpi)", 300))
          ),
          downloadButton(ns("download_plot"), "Download plot")
        ),
        tabPanel(
          "Attrition",
          uiOutput(ns("attrition_table")) |> withSpinner(type = 6),
          h4("Download attrition table"),
          downloadButton(ns("download_attrition_table"), "Download attrition table (.docx)")
        )
      )
    )
  )
}

# ---- SERVER ----
incidenceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filtered_data <- reactive({
      req(input$cdm, input$age_group, input$sex, input$interval)
      incidence %>%
        filter(
          cdm_name %in% input$cdm,
        ) |>
        visOmopResults::filterSettings(
          denominator_age_group %in% input$age_group,
          denominator_sex %in% input$sex
        ) %>%
        {if (input$interval == "years") {
          visOmopResults::filterAdditional(.,
            incidence_start_date %in% input$years,
            analysis_interval %in% input$interval)
        } else {
          visOmopResults::filterAdditional(., analysis_interval %in% input$interval)
        }}


        #|>
        # select(-c(
        #   "incidence_start_date", "incidence_end_date", "reason_id", "analysis_interval",
        #   "denominator_cohort_name", "outcome_cohort_name", "cohort_name", "analysis_type"
        # ))
    })

    # Table
    table_gt <- reactive({
      IncidencePrevalence::tableIncidence(
        filtered_data(),
        type = "gt",
        header = c("estimate_name"),
        groupColumn = c("cdm_name", "outcome_cohort_name"),
        settingsColumn = c("denominator_age_group", "denominator_sex"),
        hide = c("denominator_cohort_name")
      ) |> gt_style_darwin()
    })

    output$report_table <- renderUI({
      table_gt() 
    })

    # Download table as docx
    output$download_table <- downloadHandler(
      filename = function() {
        "incidenceEstimatesTable.docx"
      },
      content = function(file) {
        gt_tbl <- table_gt()
        # gtsave will auto-detect extension and use gt::as_word()
        gtsave(gt_tbl, file)
      }
    )

    # Attrition Table
    attrition_gt <- reactive({
      req(input$cdm, input$age_group, input$sex)
      incidence %>%
        filter(cdm_name %in% input$cdm) |> 
        visOmopResults::filterSettings(
          denominator_age_group %in% input$age_group,
          denominator_sex %in% input$sex
        )  %>%
        mutate(cdm_name = factor(cdm_name, levels = c("NAJS", "DK-DHR", "INGEF RDB", "IQVIA DA Germany", "IPCI", "CPRD GOLD"))) |> 
        arrange(cdm_name) |> 
        tableIncidenceAttrition(
          type = "gt",
          header = c("variable_name"),
          groupColumn = c("cdm_name"),
          settingsColumn = c("denominator_age_group", "denominator_sex", "denominator_days_prior_observation"),
          hide = c("denominator_cohort_name", "estimate_name", "reason_id", "variable_level")
        ) |> gt_style_darwin()
    })

    output$attrition_table <- renderUI({
      attrition_gt()
    })

    output$download_attrition_table <- downloadHandler(
      filename = function() {
        "incidenceAttritionTable.docx"
      },
      content = function(file) {
        gt_tbl <- attrition_gt()
        gtsave(gt_tbl, file)
      }
    )

    # Plot
    plot_incidence <- reactive({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No results for selected inputs"))
      
      attr(df, "settings") <- settings(df) |>
        mutate(denominator_age_group = factor(denominator_age_group,
          levels = c("18 to 55","18 to 65","56 to 65","66 to 150","66 to 75","76 to 85","86 to 150","18 to 150"),
          labels = c("18 to 55","18 to 65","56 to 65","\u2265 66",    "66 to 75","76 to 85","\u2265 86",    "\u2265 18")))

      IncidencePrevalence::plotIncidence(
        df,
        x = "incidence_start_date",
        y = "incidence_100000_pys",
        line = TRUE,
        point = TRUE,
        ribbon = as.logical(input$plot_ribbon),
        ymin = "incidence_100000_pys_95CI_lower",
        ymax = "incidence_100000_pys_95CI_upper",
        facet = if (input$plot_facet != "None") input$plot_facet else NULL,
        colour = if (input$plot_colour != "None") input$plot_colour else NULL
      ) +
        ggtitle("Incidence estimates") +
        geom_line() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        guides(fill = "none")
    })

    output$incidence_plot <- renderPlotly({
      if (input$interval == "years") {
        ggplotly(plot_incidence())
      } else {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No plot available when interval = 'overall'", size = 6) +
          theme_void()
        ggplotly(p)
      }
    })

    # Download
    output$download_plot <- downloadHandler(
      filename = function() "incidenceEstimatesPlot.png",
      content = function(file) {
        ggsave(
          filename = file,
          plot = plot_incidence(),
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
#   ui = fluidPage(incidenceUI("incidence")),
#   server = function(input, output, session) {
#     incidenceServer("incidence")
#   }
# )
