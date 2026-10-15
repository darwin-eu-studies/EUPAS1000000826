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

# Module 2: Databases Info Tab ----

library(shiny)
library(bslib)
library(DT)

# requires dbinfo dataframe

databasesUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      div(
        style = "overflow-x: auto;",
        DTOutput(ns("table"))
      )
    ),
    fluidRow(
      column(12,
             downloadButton(ns("download"), "Download CSV")
      )
    )
  )
}

databasesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDT({
      dbinfo |> 
        mutate(cdm_name = factor(cdm_name, levels = c("NAJS", "DK-DHR", "INGEF", "IQVIA DA Germany", "IPCI", "CPRD GOLD"))) |> 
        arrange(cdm_name) |> 
        rename(data_source = "cdm_name") |> 
        datatable(options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download <- downloadHandler(
      filename = function() {
        "dbinfo.csv"
      },
      content = function(file) {
        dbinfo |> 
          rename(data_source = "cdm_name") |> 
          write.csv(file, row.names = FALSE)
      }
    )
  })
}

# Use this to test the module independently
# shinyApp(
#   ui = fluidPage(databasesUI("databases")),
#   server = function(input, output, session) {
#     databasesServer("databases")
#   }
# )
