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

# ---- UI ----
lsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Large Scale Characteristics of Alzheimer's patients"),
    tabsetPanel(
      tabPanel(
        "Top N Concepts by Data Source",
        numericInput(ns("n"), "Top N", value = 10, min = 1, max = 1000, step = 1),
        DT::DTOutput(ns("topn_table")),
        downloadButton(ns("download_topn_table"), "Download table (.csv)")
      ),
      tabPanel(
        "All Large Scale Characteristics",
        DT::DTOutput(ns("ls_table")),
        downloadButton(ns("download_ls_table"), "Download table (.csv)")
      )
    )
   
  )
}

# ---- SERVER ----
lsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ls_table_dt <- reactive({
      largeScaleCharacteristics |> 
        tableLargeScaleCharacteristics2(
          type = "DT"
        )
    })
    
    output$ls_table <- DT::renderDT({
      ls_table_dt()
    })
    
    output$download_ls_table <- downloadHandler(
      filename = function() {
        "largeScaleCharacteristics.csv"
      },
      content = function(file) {
        readr::write_csv(largeScaleCharacteristics, file)
      }
    )
    
    ls_table_topn <- reactive({
      largeScaleCharacteristics |> 
        top_concepts_by_cdm(n = input$n)
    })
    
    output$topn_table <- DT::renderDT({
      ls_table_topn() |> 
        dplyr::mutate_if(is.character, as.factor) |> 
        DT::datatable(filter = "top")
    })
    
    output$download_topn_table <- downloadHandler(
      filename = function() {
        "TopLargeScaleCharacteristics.csv"
      },
      content = function(file) {
        readr::write_csv(ls_table_topn(), file)
      }
    )
    
  })
}

# ---- RUN ----
# shinyApp(
#   ui = fluidPage(lsUI("ls")),
#   server = function(input, output, session) {
#     lsServer("ls")
#   }
# )
