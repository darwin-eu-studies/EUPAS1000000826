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
library(rmarkdown)
library(shinybusy)

reportUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Download Word report"),
    
    add_busy_spinner(
      spin = "fading-circle",          
      position = "top-left",  
      timeout = 500,
      margins = c(300, 500),
      height = "300px",
      width = "300px"
    ),
    
    br(),
    downloadButton(ns("download_report"), "Create & download Word report")
  )
}

reportServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("report-", Sys.Date(), ".docx")
      },
      content = function(file) {
        
        # Copy the Rmd to a temp dir (good practice)
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Render the Rmd to Word
        rmarkdown::render(
          input         = tempReport,
          output_format = "word_document",
          output_file   = file,   # <- important: render directly to the 'file' path
          envir         = new.env(parent = globalenv())
        )
      }
    )
  })
}


# ---- RUN ----
# shinyApp(
#   ui = fluidPage(reportUI("report")),
#   server = function(input, output, session) {
#     reportServer("report")
#   }
# )