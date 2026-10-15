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

# Module 1: Background Tab ----
library(shiny)

backgroundUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 12,
        card(
          card_header("Background"),
          uiOutput(ns("background_md"))
        )
      )
    )
  )
}

backgroundServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$background_md <- renderUI({
      # Read the markdown file and render as HTML
      md_file <- "background.md"
      if (file.exists(md_file)) {
        includeMarkdown(md_file)
      } else {
        HTML("<p><em>background.md file not found.</em></p>")
      }
    })
  })
}


# shinyApp(
#   ui = fluidPage(backgroundUI("bg")),
#   server = function(input, output, session) {
#     backgroundServer("bg")
#   }
# )
