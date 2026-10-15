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

dashboardPage(
  dashboardHeader(title = tags$div(
    style = "line-height: 1.2; display: inline-block;",
    tags$span("P4-C1-021", style = "font-size: 20px; font-weight: 600; display:block; margin: 0; padding: 0; display: block;"),
    tags$small("Alzheimer's disease characterization", style = "font-size: 12px; color: #E0E0E0; margin: 0; padding:0; display: block;")
  ),
  tags$li(
    class = "dropdown",
    tags$div(
      class = "logo-container",
      img(src = "__logo__.png", height = "40px", style = "margin-right:15px;")
    ))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Background", tabName = "background", icon = icon("info-circle")),
      menuItem("Databases", tabName = "databases", icon = icon("database")),
      menuItem("Attrition", tabName = "attrition", icon = icon("diagram-next")),
      menuItem("Demographics", tabName = "demographics", icon = icon("person")),
      menuItem("Objective 1",
        menuSubItem("Incidence", tabName = "incidence", icon = icon("chart-line")),
        menuSubItem("Prevalence", tabName = "prevalence", icon = icon("chart-line"))  
      ),
      menuItem("Objective 2",
        menuSubItem("Comorbidities", tabName = "comorbidities", icon = icon("person")),
        menuSubItem("Diagnostic Procedures", tabName = "diagnostic_procedures", icon = icon("person")),
        menuSubItem("Clinical Profile", tabName = "clinical_profile", icon = icon("person")),
        menuSubItem("Time from MCI to AD", tabName = "mci", icon = icon("clock")),
        menuSubItem("Large Scale Characteristics", tabName = "ls", icon = icon("notes-medical"))
      ),
      menuItem("Report", tabName = "report", icon = icon("file"))
    )
  ),
  
  dashboardBody(
    includeCSS("www/custom.css"),
    tabItems(
      tabItem("background", backgroundUI("background")),
      tabItem("databases", databasesUI("databases")),
      tabItem("demographics", demographicsUI("demographics")),
      tabItem("attrition", cohortAttritionUI("cohortAttrition")),
      tabItem("incidence", incidenceUI("incidence")),
      tabItem("prevalence", prevalenceUI("prevalence")),
      tabItem("comorbidities", comorbiditiesUI("comorbidities", title = "Comorbidities")),
      tabItem("diagnostic_procedures", comorbiditiesUI("diagnostic_procedures", title = "Diagnostic Procedures")),
      tabItem("clinical_profile", comorbiditiesUI("clinical_profile", title = "Clinical Profile")),
      tabItem("mci", mciUI("mci")),
      tabItem("ls", lsUI("ls")),
      tabItem("report", reportUI("report"))
    )
  )
)
