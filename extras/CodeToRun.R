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

# Make sure to Build and install the R study package P4C1021
# Open the Project in RStudio and click "Install" under the Build tab
# or run devtools::install()

# Then restore the R environment
renv::restore()

library(CDMConnector)
library(P4C1021)

# IMPORTANT: If running this study on INGEF the cdmName must be set to "INGEF" to run the code
# that includes outpatient diagnoses in the observation table

# cdmName <- "INGEF"

# If running on another database the cdmName can be NULL and will automatically be assigned
cdmName <- NULL

# Where should results be saved?
outputFolder <- here::here("Results_synpuf1k")

# create your database connection here
con <- DBI::dbConnect(duckdb::duckdb(), eunomiaDir("synpuf-1k"))

# Create the CDM object
cdm <- cdmFromCon(
  con,
  cdmName = cdmName,
  cdmSchema = "main",
  writeSchema = "main")

# Run the study
runStudy(cdm, outputFolder, minCellCount = 5)

cdmDisconnect(cdm)

# launch shiny app to review results
launchResultsExplorer(dataFolder = outputFolder, launch.browser = TRUE, useCachedData = FALSE)

