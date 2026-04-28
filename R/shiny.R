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

#' Launch the Results Explorer Shiny app
#'
#' @param dataFolder       A folder where the exported zip files with the results are stored.
#'                         Zip files containing results from multiple databases can be placed in the same
#'                         folder.
#' @param launch.browser   Should the app be launched in your default browser, or in a Shiny window.
#'                         Note: copying to clipboard will not work in a Shiny window.
#' @param useCachedData    Should the cached data be used if it exists?
#'
#' @details
#' Launches a Shiny app that allows the user to explore the diagnostics
#'
#' @export
launchResultsExplorer <- function(dataFolder, launch.browser = FALSE, useCachedData = FALSE) {
  ensure_installed("DT")
  appDir <- system.file("shiny", "ResultsExplorer", package = "P4C1021")
  shinySettings <- list(dataFolder = dataFolder, useCachedData = useCachedData)
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm(shinySettings, envir = .GlobalEnv))
  shiny::runApp(appDir, launch.browser = launch.browser)
}


#' @keywords internal
#' @noRd
is_installed <- function(pkg, version = "0.0.0") {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= as.package_version(version)
}

#' @importFrom utils menu install.packages
#' @keywords internal
#' @noRd
ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <- paste0(sQuote(pkg), " must be installed for this functionality.")
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}
