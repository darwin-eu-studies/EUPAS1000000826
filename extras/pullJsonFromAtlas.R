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

# this script extracts the cohort json files from Atlas.

# Concept sets for this study are prefixed with either P4-C1-023 or P4-C1-021

# library(ROhdsiWebApi)
library(dplyr)

baseUrl <- "https://atlas.darwin-eu.org/WebAPI"

# remove the inst folder if it exists and recreate it
dir.create(here::here("inst", "cohorts"))
dir.create(here::here("inst", "concept_sets"))

# this token was copied from the browser
token <- readr::read_file("extras/token.txt")
setAuthHeader(baseUrl, authHeader = token)

md0 <- getCohortDefinitionsMetaData(baseUrl)

conceptSetIds <- c(
  3700,
  3650,
  3698,
  3703,
  3699,
  3686,
  3704,
  3702,
  3701,
  3697,
  3696,
  3695,
  3694,
  3693,
  3692,
  3691,
  3690,
  3689,
  3688,
  3687,
  3499,
  3496,
  3498,
  3493,
  3484,
  3482,
  3473)

md <- md0 %>%
  filter(stringr::str_detect(name, "P4-C1-021")) %>%
  select(name, id) %>%
  filter(id == 2183L) |>
  mutate(name2 = snakecase::to_snake_case(tolower(stringr::str_remove(name, "P4-C1-021 "))))

cohortsToCreate <- dplyr::tibble(atlasId = md$id, cohortId = md$id, cohortName = md$name2)
readr::write_csv(cohortsToCreate, here::here("inst", "cohortsToCreate.csv"))

insertCohortDefinitionSetInPackage(
  fileName = here::here("inst/cohortsToCreate.csv"),
  baseUrl = baseUrl,
  packageName = "P4C1021",
  insertCohortCreationR = F
)

for (i in seq_len(nrow(cohortsToCreate))) {
  file.rename(here::here("inst/cohorts", paste0(cohortsToCreate[i, "cohortId"], ".json")),
              here::here("inst/cohorts", paste0(cohortsToCreate[i, "cohortName"], ".json")))
}

unlink(here::here("inst", "sql"), recursive = T)
file.remove(here::here("inst", "cohortsToCreate.csv"))

df0 <- getConceptSetDefinitionsMetaData(baseUrl)

df <- df0 %>%
  # filter(stringr::str_detect(name, "P4-C1-023|P4-C1-021")) %>%
  select(id, name) %>%
  filter(id %in% conceptSetIds) |>
  mutate(name = stringr::str_remove_all(name, "COPY OF ")) %>%
  mutate(name = stringr::str_remove_all(name, "P4-C1-023_|P4-C1-021_|’")) %>%
  mutate(name = snakecase::to_snake_case(name)) %>%
  mutate(name = tolower(name)) |>
  mutate(name = stringr::str_remove_all(name, "p_4_c_1_021_|p_4_c_1_023_"))

print(df, n=100)

dir.create(here::here("inst/concept_sets"))

for (i in cli::cli_progress_along(1:nrow(df))) {
  cpt <- getConceptSetDefinition(df$id[i], baseUrl)

  jsonlite::write_json(cpt$expression,
                       path = here::here("inst", "concept_sets", paste0(df$name[i], ".json")),
                       pretty = T,
                       auto_unbox = T)
}


