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

function(input, output, session) {
  backgroundServer("background")
  databasesServer("databases")
  demographicsServer("demographics")
  cohortAttritionServer("cohortAttrition")
  incidenceServer("incidence")
  prevalenceServer("prevalence")
  comorbiditiesServer("comorbidities", tabName = "comorbidities")
  comorbiditiesServer("diagnostic_procedures", tabName = "diagnostic_procedures")
  comorbiditiesServer("clinical_profile", tabName = "clinical_profile")
  mciServer("mci")
  lsServer("ls")
  reportServer("report")
}
