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

# utility functions

# Overwrite of tableLargeScaleCharacteristics to add counts to the table as well as percentages
# result <- largeScaleCharacteristics
# type = "DT"


tableLargeScaleCharacteristics2 <- function (result, compareBy = NULL, hide = c("type"), smdReference = NULL, type = "reactable") {
  result <- dplyr::filter(omopgenerics::filterSettings(omopgenerics::validateResultArgument(result), 
                                                       .data$result_type == "summarise_large_scale_characteristics"), 
                          .data$estimate_name %in% c("count", "percentage")) 
  
  strataCols <- omopgenerics::strataColumns(result)
  choic <- c("cdm_name", "cohort_name", strataCols, "variable_level", 
             "type")
  hide <- hide %||% character()
  omopgenerics::assertChoice(type, choices = c("DT", "reactable"))
  omopgenerics::assertChoice(compareBy, choices = choic, length = 1, 
                             null = TRUE)
  omopgenerics::assertChoice(hide, choices = choic)
  rlang::check_installed(pkg = type)
  hide <- hide[!hide %in% compareBy]
  result <- dplyr::select(omopgenerics::tidy(result), !dplyr::all_of(hide))
  if (length(compareBy) == 0) {
    opts <- c("count", "percentage")
    smdReference <- NULL
  } else {
    opts <- unique(result[[compareBy]])
    omopgenerics::assertChoice(smdReference, choices = opts, 
                               length = 1, null = TRUE)
  }
  if (!is.null(compareBy)) {
    result <- tidyr::pivot_wider(result, names_from = dplyr::all_of(compareBy), 
                                 values_fill = 0, values_from = "percentage")
  }
  result <- dplyr::select(result, dplyr::any_of(c("cdm_name", 
                                                  "cohort_name", strataCols, "type", window = "variable_level", domain = "table_name",
                                                  concept_name = "variable_name", "concept_id", "source_concept_name", 
                                                  "source_concept_id", opts)))
  if (length(smdReference) > 0) {
    cols <- character()
    for (col in opts) {
      if (col == smdReference) {
        ref <- rlang::set_names(smdReference, paste0(smdReference, 
                                                     " (ref)"))
      }
      else {
        result <- dplyr::mutate(result, `:=`(!!paste0(col, " SMD"), qSmd(.data[[smdReference]], .data[[col]])))
        cols <- c(cols, col, paste0(col, " SMD"))
      }
    }
    result <- dplyr::relocate(result, dplyr::all_of(c(ref, cols)), .after = dplyr::last_col())
  }
  if (type == "DT") {
    
    out <- result |> 
      dplyr::rename(data_source = "cdm_name") |> 
      dplyr::mutate_if(is.character, as.factor) |> 
      DT::datatable(filter = "top")
  }
  else {
    out <- reactable::reactable(result)
  }
  return(out)
}


# result <- largeScaleCharacteristics

top_concepts_by_cdm <- function(result, n = 10) {
  library(dplyr)
  
  result <- dplyr::filter(omopgenerics::filterSettings(omopgenerics::validateResultArgument(result), 
                                                       .data$result_type == "summarise_large_scale_characteristics"), 
                          .data$estimate_name %in% c("count", "percentage")) 
  
  strataCols <- omopgenerics::strataColumns(result)
  choic <- c("cdm_name", "cohort_name", strataCols, "variable_level", "type")
  result <- omopgenerics::tidy(result)
  opts <- c("count", "percentage")
  result <- dplyr::select(result, dplyr::any_of(c("cdm_name", 
                                                  "cohort_name", strataCols, "type", window = "variable_level", domain = "table_name",
                                                  concept_name = "variable_name", "concept_id", "source_concept_name", 
                                                  "source_concept_id", opts)))

    
    result %>%
      group_by(cdm_name, domain, window) %>%
      arrange(desc(count), .by_group = TRUE) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= n) %>%
      ungroup() %>%
      mutate(
        concept_info = paste0(
          concept_name,
          " | count=", count,
          " | pct=", percentage
        )
      ) %>%
      select(domain, window, rank, cdm_name, concept_info) %>%
      tidyr::pivot_wider(
        id_cols   = c(domain, window, rank),
        names_from  = cdm_name,
        values_from = concept_info
      ) %>%
      arrange(domain, window, rank) 
}



gt_style_darwin <- function(tbl) {
  require(gt)
  blue <- "#003399"
  tbl |>   
    # HEADER + SPANNERS: background + borders
    tab_style(
      style = list(
        cell_fill(color = blue),
        cell_text(color = "white", size = 18),
        cell_borders(sides = "all", color = blue, weight = px(2))
      ),
      locations = list(
        cells_title(groups = c("title", "subtitle")),
        cells_column_spanners(everything()),
        cells_column_labels(everything())
      )
    ) %>%
    
    # BODY borders AND MAKE BODY TEXT 9PT
    tab_style(
      style = list(
        cell_borders(sides = "all", color = blue, weight = px(1)),
        cell_text(size = 18)
      ),
      locations = cells_body()
    ) %>%
    
    # STUB left border AND MAKE STUB TEXT 9PT
    tab_style(
      style = list(
        cell_borders(sides = "left", color = blue, weight = px(1)),
        cell_text(size = 18)
      ),
      locations = cells_stub()
    ) %>%
    
    # Add a right border to the last column cells
    tab_style(
      style = cell_borders(sides = "right", color = blue, weight = px(1)),
      locations = cells_body(columns = dplyr::last_col())
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = blue, weight = px(1)),
      locations = cells_column_labels(columns = dplyr::last_col())
    ) %>%
    tab_style(
      style = cell_fill(color = blue),
      locations = cells_column_spanners()
    ) |> 
    
    # TABLE-WIDE OUTER BORDERS (1 pt), now including left and right borders
    tab_options(
      table.border.top.color    = blue,
      table.border.bottom.color = blue,
      table.border.left.color   = blue,     # set left border color
      table.border.right.color  = blue,     # set right border color
      table.border.top.width    = px(1),
      table.border.bottom.width = px(1),
      table.border.left.width   = px(1),    # set left border width
      table.border.right.width  = px(1),    # set right border width
      
      # Ensure all cell and header borders are blue, and header background is blue
      table_body.border.top.color        = blue,
      table_body.border.bottom.color     = blue,
      table_body.hlines.color            = blue,
      table_body.vlines.color            = blue,
      column_labels.border.top.color     = blue,
      column_labels.border.bottom.color  = blue,
      column_labels.vlines.color         = blue,
      column_labels.background.color     = blue,
      heading.border.bottom.color        = blue,
      stub.border.color                  = blue
    )
}

rename_spanner <- function(gt_tbl) {
  gt_tbl[["_spanners"]]$spanner_label <- "Data Source"
  gt_tbl
}
