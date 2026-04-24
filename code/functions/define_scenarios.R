#' Build a scenario data.table from compact scenario strings
#'
#' Parses a vector of scenario strings (e.g., \code{"pcr-ews"}) into a data.table
#' with one row per data source per scenario, mapping single-letter codes to
#' full deprivation indicator variable names.
#'
#' @param scenario_strings Character vector of scenario strings. Each string encodes
#'   one or more data sources separated by \code{"-"}, where each source is a
#'   sequence of single-letter indicator codes (\code{p, c, r, e, w, s}).
#'
#' @return A data.table with columns: \code{scenario} (the original string),
#'   \code{data_source} (the segment for each source), and \code{ind_list}
#'   (a list-column of character vectors with the full indicator variable names).
define_scenarios <- function(scenario_strings) {

  # Define the mapping from single letters to full variable names
  indicator_map <- c(
    p = "dep_poor1",
    c = "dep_educ_com",
    r = "dep_educ_enr",
    e = "dep_infra_elec",
    w = "dep_infra_impw",
    s = "dep_infra_imps"
  )

  # Validate the input characters
  all_chars <- scenario_strings |>
    str_remove_all("-") |>
    str_split("") |>
    unlist() |>
    unique()

  invalid_chars <- setdiff(all_chars, names(indicator_map))

  if (length(invalid_chars) > 0) {
    warning(paste(
      "Invalid characters detected in input and will be ignored:",
      paste(invalid_chars, collapse = ", ")
    ))
  }

  # Process each scenario string into rows — one per data source
  rbindlist(lapply(scenario_strings, function(scenario) {

    # Split the scenario string by the hyphen to get data sources
    data_sources <- str_split(scenario, "-", simplify = TRUE) |> as.character()

    rbindlist(lapply(data_sources, function(source) {

      # Split the data source string into individual characters
      indicators <- str_split(source, "")[[1]]

      # Translate the characters to full variable names, drop invalid ones
      indicator_list <- unname(indicator_map[indicators])
      indicator_list <- indicator_list[!is.na(indicator_list)]

      data.table(
        scenario    = scenario,
        data_source = source,
        ind_list    = list(indicator_list)
      )
    }))
  }))
}
