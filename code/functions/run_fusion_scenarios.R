#' Run fusion scenarios over multiple data sources and bias factors
#'
#' Loops over all scenarios and simulated bias levels, fuses indicator data
#' across data sources using conditional independence, and computes MPM bounds.
#'
#' @param data Data frame with the full survey microdata including deprivation indicators.
#' @param metric_fns Named list of functions defining poverty metrics (passed to \code{mpm_bounds}).
#' @param scenarios data.table from \code{define_scenarios()} with columns: scenario, data_source, ind_list.
#' @param sim_bias Numeric vector of simulated bias multipliers (1 = no bias).
#' @param groups Character vector of grouping column names.
#' @param prob_col_name Name of the population share column (default = "pop_sh").
#'
#' @return data.table with columns: [groups], mpm, pop_sh, lower_bound, upper_bound, status, pop_sh_valid, scenario, sim_bias.
run_fusion_scenarios <- function(data, metric_fns, scenarios, sim_bias, groups,
                                  prob_col_name = "pop_sh") {

  data <- as.data.table(data)
  level_cols <- c("code", "year", "survname", "level")

  scenario_names <- unique(scenarios$scenario)

  results_by_scenario <- lapply(scenario_names, function(s) {
    message(paste0("Scenario: ", s))

    scen <- scenarios[scenario == s]

    # build df_list — one grouped dataset per data source
    df_list_nobias <- list()

    for (n in unique(scen$data_source)) {
      ind_list <- unlist(scen[data_source == n, ind_list])
      by_cols  <- c(groups, ind_list)
      df <- data[, setNames(list(sum(get(prob_col_name))), prob_col_name), by = by_cols]
      df_list_nobias[[n]] <- df
    }

    results_list <- list()

    # loop over simulated sample bias scenarios
    for (sb in unique(sim_bias)) {
      message(paste0("  Simulated bias: ", sb))

      df_list <- df_list_nobias

      # add simulated bias to secondary data sets
      if (sb != 1) {
        for (n in 2:length(df_list)) {
          dt <- copy(df_list[[n]])
          dep_cols <- grep("^dep_", names(dt), value = TRUE)

          # count deprivations per row
          dt[, n_dep := rowSums(.SD, na.rm = TRUE), .SDcols = dep_cols]

          # scale up deprived rows
          dt[n_dep > 0, (prob_col_name) := get(prob_col_name) * sb]

          # renormalize non-deprived rows so shares sum to 1 within groups
          dt[, (prob_col_name) := fifelse(
            n_dep == 0,
            get(prob_col_name) + (1 - sum(get(prob_col_name))),
            get(prob_col_name)
          ), by = groups]

          # remove groups where any adjusted share falls outside [0, 1]
          valid <- dt[, .(keep = all(
            round(get(prob_col_name), 3) >= 0 & round(get(prob_col_name), 3) <= 1
          )), by = level_cols]
          dt <- dt[valid[keep == TRUE, ..level_cols], on = level_cols, nomatch = NULL]

          dt[, n_dep := NULL]
          df_list[[n]] <- dt
        }
      }

      # filter data sets to the same levels present in all sources
      common_levels <- Reduce(
        function(a, b) merge(a, b, by = level_cols, all = FALSE),
        lapply(df_list, function(x) unique(as.data.table(x)[, ..level_cols]))
      )
      df_list <- lapply(df_list, function(x) {
        as.data.table(x)[common_levels, on = level_cols, nomatch = NULL]
      })

      # skip if no data remains
      if (all(sapply(df_list, function(x) nrow(x) == 0L))) next

      # fuse indicators across data sets, calculate FH bounds
      fused <- fuse_indicators(df_list = df_list, prob_col_name = prob_col_name)

      # compute predicted (fusion) mpm and theoretical bounds
      ind_list <- scen[, ind_list]

      fused_result <- mpm_bounds(
        fused_dt      = fused,
        mpm_fns       = metric_fns,
        ind_list      = ind_list,
        group_vars    = groups,
        prob_col_name = prob_col_name
      )

      fused_result_dt <- as.data.table(fused_result)
      fused_result_dt[, `:=`(scenario = s, sim_bias = sb)]
      results_list[[length(results_list) + 1L]] <- fused_result_dt
    }

    results_list
  })

  rbindlist(unlist(results_by_scenario, recursive = FALSE))
}
