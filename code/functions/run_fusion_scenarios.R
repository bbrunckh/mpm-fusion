run_fusion_scenarios <- function(data, metric_fns, scenarios, sim_bias, groups, prob_col_name = "pop_sh") {

  fused_all <- NULL

  # loop over validation scenarios
  for (s in unique(scenarios$scenario)) {
    message(paste0("Scenario: ", s))

    # split validation data into scenario data sets
    scen <- scenarios |> filter(scenario == s)

    # loop over data sources to construct df_list
    df_list_nobias <- list()

    for (n in unique(scen$data_source)) {

      ind_list <- scen |>
        filter(data_source == n) |>
        pull(ind_list) |>
        unlist()

      df <- data |>
        group_by(pick(all_of(c(groups, ind_list)))) |>
        summarise(!!prob_col_name := sum(.data[[prob_col_name]]), .groups = "drop")

      df_list_nobias[[n]] <- df
    }

    # loop over simulated sample bias scenarios
    for (sb in unique(sim_bias)) {
      message(paste0("Simulated bias: ", sb))

      df_list <- df_list_nobias

      # add simulated bias to secondary data sets
      if (sb != 1) {
        for (n in 2:length(df_list)) {
          df_list[[n]] <- df_list[[n]] |>
            mutate(n_dep = rowSums(across(starts_with("dep_")), na.rm = TRUE),
                   !!prob_col_name := if_else(n_dep > 0,
                                             .data[[prob_col_name]] * sb,
                                             .data[[prob_col_name]])) |>
            group_by(pick(all_of(c(groups)))) |>
            mutate(!!prob_col_name := if_else(n_dep == 0,
                                             .data[[prob_col_name]] + (1 - sum(.data[[prob_col_name]])),
                                             .data[[prob_col_name]])) |>
            group_by(code, year, survname, level) |>
            filter( # remove level if any bias adjusted pop_sh is outside [0,1]
              all(round(.data[[prob_col_name]], 3) >= 0 & round(.data[[prob_col_name]], 3) <= 1)) |>
            ungroup() |>
            select(-n_dep)
        }
      }

      # filter data sets to keep the same levels for each survey
      common_levels <- df_list |>
        map(~ .x |> distinct(code, year, survname, level)) |>
        reduce(inner_join, by = c("code", "year", "survname", "level"))

      df_list <- df_list |>
        map(~ .x |> semi_join(common_levels,
                              by = c("code", "year", "survname", "level")))

      # if no data remains after filtering, move to next scenario
      if (all(map_lgl(df_list, ~ nrow(.x) == 0))) next

      # fuse indicators across data sets, calculate FH bounds
      fused <- fuse_indicators(df_list = df_list, prob_col_name = prob_col_name)

      # compute predicted (fusion) mpm and theoretical bounds
      ind_list <- scen |> pull(ind_list)

      fused_result <- mpm_bounds(
        fused_dt = fused,
        mpm_fns = metric_fns,
        ind_list = ind_list,
        group_vars = groups,
        prob_col_name = prob_col_name
      )

      # accumulate results
      fused_all <- bind_rows(fused_all, mutate(fused_result, scenario = s, sim_bias = sb))
    }
  }

  return(fused_all)
}
