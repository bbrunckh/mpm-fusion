# Calculate fused multidimensional poverty estimates, bounds &
# leave-one-out conformal prediction intervals - infrastructure indicators

# packages
library(here)

# functions
source(here("mpm_fns.R"))
source(here("compute_mpm.R"))
source(here("define_scenarios.R"))
source(here("fuse_indicators.R"))
source(here("mpm_bounds.R"))
source(here("conformal_intervals_loo.R"))

#------------------------------------------------------------------------------#

#load pre-processed validation data (see prep_validation_data.R)
data <- haven::read_dta(here("data/true_mpm_groups.dta"))

# define grouping variables in data
groups <- c("code", "year", "survname", "level", "sample", "pop_sample")

# define infra variables
deps <- c("dep_infra_elec", "dep_infra_impw", "dep_infra_imps")

# functions defining target multidimensional poverty metrics
inf_fns <- list(
  "al1_dep" = function(data) atleast_k_deps(data, deps, 1),
  "al2_dep" = function(data) atleast_k_deps(data, deps, 2),
  "all3_dep" = function(data) atleast_k_deps(data, deps, 3)
  )

# keep infrastructure deprivations
data <- data |>
  select(c(groups), dep_infra_elec, dep_infra_impw, dep_infra_imps, pop_sh)

#------------------------------------------------------------------------------#
# compute true multidimensional poverty metrics
inf_true <- compute_mpm(data, inf_fns, groups, "pop_sh") |>
  rename(pop_sh_true = pop_sh, pop_sh_valid_true = pop_sh_valid)

#------------------------------------------------------------------------------#

# define validation scenarios
s_list <- c("e-w-s", "e-ws", "es-w", "ew-s", "es-ws", "ew-es", "ew-ws")

scenarios <- define_scenarios(s_list)

# "-" indicates separate data sources. The joint distribution within a data source is known.
# e = "dep_infra_elec": Deprived if HH has no access to electricity
# w = "dep_infra_impw": Deprived if HH has no access to improved drinking water
# s = "dep_infra_imps": Deprived if HH has no access to improved sanitation

# simulated sample bias for each scenario
sim_bias <- c(0.8, 0.85, 0.9, 0.95, 0.975, 1, 1.025, 1.05, 1.1, 1.15, 1.2)

#------------------------------------------------------------------------------#

# loop over validation scenarios
for (s in unique(scenarios$scenario)) {
  message(paste0("Scenario: ",s))
  
  # split validation data into scenario data sets
  scen <- scenarios |> filter(scenario == s)
  
  # loop over data sources to construct df_list
  df_list_nobias <- list()
  
  for (n in unique(scen$data_source)){
    
    ind_list <- scen |> 
      filter(data_source == n) |> 
      pull(ind_list) |> 
      unlist()
    
    df <- data |>
      group_by(pick(all_of(c(groups, ind_list)))) |>
      summarise(pop_sh = sum(pop_sh), .groups = "drop") 
    
    df_list_nobias[[n]] <- df
  }
  
  # loop over simulated sample bias scenarios
  for (sb in unique(sim_bias)) {
    message(paste0("Simulated bias: ",sb))
    
    df_list <- df_list_nobias
    
    # add simulated bias to secondary data sets
    if (sb != 1){
      for (n in 2:length(df_list)){
        df_list[[n]] <- df_list[[n]] |>
          mutate(n_dep = rowSums(across(starts_with("dep_")), na.rm = TRUE),
                 pop_sh = if_else(n_dep > 0, 
                                  pop_sh*sb,
                                  pop_sh)) |>
          group_by(pick(all_of(c(groups)))) |>
          mutate(pop_sh = if_else(n_dep == 0, 
                                  pop_sh + (1-sum(pop_sh)), 
                                  pop_sh)) |>
          group_by(code, year, survname, level) |>
          filter( # remove level if any bias adjusted pop_sh is outside [0,1]
            all(round(pop_sh,3) >= 0 & round(pop_sh,3) <= 1)) |>
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
    fused <- fuse_indicators(df_list = df_list, prob_col_name = "pop_sh")
    
    # compute predicted (fusion) mpm and theoretical bounds
    ind_list <- scen |> pull(ind_list)
    
    fused_inf <- mpm_bounds(
      fused_dt = fused, 
      mpm_fns = inf_fns,
      ind_list = ind_list,
      group_vars = groups,
      prob_col_name = "pop_sh"
    )
    
    # bind fused_inf for this scenario
    if (s == unique(scenarios$scenario)[1] & sb == unique(sim_bias)[1]) {
      fused_inf_all <- mutate(fused_inf, scenario = s, sim_bias = sb)
    } else fused_inf_all <- bind_rows(
      fused_inf_all, mutate(fused_inf, scenario = s, sim_bias = sb))
  }
}
#------------------------------------------------------------------------------#

# Merge true multidimensional poverty metrics
fused_inf_all <- left_join(inf_true, fused_inf_all, by = c(groups, "mpm")) |>
  # keep if at least 90% of (weighted) sample is used for mpm estimates
  filter(pop_sh_valid_true >= 0.90, pop_sh_valid >= 0.90)

# Normalized conformal prediction intervals
# alpha = 0.05 (95% prediction intervals)
# global + localized by fusion estimate and bounds (nearest sqrt(n) neighbors)
# weight countries equally
# leave (own) country out of validation dataset

fused_inf_pred <- conformal_intervals_loo(
  fused_inf_all, 
  est_col_name = "pop_sh",
  true_col_name = "pop_sh_true",
  alpha = 0.05,
  feature_cols = c("pop_sh"),
  group_vars = c("scenario", "sim_bias", "mpm", "level"),
  equal_weights = "code",
  leave_out = "code"
) 

# save results
fused_inf_pred <- fused_inf_pred |> 
  relocate(scenario, sim_bias, .before = 1) |> 
  arrange(sim_bias)

nanoparquet::write_parquet(fused_inf_pred, 
                           here("output/fused_inf.parquet"))

haven::write_dta(fused_inf_pred, here("output/fused_inf.dta"))
