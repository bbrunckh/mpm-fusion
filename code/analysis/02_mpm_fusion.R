# Calculate fused multidimensional poverty estimates, bounds &
# leave-one-out conformal prediction intervals

# packages
library(here)

# functions
source(here("mpm_fns.R"))
source(here("compute_mpm.R"))
source(here("define_scenarios.R"))
source(here("fuse_indicators.R"))
source(here("mpm_bounds.R"))
source(here("conformal_intervals_loo.R"))
source(here("run_fusion_scenarios.R"))

#------------------------------------------------------------------------------#

#load pre-processed validation data (see prep_validation_data.R)
data <- haven::read_dta(here("data/true_mpm_groups.dta"))

# define grouping variables in data
groups <- c("code", "year", "survname", "level", "sample", "pop_sample")

# define MPM variables
dims <- list("dep_poor1", c("dep_educ_com", "dep_educ_enr"), 
             c("dep_infra_elec", "dep_infra_impw", "dep_infra_imps"))

weights <- c(1/3, 1/3, 1/3)

deps <- unlist(dims)

# functions defining target multidimensional poverty metrics
mpm_fns <- list(
  "al1_dep" = function(data) atleast_k_deps(data, deps, 1),
  "al3_dep" = function(data) atleast_k_deps(data, deps, 3),
  "mpm" = function(data) weighted_headcount(data, dims, weights, 1/3),
  "mpm_af" = function(data) adjusted_headcount(data, dims, weights, 1/3)
)

#------------------------------------------------------------------------------#
# compute true multidimensional poverty metrics
mpm_true <- compute_mpm(data, mpm_fns, groups, "pop_sh") |>
  rename(pop_sh_true = pop_sh, pop_sh_valid_true = pop_sh_valid)

#------------------------------------------------------------------------------#

# define validation scenarios
s_list <- c("p-c-r-e-w-s", "p-cr-ews", "pcr-ews","pcre-ws",
            "pce-rews","pcew-rews","pcews-rews")

scenarios <- define_scenarios(s_list)

  # "-" indicates separate data sources. The joint distribution within a data source is known.
  # p = "dep_poor1": Deprived if HH below extreme poverty line ($3.00 per day, 2021 PPP)
  # c = "dep_educ_com": Deprived if no adults 15+ in HH completed primary education
  # r = "dep_educ_enr": Deprived if at least one school-aged child in HH not enrolled in school
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
    
    fused_mpm <- mpm_bounds(
      fused_dt = fused, 
      mpm_fns = mpm_fns,
      ind_list = ind_list,
      group_vars = groups,
      prob_col_name = "pop_sh"
      )
      
    # bind fused_mpm for this scenario
    if (s == unique(scenarios$scenario)[1] & sb == unique(sim_bias)[1]) {
      fused_mpm_all <- mutate(fused_mpm, scenario = s, sim_bias = sb)
    } else fused_mpm_all <- bind_rows(
      fused_mpm_all, mutate(fused_mpm, scenario = s, sim_bias = sb))
  }
}
#------------------------------------------------------------------------------#

# Merge true multidimensional poverty metrics
fused_mpm_all <- left_join(mpm_true, fused_mpm_all, by = c(groups, "mpm")) |>
  # keep if at least 90% of (weighted) sample is used for mpm estimates
  filter(pop_sh_valid_true >= 0.90, pop_sh_valid >= 0.90)

# Normalized conformal prediction intervals
  # alpha = 0.05 (95% prediction intervals)
  # global + localized by fusion estimate and bounds (nearest sqrt(n) neighbors)
  # weight countries equally
  # leave (own) country out of validation dataset

fused_mpm_pred <- conformal_intervals_loo(
  fused_mpm_all, 
  est_col_name = "pop_sh",
  true_col_name = "pop_sh_true",
  alpha = 0.05,
  feature_cols = c("pop_sh"),
  group_vars = c("scenario", "sim_bias", "mpm", "level"),
  equal_weights = "code",
  leave_out = "code"
  ) 

# save results
fused_mpm_pred <- fused_mpm_pred |> 
  relocate(scenario, sim_bias, .before = 1) |> 
  arrange(sim_bias)

nanoparquet::write_parquet(fused_mpm_pred, here("output/fused_mpm.parquet"))
haven::write_dta(fused_mpm_pred, here("output/fused_mpm.dta"))
