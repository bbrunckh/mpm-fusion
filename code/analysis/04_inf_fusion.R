# Infrastructure fusion: estimate bounds, conformal intervals, and validation metrics

# ============================================================================
# FUSION
# ============================================================================

#load pre-processed validation data (see prep_validation_data.R)
data <- nanoparquet::read_parquet(here("data/true_mpm_groups.parquet"))

# define grouping variables in data
groups <- c("code", "year", "survname", "level", "sample", "pop_sample")

# define infra variables
deps <- c("dep_infra_elec", "dep_infra_impw", "dep_infra_imps")

# functions defining target multidimensional poverty metrics
inf_fns <- list(
  "al1_dep"  = function(data) atleast_k_deps(data, deps, 1),
  "al2_dep"  = function(data) atleast_k_deps(data, deps, 2),
  "all3_dep" = function(data) atleast_k_deps(data, deps, 3)
)

# keep infrastructure deprivations only
keep <- c(groups, "dep_infra_elec", "dep_infra_impw", "dep_infra_imps", "pop_sh")
data <- as.data.table(data)[, ..keep]

#------------------------------------------------------------------------------#
# compute true multidimensional poverty metrics
inf_true <- compute_mpm(data, inf_fns, groups, "pop_sh")
setnames(inf_true, c("pop_sh", "pop_sh_valid"), c("pop_sh_true", "pop_sh_valid_true"))

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

fused_inf_all <- run_fusion_scenarios(
  data          = data,
  metric_fns    = inf_fns,
  scenarios     = scenarios,
  sim_bias      = sim_bias,
  groups        = groups,
  prob_col_name = "pop_sh"
)

#------------------------------------------------------------------------------#

# Merge true multidimensional poverty metrics (inf_true is left table)
fused_inf_all <- merge(inf_true, fused_inf_all, by = c(groups, "mpm"), all.x = TRUE)

# keep if at least 90% of (weighted) sample is used for mpm estimates
fused_inf_all <- fused_inf_all[pop_sh_valid_true >= 0.90 & pop_sh_valid >= 0.90]

# Normalized conformal prediction intervals
# alpha = 0.05 (95% prediction intervals)
# global + localized by fusion estimate and bounds (nearest sqrt(n) neighbors)
# weight countries equally
# leave (own) country out of validation dataset

fused_inf_pred <- conformal_intervals_loo(
  fused_inf_all,
  est_col_name  = "pop_sh",
  true_col_name = "pop_sh_true",
  alpha         = 0.05,
  feature_cols  = c("pop_sh"),
  group_vars    = c("scenario", "sim_bias", "mpm", "level"),
  equal_weights = "code",
  leave_out     = "code"
)

# save results
fused_inf_pred <- as.data.table(fused_inf_pred)
setcolorder(fused_inf_pred,
  c("scenario", "sim_bias", setdiff(names(fused_inf_pred), c("scenario", "sim_bias"))))
setorder(fused_inf_pred, sim_bias)

nanoparquet::write_parquet(fused_inf_pred, here("output/fused_inf.parquet"))

# ============================================================================
# VALIDATION METRICS
# ============================================================================

run_validation(
  input_file    = "output/fused_inf.parquet",
  output_prefix = "fused_inf",
  group_vars    = c("code", "year", "survname", "level"),
  equal_weights = "code"
)
