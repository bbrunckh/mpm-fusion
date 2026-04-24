# MPM fusion: estimate bounds, conformal intervals, and validation metrics

# ============================================================================
# FUSION
# ============================================================================

#load pre-processed validation data (see prep_validation_data.R)
data <- nanoparquet::read_parquet(here("data/true_mpm_groups.parquet"))

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
  "mpm"     = function(data) weighted_headcount(data, dims, weights, 1/3),
  "mpm_af"  = function(data) adjusted_headcount(data, dims, weights, 1/3)
)

#------------------------------------------------------------------------------#
# compute true multidimensional poverty metrics
mpm_true <- compute_mpm(data, mpm_fns, groups, "pop_sh")
setnames(mpm_true, c("pop_sh", "pop_sh_valid"), c("pop_sh_true", "pop_sh_valid_true"))

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

fused_mpm_all <- run_fusion_scenarios(
  data          = data,
  metric_fns    = mpm_fns,
  scenarios     = scenarios,
  sim_bias      = sim_bias,
  groups        = groups,
  prob_col_name = "pop_sh"
)

#------------------------------------------------------------------------------#

# Merge true multidimensional poverty metrics (mpm_true is left table)
fused_mpm_all <- merge(mpm_true, fused_mpm_all, by = c(groups, "mpm"), all.x = TRUE)

# keep if at least 90% of (weighted) sample is used for mpm estimates
fused_mpm_all <- fused_mpm_all[pop_sh_valid_true >= 0.90 & pop_sh_valid >= 0.90]

# Normalized conformal prediction intervals
  # alpha = 0.05 (95% prediction intervals)
  # global + localized by fusion estimate and bounds (nearest sqrt(n) neighbors)
  # weight countries equally
  # leave (own) country out of validation dataset

fused_mpm_pred <- conformal_intervals_loo(
  fused_mpm_all,
  est_col_name  = "pop_sh",
  true_col_name = "pop_sh_true",
  alpha         = 0.05,
  feature_cols  = c("pop_sh"),
  group_vars    = c("scenario", "sim_bias", "mpm", "level"),
  equal_weights = "code",
  leave_out     = "code"
)

# save results
fused_mpm_pred <- as.data.table(fused_mpm_pred)
setcolorder(fused_mpm_pred,
  c("scenario", "sim_bias", setdiff(names(fused_mpm_pred), c("scenario", "sim_bias"))))
setorder(fused_mpm_pred, sim_bias)

nanoparquet::write_parquet(fused_mpm_pred, here("output/fused_mpm.parquet"))

# ============================================================================
# VALIDATION METRICS
# ============================================================================

run_validation(
  input_file    = "output/fused_mpm.parquet",
  output_prefix = "fused_mpm",
  group_vars    = c("code", "year", "survname", "level"),
  equal_weights = "code"
)
