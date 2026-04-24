# ============================================================================
# COMPARE RANK CORRELATIONS
# ============================================================================

# ============================================================================
# TRUE MONETARY POVERTY AND MPM (national level, by data scenario)
# ============================================================================

# Fusion MPM vs MPM rank correlation (national level, by data scenario)
mpm_val_national <- as.data.table(nanoparquet::read_parquet(here("output/fused_mpm_val_national.parquet")))
mpm_rho <- mpm_val_national[target == "pop_sh" & sim_bias == 1 & data_level == "National",
                             .(scenario, mpm, rho_fusion = rho)]

# Monetary poverty + MPM rank correlation (national level, by data scenario)

  # get survey level true mpm
  mpm <- as.data.table(nanoparquet::read_parquet(here("output/fused_mpm.parquet")))
  mpm <- mpm[sim_bias == 1 & level == "National",
             .(pop_sh_true = mean(pop_sh_true)), by = .(code, year, survname, mpm)]

  # get survey level monetary poverty
  pov <- as.data.table(nanoparquet::read_parquet(here("data/true_mpm_groups.parquet")))
  pov <- pov[level == "National",
             .(pov = sum(pop_sh[dep_poor1 == 1], na.rm = TRUE)),
             by = .(code, year, survname)]

  # merge
  pov_mpm <- merge(pov, mpm, by = c("code", "year", "survname"), all = FALSE)

  # compute rank correlation
  pov_rho <- as.data.table(compute_val_metrics(
    data           = pov_mpm,
    true_col_name  = "pop_sh_true",
    pred_col_names = "pov",
    equal_weights  = "code",
    group_vars     = "mpm"
  ))[, .(pov_rho = rho)]

# compare rank correlations
rho_comparison <- cbind(mpm_rho, pov_rho)
setorder(rho_comparison, mpm)

print(rho_comparison)

# save results to csv
write.csv(rho_comparison, here("output/rho_comparison.csv"), row.names = FALSE)