# ============================================================================
# COMPARE RANK CORRELATIONS
# ============================================================================

# Load required packages
library(here)
library(dplyr)
library(nanoparquet)
library(haven)

# functions
source(here("validation_metrics.R"))

# ============================================================================
# TRUE MONETARY POVERTY AND MPM (national level, by data scenario)
# ============================================================================

# Fusion MPM vs MPM rank correlation (national level, by data scenario)
mpm_val_national <- read_parquet(here("../../04.output/fused_mpm_val_national.parquet"))
mpm_rho <- mpm_val_national |>
  filter(target == "pop_sh", sim_bias == 1, 
         data_level == "National") |>
  select(scenario, mpm, rho) |>
  rename(rho_fusion = rho)

# Monetary poverty + MPM rank correlation (national level, by data scenario)

  # get survey level true mpm
  mpm <- read_parquet(here("../../04.output/fused_mpm.parquet")) |>
    filter(sim_bias == 1, level == "National") |>
    summarise(pop_sh_true = mean(pop_sh_true), .by = c(code, year, survname, mpm))
  
  # get survey level monetary poverty
  pov <- read_dta(here("../../03.intermediate/true_mpm_groups.dta")) |> 
    filter(level == "National") |>
    summarise(pov = sum(pop_sh[dep_poor1==1], na.rm = TRUE),
              .by = c(code, year, survname))
  
  # merge
  pov_mpm <- pov |>
    inner_join(mpm) 
  
  # compute rank correlation
  pov_rho <- compute_val_metrics(
    data = pov_mpm ,
    true_col_name = "pop_sh_true",
    pred_col_names = "pov",
    equal_weights = "code",
    group_vars = "mpm"
  ) |>
    select(rho) |>
    rename(pov_rho = rho)
  
# compare rank correlations
rho_comparison <- cbind(mpm_rho, pov_rho) |>
  arrange(mpm)

rho_comparison
  
