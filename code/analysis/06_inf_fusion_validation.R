# Validate fused multidimensional poverty estimates and bounds

# packages
library(here)
library(dplyr)

# functions
source(here("validation_metrics.R"))

#------------------------------------------------------------------------------#
# load INF fusion results
fused_inf <- nanoparquet::read_parquet(here("output/fused_inf.parquet"))

# calculate INF validation metrics (fusion level)
inf_val_level <- compute_val_metrics(
  data = fused_inf ,
  true_col_name = "pop_sh_true",
  pred_col_names = c("pop_sh", "pred_p50_global", "pred_p50_local"),
  lower_col_names = c("lower_bound", "pred_p025_global", "pred_p025_local"),
  upper_col_names = c("upper_bound", "pred_p975_global", "pred_p975_local"),
  group_vars = c("scenario", "sim_bias", "level", "mpm"),
  equal_weights = "code"
) |>
  # convert some validation metrics to percentage points
  mutate(across(c("mean_true", "mean_error", "mae", "mse", "rmse", "mpiw"), ~.x*100))

# save
nanoparquet::write_parquet(inf_val_level, here("output/fused_inf_val_level.parquet"))
haven::write_dta(inf_val_level, here("output/fused_inf_val_level.dta"))


# aggregate population rates to national level 
fused_inf_national <- fused_inf |>
  group_by(scenario, sim_bias, code, year, survname, level, mpm) |>
  summarise(across(c("pop_sh", "lower_bound", "upper_bound", 
                     "pred_p025_global","pred_p50_global", "pred_p975_global",
                     "pred_p025_local", "pred_p50_local", "pred_p975_local"),
                   ~ sum(.x*pop_sample)/sum(pop_sample)),
            pop = sum(pop_sample)) |>
  ungroup() |>
  rename(data_level = level) |>
  left_join(fused_inf |> filter(level == "National") |>
              select(scenario, sim_bias, code, year, survname, mpm, pop_sh_true))

# calculate INF validation metrics (national level)
inf_val_national <- compute_val_metrics(
  data = fused_inf_national ,
  true_col_name = "pop_sh_true",
  pred_col_names = c("pop_sh", "pred_p50_global", "pred_p50_local"),
  lower_col_names = c("lower_bound", "pred_p025_global", "pred_p025_local"),
  upper_col_names = c("upper_bound", "pred_p975_global", "pred_p975_local"),
  group_vars = c("scenario", "sim_bias", "data_level", "mpm"),
  equal_weights = "code"
) |>
  # convert some validation metrics to percentage points
  mutate(across(c("mean_true", "mean_error", "mae", "mse", "rmse", "mpiw"), ~.x*100))


# save
nanoparquet::write_parquet(inf_val_national, here("output/fused_inf_val_national.parquet"))
haven::write_dta(inf_val_national, here("output/fused_inf_val_national.dta"))
