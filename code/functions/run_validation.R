#' Run the validation pipeline for fused poverty estimates
#'
#' Loads fused results, computes weighted validation metrics at the fusion level
#' and aggregated to national level, and saves outputs as parquet files.
#'
#' @param input_file Path (relative to project root) to the input parquet file.
#' @param output_prefix File name prefix for output files (e.g., "fused_mpm").
#' @param group_vars Character vector of survey grouping columns (code, year, survname, level).
#' @param equal_weights Column name for equal-weighting (e.g., "code").
#'
#' @return NULL (invisibly). Side effect: writes parquet files to output/.
run_validation <- function(input_file, output_prefix, group_vars, equal_weights) {

  fused <- as.data.table(nanoparquet::read_parquet(here(input_file)))

  scale_cols <- c("mean_true", "mean_error", "mae", "mse", "rmse", "mpiw")

  # calculate validation metrics (fusion level)
  val_level <- as.data.table(compute_val_metrics(
    data = fused,
    true_col_name  = "pop_sh_true",
    pred_col_names = c("pop_sh", "pred_p50_global", "pred_p50_local"),
    lower_col_names = c("lower_bound", "pred_p025_global", "pred_p025_local"),
    upper_col_names = c("upper_bound", "pred_p975_global", "pred_p975_local"),
    group_vars     = c("scenario", "sim_bias", "level", "mpm"),
    equal_weights  = equal_weights
  ))
  val_level[, (scale_cols) := lapply(.SD, function(x) x * 100), .SDcols = scale_cols]

  nanoparquet::write_parquet(val_level, here(paste0("output/", output_prefix, "_val_level.parquet")))

  # aggregate population rates to national level
  agg_cols <- c("pop_sh", "lower_bound", "upper_bound",
                "pred_p025_global", "pred_p50_global", "pred_p975_global",
                "pred_p025_local",  "pred_p50_local",  "pred_p975_local")
  by_cols <- c("scenario", "sim_bias", group_vars, "mpm")

  fused_national <- fused[,
    c(lapply(.SD, function(x) sum(x * pop_sample) / sum(pop_sample)),
      list(pop = sum(pop_sample))),
    by  = by_cols,
    .SDcols = agg_cols
  ]
  setnames(fused_national, "level", "data_level")

  true_national <- fused[level == "National",
    .(scenario, sim_bias, code, year, survname, mpm, pop_sh_true)]
  fused_national <- merge(fused_national, true_national,
    by = c("scenario", "sim_bias", "code", "year", "survname", "mpm"), all.x = TRUE)

  # calculate validation metrics (national level)
  val_national <- as.data.table(compute_val_metrics(
    data = fused_national,
    true_col_name  = "pop_sh_true",
    pred_col_names = c("pop_sh", "pred_p50_global", "pred_p50_local"),
    lower_col_names = c("lower_bound", "pred_p025_global", "pred_p025_local"),
    upper_col_names = c("upper_bound", "pred_p975_global", "pred_p975_local"),
    group_vars     = c("scenario", "sim_bias", "data_level", "mpm"),
    equal_weights  = equal_weights
  ))
  val_national[, (scale_cols) := lapply(.SD, function(x) x * 100), .SDcols = scale_cols]

  nanoparquet::write_parquet(val_national, here(paste0("output/", output_prefix, "_val_national.parquet")))
}
