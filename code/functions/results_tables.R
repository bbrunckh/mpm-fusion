#' Create and save a formatted summary Excel workbook
#'
#' Generates a workbook with one sheet per data level, showing MAE and MPIW
#' for each estimation target, grouped by MPM metric and scenario.
#'
#' @param dt_bias1 data.table of validation metrics filtered to sim_bias == 1.
#' @param output_path Path (relative to project root) for the output xlsx file.
#'
#' @return NULL (invisibly). Side effect: writes an xlsx file.
create_summary_workbook <- function(dt_bias1, output_path) {

  wb_summary <- createWorkbook()

  data_levels <- unique(dt_bias1$data_level)

  for (level in data_levels) {
    dt_level <- dt_bias1[data_level == level]

    targets <- unique(dt_level$target)

    # pivot mae and mpiw to wide format
    mae_wide <- dcast(dt_level[, .(mpm, scenario, target, mae)],
                      mpm + scenario ~ target, value.var = "mae")
    setnames(mae_wide,
             setdiff(names(mae_wide), c("mpm", "scenario")),
             paste0("mae_", setdiff(names(mae_wide), c("mpm", "scenario"))))

    mpiw_wide <- dcast(dt_level[, .(mpm, scenario, target, mpiw)],
                       mpm + scenario ~ target, value.var = "mpiw")
    setnames(mpiw_wide,
             setdiff(names(mpiw_wide), c("mpm", "scenario")),
             paste0("mpiw_", setdiff(names(mpiw_wide), c("mpm", "scenario"))))

    n_data <- dt_level[, .(N = N[1L]), by = .(mpm, scenario)]

    summary_table <- merge(mae_wide,  mpiw_wide, by = c("mpm", "scenario"), all.x = TRUE)
    summary_table <- merge(summary_table, n_data, by = c("mpm", "scenario"), all.x = TRUE)
    setorder(summary_table, mpm, scenario)
    summary_table[, mpm_scenario := scenario]
    summary_table[, scenario := NULL]
    setcolorder(summary_table, c("mpm", "mpm_scenario"))

    numeric_cols <- sapply(summary_table, is.numeric)
    cols_to_format <- setdiff(names(summary_table)[numeric_cols], "N")
    summary_table[, (cols_to_format) := lapply(.SD, round, 2), .SDcols = cols_to_format]

    addWorksheet(wb_summary, level)

    mae_cols  <- grep("^mae_",  names(summary_table))
    mpiw_cols <- grep("^mpiw_", names(summary_table))

    writeData(wb_summary, level, "MPM",  startCol = 1,                      startRow = 1)
    writeData(wb_summary, level, "MAE",  startCol = 2,                      startRow = 1)
    writeData(wb_summary, level, "MPIW", startCol = 2 + length(targets),    startRow = 1)
    writeData(wb_summary, level, "N",    startCol = 2 + 2*length(targets),  startRow = 1)

    writeData(wb_summary, level, "MPM", startCol = 1, startRow = 2)
    for (i in seq_along(targets)) {
      writeData(wb_summary, level, targets[i], startCol = 1 + i,                startRow = 2)
      writeData(wb_summary, level, targets[i], startCol = 1 + length(targets) + i, startRow = 2)
    }
    writeData(wb_summary, level, "n", startCol = 2 + 2*length(targets), startRow = 2)

    current_row <- 3
    mpms_in_table <- unique(summary_table$mpm)

    for (mpm_val in mpms_in_table) {
      mpm_data <- summary_table[mpm == mpm_val]

      writeData(wb_summary, level, mpm_val, startCol = 1, startRow = current_row)

      bold_style <- createStyle(textDecoration = "bold")
      addStyle(wb_summary, level, bold_style, rows = current_row, cols = 1,
               gridExpand = FALSE, stack = TRUE)

      current_row <- current_row + 1

      for (j in 1:nrow(mpm_data)) {
        writeData(wb_summary, level, paste0("  ", mpm_data$mpm_scenario[j]),
                  startCol = 1, startRow = current_row)

        data_cols <- mpm_data[j, -c(1, 2)]
        writeData(wb_summary, level, data_cols, startCol = 2, startRow = current_row, colNames = FALSE)

        current_row <- current_row + 1
      }
    }

    mergeCells(wb_summary, level, cols = 2:(1 + length(targets)), rows = 1)
    mergeCells(wb_summary, level, cols = (2 + length(targets)):(1 + 2*length(targets)), rows = 1)
    mergeCells(wb_summary, level, cols = (2 + 2*length(targets)), rows = 1:2)
    mergeCells(wb_summary, level, cols = 1, rows = 1:2)

    header_style <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                                border = "TopBottomLeftRight", borderStyle = "thin")
    addStyle(wb_summary, level, header_style, rows = 1:2, cols = 1:(2 + 2*length(targets)),
             gridExpand = TRUE, stack = TRUE)

    for (col_idx in 2:(ncol(summary_table) - 1)) {
      addStyle(wb_summary, level,
               style = createStyle(numFmt = "0.00"),
               rows = 3:(current_row - 1), cols = col_idx,
               gridExpand = TRUE, stack = TRUE)
    }

    n_col_position <- ncol(summary_table) - 1
    addStyle(wb_summary, level,
             style = createStyle(numFmt = "0"),
             rows = 3:(current_row - 1), cols = n_col_position,
             gridExpand = TRUE, stack = TRUE)

    setColWidths(wb_summary, level, cols = 1:ncol(summary_table), widths = 15)
  }

  saveWorkbook(wb_summary, here(output_path), overwrite = TRUE)
}


#' Create and save a formatted extended Excel workbook
#'
#' Generates a workbook with one sheet per MPM metric, showing all validation
#' metrics for each estimation target, grouped by data level and scenario.
#'
#' @param dt_bias1 data.table of validation metrics filtered to sim_bias == 1.
#' @param output_path Path (relative to project root) for the output xlsx file.
#'
#' @return NULL (invisibly). Side effect: writes an xlsx file.
create_extended_workbook <- function(dt_bias1, output_path) {

  wb_extended <- createWorkbook()

  mpms        <- unique(dt_bias1$mpm)
  metric_cols <- c("bias", "mae", "mape", "mse", "rmse", "r", "rho", "mpiw")

  metric_labels <- c(
    bias = "Bias", mae = "MAE", mape = "MAPE", mse = "MSE",
    rmse = "RMSE", r = "r", rho = "\u03c1", mpiw = "MPIW"
  )

  # styles for metric headers
  header_style <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                              border = "TopBottomLeftRight", borderStyle = "thin")
  r_header_style <- createStyle(textDecoration = c("bold", "italic"), halign = "center",
                                valign = "center", border = "TopBottomLeftRight",
                                borderStyle = "thin")

  for (mpm_val in mpms) {
    dt_mpm  <- dt_bias1[mpm == mpm_val]
    targets <- unique(dt_mpm$target)

    # extract N before pivoting (constant within data_level × scenario)
    n_data <- dt_mpm[, .(N = N[1L]), by = .(data_level, scenario)]

    # pivot all metrics wide by target; dcast produces "metric_target" columns
    keep_cols <- c("data_level", "scenario", "target", metric_cols)
    wide_dt <- dcast(
      dt_mpm[, ..keep_cols],
      data_level + scenario ~ target,
      value.var = metric_cols
    )

    # dcast names columns "metric_target"; rename to "target_metric"
    orig <- setdiff(names(wide_dt), c("data_level", "scenario"))
    renamed <- sub("^(.+?)_(.+)$", "\\2_\\1", orig)
    setnames(wide_dt, orig, renamed)

    # reorder so columns are grouped by target
    tgt_cols <- unlist(lapply(targets, function(t) {
      nms <- names(wide_dt)
      nms[startsWith(nms, paste0(t, "_"))]
    }))
    setcolorder(wide_dt, c("data_level", "scenario", tgt_cols))
    setorder(wide_dt, data_level, scenario)

    # join N at the end
    wide_dt <- merge(wide_dt, n_data, by = c("data_level", "scenario"), sort = FALSE)

    extended_table <- wide_dt
    extended_table[, level_scenario := scenario]
    extended_table[, scenario := NULL]
    setcolorder(extended_table, c("data_level", "level_scenario"))

    all_cols <- names(extended_table)

    metrics_2dp <- c("mae", "mape", "mse", "rmse", "mpiw")
    pattern_2dp <- paste0("_(", paste(metrics_2dp, collapse = "|"), ")$")
    cols_2dp <- all_cols[grepl(pattern_2dp, all_cols)]

    metrics_3dp <- c("bias", "r", "rho")
    pattern_3dp <- paste0("_(", paste(metrics_3dp, collapse = "|"), ")$")
    cols_3dp <- all_cols[grepl(pattern_3dp, all_cols)]

    if (length(cols_2dp) > 0)
      extended_table[, (cols_2dp) := lapply(.SD, round, 2), .SDcols = cols_2dp]
    if (length(cols_3dp) > 0)
      extended_table[, (cols_3dp) := lapply(.SD, round, 3), .SDcols = cols_3dp]

    sheet_name <- substr(gsub("[^A-Za-z0-9]", "_", mpm_val), 1, 31)
    addWorksheet(wb_extended, sheet_name)

    start_col  <- 2
    writeData(wb_extended, sheet_name, "Level", startCol = 1, startRow = 1)

    for (i in seq_along(targets)) {
      target          <- targets[i]
      target_start_col <- start_col + (i - 1) * length(metric_cols)
      target_end_col   <- target_start_col + length(metric_cols) - 1

      writeData(wb_extended, sheet_name, target, startCol = target_start_col, startRow = 1)
      mergeCells(wb_extended, sheet_name, cols = target_start_col:target_end_col, rows = 1)

      for (j in seq_along(metric_cols)) {
        metric_col <- target_start_col + j - 1
        writeData(wb_extended, sheet_name, metric_labels[metric_cols[j]], startCol = metric_col, startRow = 2)
      }
    }

    n_col_idx <- start_col + length(targets) * length(metric_cols)
    writeData(wb_extended, sheet_name, "N", startCol = n_col_idx, startRow = 1)
    mergeCells(wb_extended, sheet_name, cols = n_col_idx, rows = 1:2)
    mergeCells(wb_extended, sheet_name, cols = 1, rows = 1:2)

    current_row <- 3
    levels_in_table <- unique(extended_table$data_level)

    for (level_val in levels_in_table) {
      level_data <- extended_table[data_level == level_val]

      writeData(wb_extended, sheet_name, level_val, startCol = 1, startRow = current_row)

      bold_style <- createStyle(textDecoration = "bold")
      addStyle(wb_extended, sheet_name, bold_style, rows = current_row, cols = 1,
               gridExpand = FALSE, stack = TRUE)

      current_row <- current_row + 1

      for (j in 1:nrow(level_data)) {
        writeData(wb_extended, sheet_name, paste0("  ", level_data$level_scenario[j]),
                  startCol = 1, startRow = current_row)

        data_cols <- level_data[j, -c(1, 2)]
        writeData(wb_extended, sheet_name, data_cols, startCol = 2, startRow = current_row, colNames = FALSE)

        current_row <- current_row + 1
      }
    }

    addStyle(wb_extended, sheet_name, header_style, rows = 1:2, cols = 1:n_col_idx,
             gridExpand = TRUE, stack = TRUE)

    # re-apply italic style to all "r" header cells (one per target)
    for (i in seq_along(targets)) {
      r_col_position <- start_col + (i - 1) * length(metric_cols) + which(metric_cols == "r") - 1
      addStyle(wb_extended, sheet_name, r_header_style,
               rows = 2, cols = r_col_position, stack = FALSE)
    }

    for (i in seq_along(targets)) {
      target_start_col <- start_col + (i - 1) * length(metric_cols)

      for (j in seq_along(metric_cols)) {
        metric      <- metric_cols[j]
        col_position <- target_start_col + j - 1

        if (metric %in% metrics_2dp) {
          addStyle(wb_extended, sheet_name,
                   style = createStyle(numFmt = "0.00"),
                   rows = 3:(current_row - 1), cols = col_position,
                   gridExpand = TRUE, stack = TRUE)
        } else if (metric %in% metrics_3dp) {
          addStyle(wb_extended, sheet_name,
                   style = createStyle(numFmt = "0.000"),
                   rows = 3:(current_row - 1), cols = col_position,
                   gridExpand = TRUE, stack = TRUE)
        }
      }
    }

    addStyle(wb_extended, sheet_name,
             style = createStyle(numFmt = "0"),
             rows = 3:(current_row - 1), cols = n_col_idx,
             gridExpand = TRUE, stack = TRUE)

    setColWidths(wb_extended, sheet_name, cols = 2:n_col_idx, widths = "5.5")
  }

  saveWorkbook(wb_extended, here(output_path), overwrite = TRUE)
}
