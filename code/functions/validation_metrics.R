#' Compute prediction metrics for multiple targets by group
#'
#' @param data A data.frame or data.table with actual, predicted, and grouping columns.
#' @param true_col_name Character. Name of the column with true values.
#' @param pred_col_names Character vector. Names of one or more columns with predicted values.
#' @param lower_col_names Character vector. Names of columns with lower bounds (same order as pred_col_names). Default = NULL.
#' @param upper_col_names Character vector. Names of columns with upper bounds (same order as pred_col_names). Default = NULL.
#' @param group_vars Character vector of grouping columns (default = NULL).
#' @param equal_weights Character vector for equal weighting (default = NULL).
#' @param mape_zero_handling One of "exclude" (default) or "epsilon".
#' @param mape_epsilon Numeric. Small value to stabilize division for MAPE.
#'
#' @return A data.table with one row per group and per target, containing:
#' target, n, mean_true, mean_error, bias, mae, mape, mse, rmse, r, rho, mpiw (if bounds provided)

compute_val_metrics <- function(data,
                                true_col_name,
                                pred_col_names,
                                lower_col_names = NULL,
                                upper_col_names = NULL,
                                group_vars = NULL,
                                equal_weights = NULL,
                                mape_zero_handling = c("exclude", "epsilon"),
                                mape_epsilon = 1e-8) {
  # --- Input Validation ---
  stopifnot(is.data.frame(data))
  stopifnot(is.character(true_col_name), length(true_col_name) == 1)
  stopifnot(is.character(pred_col_names), length(pred_col_names) > 0)
  stopifnot(all(c(true_col_name, pred_col_names, group_vars) %in% names(data)))
  
  # Validate bounds
  has_bounds <- !is.null(lower_col_names) || !is.null(upper_col_names)
  if (has_bounds) {
    if (is.null(lower_col_names) || is.null(upper_col_names)) {
      stop("Both lower_col_names and upper_col_names must be provided together")
    }
    stopifnot(length(lower_col_names) == length(pred_col_names))
    stopifnot(length(upper_col_names) == length(pred_col_names))
    stopifnot(all(c(lower_col_names, upper_col_names) %in% names(data)))
  }
  
  if (!is.null(equal_weights)) {
    stopifnot(is.character(equal_weights))
    stopifnot(all(equal_weights %in% names(data)))
  }
  
  mape_zero_handling <- match.arg(mape_zero_handling)
  
  # Convert to data.table
  dt <- as.data.table(data)
  
  # --- Helper function to compute metrics ---
  compute_metrics <- function(a, p, w, lower = NULL, upper = NULL) {
    # Keep only finite pairs
    idx <- is.finite(a) & is.finite(p) & is.finite(w)
    
    # Also check bounds if provided
    if (!is.null(lower) && !is.null(upper)) {
      idx <- idx & is.finite(lower) & is.finite(upper)
    }
    
    a <- a[idx]
    p <- p[idx]
    w <- w[idx]
    
    if (!is.null(lower) && !is.null(upper)) {
      lower <- lower[idx]
      upper <- upper[idx]
    }
    
    n <- length(a)
    if (n == 0L || sum(w) == 0) {
      base_result <- list(
        n = 0L, mean_true = NA_real_, mean_error = NA_real_, bias = NA_real_, 
        mae = NA_real_, mape = NA_real_, mse = NA_real_, rmse = NA_real_, 
        r = NA_real_, rho = NA_real_
      )
      
      if (!is.null(lower) && !is.null(upper)) {
        base_result$mpiw <- NA_real_
        base_result$picp <- NA_real_
      }
      
      return(base_result)
    }
    
    # Error
    err <- p - a
    
    # Weighted mean
    wmean <- function(x, w) sum(x * w) / sum(w)
    
    # Mean true value
    mean_true <- wmean(a, w)
    
    # Mean error
    mean_error <- wmean(err, w)
    
    # Bias
    mean_a <- wmean(a, w)
    bias <- if (mean_a == 0) NA_real_ else (wmean(p, w) / mean_a - 1)
    
    # MAE
    mae <- wmean(abs(err), w)
    
    # MAPE
    if (mape_zero_handling == "exclude") {
      denom_ok <- a != 0
      mape <- if (any(denom_ok)) {
        wmean(abs(err[denom_ok] / a[denom_ok]), w[denom_ok]) * 100
      } else NA_real_
    } else {
      mape <- wmean(abs(err) / pmax(abs(a), mape_epsilon), w) * 100
    }
    
    # MSE and RMSE
    mse <- wmean(err^2, w)
    rmse <- sqrt(mse)
    
    # Correlations
    r <- if (n > 1 && sd(a) > 0 && sd(p) > 0) {
      cor(a, p, method = "pearson")
    } else NA_real_
    
    rho <- if (n > 1 && sd(a) > 0 && sd(p) > 0) {
      cor(a, p, method = "spearman")
    } else NA_real_
    
    result <- list(
      n = n, mean_true = mean_true, mean_error = mean_error, bias = bias, 
      mae = mae, mape = mape, mse = mse, rmse = rmse, r = r, rho = rho
    )
    
    # Compute prediction interval metrics if bounds provided
    if (!is.null(lower) && !is.null(upper)) {
      # Mean Prediction Interval Width (weighted)
      interval_width <- abs(upper - lower)
      mpiw <- wmean(interval_width, w)

      result$mpiw <- mpiw
    }
    
    result
  }
  
  # --- Add weights ---
  if (!is.null(equal_weights)) {
    # Count observations in each subgroup
    dt[, .n_in_subgroup := .N, by = c(group_vars, equal_weights)]
    # Count number of subgroups
    dt[, .n_subgroups := uniqueN(.SD), by = group_vars, .SDcols = equal_weights]
    # Calculate weight
    dt[, .weight := (1 / .n_subgroups) / .n_in_subgroup]
  } else {
    dt[, .weight := 1]
  }
  
  # --- Compute metrics for each prediction column ---
  results_list <- lapply(seq_along(pred_col_names), function(i) {
    pred_col <- pred_col_names[i]
    lower_col <- if (has_bounds) lower_col_names[i] else NULL
    upper_col <- if (has_bounds) upper_col_names[i] else NULL
    
    if (is.null(group_vars)) {
      # No grouping - compute for entire dataset
      metrics <- compute_metrics(
        dt[[true_col_name]], 
        dt[[pred_col]], 
        dt$.weight,
        if (!is.null(lower_col)) dt[[lower_col]] else NULL,
        if (!is.null(upper_col)) dt[[upper_col]] else NULL
      )
      result_dt <- as.data.table(metrics)
      result_dt[, target := pred_col]
    } else {
      # Group by specified variables
      result_dt <- dt[, {
        metrics <- compute_metrics(
          .SD[[true_col_name]], 
          .SD[[pred_col]], 
          .weight,
          if (!is.null(lower_col)) .SD[[lower_col]] else NULL,
          if (!is.null(upper_col)) .SD[[upper_col]] else NULL
        )
        as.data.table(metrics)
      }, by = group_vars]
      result_dt[, target := pred_col]
    }
    
    # Reorder columns to have target first
    setcolorder(result_dt, c("target", setdiff(names(result_dt), "target")))
    result_dt
  })
  
  # Combine all results
  results <- rbindlist(results_list)
  
  return(results)
}