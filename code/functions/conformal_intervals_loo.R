library(data.table)
library(Hmisc)

#' Leave-One-Out Local Normalized Conformal Prediction Intervals
#'
#' Computes conformal intervals using leave-one-out validation.
#' For each observation, intervals are calculated using validation data
#' that excludes observations with the same leave_out value.
#' Returns both global and local (if feature_cols specified) intervals.
#'
#' @param data_dt Data frame with true value, estimate, lower/upper bounds, and optional features
#' @param alpha Significance level (default = 0.05)
#' @param true_col_name Column name with true value (default = "true")
#' @param est_col_name Column name with estimate (default = "estimate")
#' @param lower_col_name Column name with lower bound (default = "lower_bound")
#' @param upper_col_name Column name with upper bound (default = "upper_bound")
#' @param feature_cols Character vector of feature columns for localization (default = NULL)
#' @param k Number of neighbors (default = sqrt(n) for local, n for global)
#' @param distance_metric "euclidean", "manhattan", or "mahalanobis" (default = "euclidean")
#' @param scale_features Scale features before distance computation (default = TRUE)
#' @param group_vars Character vector of grouping columns (default = NULL)
#' @param equal_weights Character vector for equal weighting (default = NULL)
#' @param leave_out Column name for leave-one-out grouping (default = NULL)
#'
#' @return data_dt with pred_p{percentile}_global/local, n_obs_global/local
#'
conformal_intervals_loo <- function(data_dt,
                                    alpha = 0.05,
                                    true_col_name = "true",
                                    est_col_name = "estimate",
                                    lower_col_name = "lower_bound",
                                    upper_col_name = "upper_bound",
                                    feature_cols = NULL,
                                    k = NULL,
                                    distance_metric = "euclidean",
                                    scale_features = TRUE,
                                    group_vars = NULL,
                                    equal_weights = NULL,
                                    leave_out = NULL) {
  
  # Convert to data.table
  data_dt <- copy(as.data.table(data_dt))
  
  # Input validation
  req_cols <- c(true_col_name, est_col_name, lower_col_name, upper_col_name)
  
  stopifnot(
    "Missing required columns" = all(req_cols %in% names(data_dt)),
    "Invalid alpha" = alpha > 0 && alpha < 1,
    "Invalid distance metric" = distance_metric %in% c("euclidean", "manhattan", "mahalanobis")
  )
  
  if (!is.null(group_vars)) {
    stopifnot("Missing group columns" = all(group_vars %in% names(data_dt)))
  }
  
  if (!is.null(equal_weights)) {
    stopifnot("Missing equal_weights columns" = all(equal_weights %in% names(data_dt)))
  }
  
  if (!is.null(leave_out)) {
    stopifnot("Missing leave_out column" = leave_out %in% names(data_dt))
  }
  
  # Compute normalized errors z for all data (done once)
  data_dt[, `:=`(
    err = get(true_col_name) - get(est_col_name),
    lw = get(est_col_name) - get(lower_col_name),
    uw = get(upper_col_name) - get(est_col_name)
  )]
  
  stopifnot(
    "Lower bounds exceed estimates" = all(data_dt$lw >= -1e-5, na.rm = TRUE),
    "Upper bounds below estimates" = all(data_dt$uw >= -1e-5, na.rm = TRUE)
  )
  
  data_dt[, z := fcase(
    err == 0, 0,
    err > 0 & uw > 0, err / uw,
    err < 0 & lw > 0, err / lw,
    default = NA_real_
  )]
  data_dt[, z := pmin(pmax(z, -1), 1)]
  
  # Compute weights (done once)
  if (!is.null(equal_weights)) {
    if (!is.null(group_vars)) {
      data_dt[, w := 1/.N, by = c(group_vars, equal_weights)]
      data_dt[, w := w / sum(w), by = group_vars]
    } else {
      data_dt[, w := 1/.N, by = equal_weights]
      data_dt[, w := w / sum(w)]
    }
  } else {
    data_dt[, w := if (!is.null(group_vars)) 1/.N else 1/.N]
  }
  
  # Determine if we should compute local intervals
  use_local <- !is.null(feature_cols) && length(feature_cols) > 0
  
  # Helper: compute weighted quantiles
  get_quantiles <- function(z, w, probs) {
    idx <- is.finite(z) & is.finite(w) & w > 0
    if (!any(idx)) return(rep(NA_real_, length(probs)))
    Hmisc::wtd.quantile(z[idx], weights = w[idx], probs = probs,
                        normwt = TRUE, na.rm = TRUE, type = "quantile")
  }
  
  probs <- c(alpha/2, 0.5, 1 - alpha/2)
  
  # Get unique leave_out values
  if (!is.null(leave_out)) {
    leave_out_vals <- unique(data_dt[[leave_out]])
    message(sprintf("Computing LOO intervals for %d unique %s values", 
                    length(leave_out_vals), leave_out))
  } else {
    leave_out_vals <- NA
    message("No leave_out specified - using all data for each prediction")
  }
  
  # === GLOBAL INTERVALS (ALWAYS COMPUTED) ===
  message(sprintf("Computing global %.1f%% conformal intervals (n=%d observations)", 
                  (1-alpha)*100, nrow(data_dt)))
  
  compute_global <- function(val_dt) {
    qs <- get_quantiles(val_dt$z, val_dt$w, probs)
    list(z_lower = qs[1], z_p50 = qs[2], z_upper = qs[3], n = nrow(val_dt))
  }
  
  # Initialize global result columns
  data_dt[, `:=`(z_lower_global = NA_real_, z_p50_global = NA_real_, 
                 z_upper_global = NA_real_, n_global = NA_integer_)]
  
  if (!is.null(group_vars)) {
    # Process each group
    groups <- unique(data_dt[, ..group_vars])
    
    for (i in seq_len(nrow(groups))) {
      grp <- groups[i]
      grp_idx <- rep(TRUE, nrow(data_dt))
      for (gv in group_vars) {
        grp_idx <- grp_idx & (data_dt[[gv]] == grp[[gv]])
      }
      
      if (!is.null(leave_out)) {
        # For each leave_out value in this group
        for (loo_val in leave_out_vals) {
          test_idx <- grp_idx & (data_dt[[leave_out]] == loo_val)
          val_idx <- grp_idx & (data_dt[[leave_out]] != loo_val)
          
          if (sum(test_idx) > 0 && sum(val_idx) > 0) {
            qs <- compute_global(data_dt[val_idx])
            data_dt[test_idx, `:=`(z_lower_global = qs$z_lower, z_p50_global = qs$z_p50, 
                                   z_upper_global = qs$z_upper, n_global = qs$n)]
          }
        }
      } else {
        # Use all data in group
        qs <- compute_global(data_dt[grp_idx])
        data_dt[grp_idx, `:=`(z_lower_global = qs$z_lower, z_p50_global = qs$z_p50, 
                              z_upper_global = qs$z_upper, n_global = qs$n)]
      }
    }
  } else {
    # No grouping
    if (!is.null(leave_out)) {
      for (loo_val in leave_out_vals) {
        test_idx <- data_dt[[leave_out]] == loo_val
        val_idx <- data_dt[[leave_out]] != loo_val
        
        if (sum(test_idx) > 0 && sum(val_idx) > 0) {
          qs <- compute_global(data_dt[val_idx])
          data_dt[test_idx, `:=`(z_lower_global = qs$z_lower, z_p50_global = qs$z_p50, 
                                 z_upper_global = qs$z_upper, n_global = qs$n)]
        }
      }
    } else {
      qs <- compute_global(data_dt)
      data_dt[, `:=`(z_lower_global = qs$z_lower, z_p50_global = qs$z_p50, 
                     z_upper_global = qs$z_upper, n_global = qs$n)]
    }
  }
  
  # === LOCAL INTERVALS (IF REQUESTED) ===
  if (use_local) {
    stopifnot(
      "Missing feature columns" = all(feature_cols %in% names(data_dt))
    )
    
    message(sprintf("Computing local %.1f%% conformal intervals using features", 
                    (1-alpha)*100))
    
    # Build feature matrix (done once)
    X_all <- as.matrix(data_dt[, ..feature_cols])
    storage.mode(X_all) <- "double"
    
    # Initialize local result columns
    data_dt[, `:=`(z_lower_local = NA_real_, z_p50_local = NA_real_, 
                   z_upper_local = NA_real_, n_local = NA_integer_)]
    data_dt[, .idx := .I]
    
    # Distance function
    compute_distances <- function(x_test, X_val, metric, cov_inv = NULL) {
      XT <- matrix(x_test, nrow = nrow(X_val), ncol = ncol(X_val), byrow = TRUE)
      diff <- X_val - XT
      switch(metric,
             euclidean = sqrt(rowSums(diff^2)),
             manhattan = rowSums(abs(diff)),
             mahalanobis = sqrt(rowSums((diff %*% cov_inv) * diff))
      )
    }
    
    if (!is.null(group_vars)) {
      # Process each group
      groups <- unique(data_dt[, ..group_vars])
      
      for (i in seq_len(nrow(groups))) {
        grp <- groups[i]
        grp_idx <- rep(TRUE, nrow(data_dt))
        for (gv in group_vars) {
          grp_idx <- grp_idx & (data_dt[[gv]] == grp[[gv]])
        }
        grp_data <- data_dt[grp_idx]
        
        if (nrow(grp_data) == 0) next
        
        if (!is.null(leave_out)) {
          # For each leave_out value in this group
          for (loo_val in leave_out_vals) {
            test_idx <- grp_data[[leave_out]] == loo_val
            val_idx <- grp_data[[leave_out]] != loo_val
            
            if (sum(test_idx) == 0 || sum(val_idx) == 0) next
            
            # Scale features using validation data
            X_val <- X_all[grp_data$.idx[val_idx], , drop = FALSE]
            X_test <- X_all[grp_data$.idx[test_idx], , drop = FALSE]
            
            if (scale_features) {
              means <- colMeans(X_val)
              sds <- apply(X_val, 2, sd)
              sds[!is.finite(sds) | sds == 0] <- 1
              X_val <- scale(X_val, center = means, scale = sds)
              X_test <- scale(X_test, center = means, scale = sds)
            }
            
            # Mahalanobis setup
            cov_inv <- NULL
            if (distance_metric == "mahalanobis") {
              cov_inv <- tryCatch(solve(cov(X_val)), error = function(e) NULL)
              if (is.null(cov_inv)) {
                warning("Failed to compute covariance inverse, using euclidean")
                distance_metric <- "euclidean"
              }
            }
            
            # Set k
            k_use <- if (is.null(k)) ceiling(sqrt(nrow(X_val))) else min(k, nrow(X_val))
            
            # For each test point
            for (j in seq_len(nrow(X_test))) {
              d <- compute_distances(X_test[j,], X_val, distance_metric, cov_inv)
              k_act <- min(k_use, length(d))
              nn <- order(d)[1:k_act]
              
              val_data <- grp_data[val_idx]
              w_nn <- if (!is.null(equal_weights)) val_data$w[nn] else rep(1/k_act, k_act)
              qs <- get_quantiles(val_data$z[nn], w_nn, probs)
              
              test_row_idx <- grp_data$.idx[test_idx][j]
              data_dt[test_row_idx, `:=`(z_lower_local = qs[1], z_p50_local = qs[2], 
                                         z_upper_local = qs[3], n_local = k_act)]
            }
          }
        } else {
          # Use all data in group
          X_grp <- X_all[grp_data$.idx, , drop = FALSE]
          
          if (scale_features) {
            means <- colMeans(X_grp)
            sds <- apply(X_grp, 2, sd)
            sds[!is.finite(sds) | sds == 0] <- 1
            X_grp <- scale(X_grp, center = means, scale = sds)
          }
          
          cov_inv <- NULL
          if (distance_metric == "mahalanobis") {
            cov_inv <- tryCatch(solve(cov(X_grp)), error = function(e) NULL)
          }
          
          k_use <- if (is.null(k)) ceiling(sqrt(nrow(X_grp))) else min(k, nrow(X_grp))
          
          for (j in seq_len(nrow(grp_data))) {
            d <- compute_distances(X_grp[j,], X_grp, distance_metric, cov_inv)
            k_act <- min(k_use, length(d))
            nn <- order(d)[1:k_act]
            
            w_nn <- if (!is.null(equal_weights)) grp_data$w[nn] else rep(1/k_act, k_act)
            qs <- get_quantiles(grp_data$z[nn], w_nn, probs)
            
            data_dt[grp_data$.idx[j], `:=`(z_lower_local = qs[1], z_p50_local = qs[2], 
                                           z_upper_local = qs[3], n_local = k_act)]
          }
        }
      }
    } else {
      # No grouping
      if (!is.null(leave_out)) {
        for (loo_val in leave_out_vals) {
          test_idx <- data_dt[[leave_out]] == loo_val
          val_idx <- data_dt[[leave_out]] != loo_val
          
          if (sum(test_idx) == 0 || sum(val_idx) == 0) next
          
          X_val <- X_all[val_idx, , drop = FALSE]
          X_test <- X_all[test_idx, , drop = FALSE]
          
          if (scale_features) {
            means <- colMeans(X_val)
            sds <- apply(X_val, 2, sd)
            sds[!is.finite(sds) | sds == 0] <- 1
            X_val <- scale(X_val, center = means, scale = sds)
            X_test <- scale(X_test, center = means, scale = sds)
          }
          
          cov_inv <- NULL
          if (distance_metric == "mahalanobis") {
            cov_inv <- tryCatch(solve(cov(X_val)), error = function(e) NULL)
          }
          
          k_use <- if (is.null(k)) ceiling(sqrt(nrow(X_val))) else min(k, nrow(X_val))
          
          val_data <- data_dt[val_idx]
          test_rows <- which(test_idx)
          
          for (j in seq_len(nrow(X_test))) {
            d <- compute_distances(X_test[j,], X_val, distance_metric, cov_inv)
            k_act <- min(k_use, length(d))
            nn <- order(d)[1:k_act]
            
            w_nn <- if (!is.null(equal_weights)) val_data$w[nn] else rep(1/k_act, k_act)
            qs <- get_quantiles(val_data$z[nn], w_nn, probs)
            
            data_dt[test_rows[j], `:=`(z_lower_local = qs[1], z_p50_local = qs[2], 
                                       z_upper_local = qs[3], n_local = k_act)]
          }
        }
      } else {
        X_scaled <- X_all
        if (scale_features) {
          means <- colMeans(X_all)
          sds <- apply(X_all, 2, sd)
          sds[!is.finite(sds) | sds == 0] <- 1
          X_scaled <- scale(X_all, center = means, scale = sds)
        }
        
        cov_inv <- NULL
        if (distance_metric == "mahalanobis") {
          cov_inv <- tryCatch(solve(cov(X_scaled)), error = function(e) NULL)
        }
        
        k_use <- if (is.null(k)) ceiling(sqrt(nrow(X_scaled))) else min(k, nrow(X_scaled))
        
        for (i in seq_len(nrow(data_dt))) {
          d <- compute_distances(X_scaled[i,], X_scaled, distance_metric, cov_inv)
          k_act <- min(k_use, length(d))
          nn <- order(d)[1:k_act]
          
          w_nn <- if (!is.null(equal_weights)) data_dt$w[nn] else rep(1/k_act, k_act)
          qs <- get_quantiles(data_dt$z[nn], w_nn, probs)
          
          data_dt[i, `:=`(z_lower_local = qs[1], z_p50_local = qs[2], 
                          z_upper_local = qs[3], n_local = k_act)]
        }
      }
    }
    
    data_dt[, .idx := NULL]
    
    avg_n <- round(mean(data_dt$n_local, na.rm = TRUE))
    message(sprintf("Computed localized %.1f%% intervals using k=%d neighbors (avg)", 
                    (1-alpha)*100, avg_n))
  }
  
  # Apply thresholds to generate predictions with proper percentile formatting
  # Format percentile as 3 digits (e.g., 025, 050, 975)
  lower_percentile <- (alpha/2) * 100
  upper_percentile <- (1 - alpha/2) * 100
  
  # Determine number of decimal places needed
  if (lower_percentile == floor(lower_percentile)) {
    lower_str <- sprintf("%02d", as.integer(lower_percentile))
    upper_str <- sprintf("%02d", as.integer(upper_percentile))
  } else if (lower_percentile * 10 == floor(lower_percentile * 10)) {
    lower_str <- sprintf("%04.1f", lower_percentile)
    upper_str <- sprintf("%04.1f", upper_percentile)
    lower_str <- gsub("\\.", "", lower_str)
    upper_str <- gsub("\\.", "", upper_str)
  } else {
    lower_str <- sprintf("%05.2f", lower_percentile)
    upper_str <- sprintf("%05.2f", upper_percentile)
    lower_str <- gsub("\\.", "", lower_str)
    upper_str <- gsub("\\.", "", upper_str)
  }
  
  # Global predictions
  lower_col_global <- sprintf("pred_p%s_global", lower_str)
  upper_col_global <- sprintf("pred_p%s_global", upper_str)
  
  data_dt[, pred_p50_global := pmax(0, pmin(1, get(est_col_name) + fifelse(z_p50_global <= 0, lw * z_p50_global, uw * z_p50_global)))]
  data_dt[, (lower_col_global) := pmax(0, get(est_col_name) + lw * z_lower_global)]
  data_dt[, (upper_col_global) := pmin(1, get(est_col_name) + uw * z_upper_global)]
  data_dt[, n_obs_global := n_global]
  
  # Local predictions (if computed)
  if (use_local) {
    lower_col_local <- sprintf("pred_p%s_local", lower_str)
    upper_col_local <- sprintf("pred_p%s_local", upper_str)
    
    data_dt[, pred_p50_local := pmax(0, pmin(1, get(est_col_name) + fifelse(z_p50_local <= 0, lw * z_p50_local, uw * z_p50_local)))]
    data_dt[, (lower_col_local) := pmax(0, get(est_col_name) + lw * z_lower_local)]
    data_dt[, (upper_col_local) := pmin(1, get(est_col_name) + uw * z_upper_local)]
    data_dt[, n_obs_local := n_local]
  }
  
  # Return clean output
  cols_to_remove <- c("z_lower_global", "z_p50_global", "z_upper_global", "n_global",
                      "lw", "uw", "err", "z", "w")
  if (use_local) {
    cols_to_remove <- c(cols_to_remove, "z_lower_local", "z_p50_local", "z_upper_local", "n_local")
  }
  data_dt[, (cols_to_remove) := NULL]
  
  return(data_dt[])
}