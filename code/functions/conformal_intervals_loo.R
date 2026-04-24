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
#' @note Local intervals use dbscan::kNN for Euclidean and Mahalanobis (via Cholesky
#'   pre-whitening). Manhattan distance uses the same Euclidean kd-tree as an
#'   approximation; exact L1 KNN is not efficiently supported at large scale.
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

  data_dt <- copy(as.data.table(data_dt))

  req_cols <- c(true_col_name, est_col_name, lower_col_name, upper_col_name)
  stopifnot(
    "Missing required columns"  = all(req_cols %in% names(data_dt)),
    "Invalid alpha"             = alpha > 0 && alpha < 1,
    "Invalid distance metric"   = distance_metric %in% c("euclidean", "manhattan", "mahalanobis")
  )
  if (!is.null(group_vars))    stopifnot("Missing group columns"          = all(group_vars    %in% names(data_dt)))
  if (!is.null(equal_weights)) stopifnot("Missing equal_weights columns"  = all(equal_weights %in% names(data_dt)))
  if (!is.null(leave_out))     stopifnot("Missing leave_out column"       = leave_out %in% names(data_dt))

  # Compute normalised errors (done once)
  data_dt[, `:=`(
    err = get(true_col_name) - get(est_col_name),
    lw  = get(est_col_name)  - get(lower_col_name),
    uw  = get(upper_col_name) - get(est_col_name)
  )]
  stopifnot(
    "Lower bounds exceed estimates" = all(data_dt$lw >= -1e-5, na.rm = TRUE),
    "Upper bounds below estimates"  = all(data_dt$uw >= -1e-5, na.rm = TRUE)
  )
  data_dt[, z := fcase(
    err == 0,           0,
    err > 0 & uw > 0,   err / uw,
    err < 0 & lw > 0,   err / lw,
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
    data_dt[, w := 1/.N]
  }

  use_local <- !is.null(feature_cols) && length(feature_cols) > 0

  # Weighted quantile: single sort pass, no Hmisc dependency
  get_quantiles <- function(z, w, probs) {
    idx <- is.finite(z) & is.finite(w) & w > 0
    if (!any(idx)) return(rep(NA_real_, length(probs)))
    z <- z[idx]; w <- w[idx]
    w <- w / sum(w)
    o    <- order(z)
    cumw <- cumsum(w[o])
    vapply(probs, function(p) z[o][which(cumw >= p)[1L]], numeric(1))
  }

  # Fast path for uniform weights (kNN case when equal_weights = NULL)
  get_quantiles_uniform <- function(z, probs) {
    quantile(z[is.finite(z)], probs = probs, names = FALSE)
  }

  probs <- c(alpha / 2, 0.5, 1 - alpha / 2)

  # Accept z/w vectors directly — avoids copying the full data.table per LOO iteration
  compute_global <- function(z, w) {
    qs <- get_quantiles(z, w, probs)
    list(z_lower = qs[1], z_p50 = qs[2], z_upper = qs[3], n = length(z))
  }

  # Precompute leave_out vector once to avoid repeated column access in loops
  if (!is.null(leave_out)) {
    leave_out_vec  <- data_dt[[leave_out]]
    leave_out_vals <- unique(leave_out_vec)
    message(sprintf("Computing LOO intervals for %d unique %s values",
                    length(leave_out_vals), leave_out))
  } else {
    leave_out_vec  <- NULL
    leave_out_vals <- NA
    message("No leave_out specified - using all data for each prediction")
  }

  message(sprintf("Computing global %.1f%% conformal intervals (n=%d observations)",
                  (1 - alpha) * 100, nrow(data_dt)))

  # Precompute z/w vectors once to avoid repeated $-access inside loops
  z_vec <- data_dt$z
  w_vec <- data_dt$w

  data_dt[, `:=`(z_lower_global = NA_real_, z_p50_global = NA_real_,
                 z_upper_global = NA_real_, n_global     = NA_integer_)]

  # === GLOBAL INTERVALS ===
  if (!is.null(group_vars)) {
    groups <- unique(data_dt[, ..group_vars])
    for (i in seq_len(nrow(groups))) {
      grp     <- groups[i]
      grp_idx <- rep(TRUE, nrow(data_dt))
      for (gv in group_vars) grp_idx <- grp_idx & (data_dt[[gv]] == grp[[gv]])

      if (!is.null(leave_out)) {
        for (loo_val in leave_out_vals) {
          test_idx <- grp_idx & (leave_out_vec == loo_val)
          val_idx  <- grp_idx & (leave_out_vec != loo_val)
          if (sum(test_idx) == 0 || sum(val_idx) == 0) next
          qs <- compute_global(z_vec[val_idx], w_vec[val_idx])
          data_dt[test_idx, `:=`(z_lower_global = qs$z_lower, z_p50_global = qs$z_p50,
                                 z_upper_global = qs$z_upper, n_global      = qs$n)]
        }
      } else {
        qs <- compute_global(z_vec[grp_idx], w_vec[grp_idx])
        data_dt[grp_idx, `:=`(z_lower_global = qs$z_lower, z_p50_global = qs$z_p50,
                              z_upper_global = qs$z_upper, n_global      = qs$n)]
      }
    }
  } else {
    if (!is.null(leave_out)) {
      for (loo_val in leave_out_vals) {
        test_idx <- leave_out_vec == loo_val
        val_idx  <- leave_out_vec != loo_val
        if (sum(test_idx) == 0 || sum(val_idx) == 0) next
        qs <- compute_global(z_vec[val_idx], w_vec[val_idx])
        data_dt[test_idx, `:=`(z_lower_global = qs$z_lower, z_p50_global = qs$z_p50,
                               z_upper_global = qs$z_upper, n_global      = qs$n)]
      }
    } else {
      qs <- compute_global(z_vec, w_vec)
      data_dt[, `:=`(z_lower_global = qs$z_lower, z_p50_global = qs$z_p50,
                     z_upper_global = qs$z_upper, n_global      = qs$n)]
    }
  }

  # === LOCAL INTERVALS ===
  if (use_local) {
    stopifnot("Missing feature columns" = all(feature_cols %in% names(data_dt)))
    message(sprintf("Computing local %.1f%% conformal intervals using features",
                    (1 - alpha) * 100))

    X_all <- as.matrix(data_dt[, ..feature_cols])
    storage.mode(X_all) <- "double"

    data_dt[, `:=`(z_lower_local = NA_real_, z_p50_local = NA_real_,
                   z_upper_local = NA_real_, n_local     = NA_integer_)]
    data_dt[, .idx := .I]

    # Scale features and optionally pre-whiten for Mahalanobis.
    # Mahalanobis = Euclidean after X %*% t(chol(solve(cov(X_val)))).
    # Manhattan: kd-tree only supports Euclidean; Euclidean used as approximation.
    prep_knn_matrices <- function(X_val, X_test, metric) {
      if (scale_features) {
        means <- colMeans(X_val)
        sds   <- apply(X_val, 2, sd)
        sds[!is.finite(sds) | sds == 0] <- 1
        X_val  <- scale(X_val,  center = means, scale = sds)
        X_test <- scale(X_test, center = means, scale = sds)
      }
      if (metric == "mahalanobis") {
        W <- tryCatch(chol(solve(cov(X_val))), error = function(e) NULL)
        if (!is.null(W)) {
          X_val  <- X_val  %*% t(W)
          X_test <- X_test %*% t(W)
        } else {
          warning("Failed to compute Cholesky of covariance inverse, using euclidean")
        }
      }
      list(X_val = unname(X_val), X_test = unname(X_test))
    }

    # Run kNN via dbscan for all test points at once; bulk-assign results.
    # sort = FALSE: skip distance sorting since we only need indices for quantiles.
    run_local_knn <- function(X_val, X_test, z_val, w_val, k_use) {
      nn <- dbscan::kNN(x = X_val, query = X_test, k = k_use, sort = FALSE)

      if (is.null(equal_weights)) {
        # Bulk-index all neighbour z-values into an n_test × k matrix, then quantile row-wise
        z_nn    <- matrix(z_val[nn$id], nrow = nrow(nn$id))
        results <- t(apply(z_nn, 1, function(z_row) {
          get_quantiles_uniform(z_row, probs)
        }))
      } else {
        results <- t(vapply(seq_len(nrow(nn$id)), function(j) {
          idx <- nn$id[j, ]
          get_quantiles(z_val[idx], w_val[idx], probs)
        }, numeric(3)))
      }

      list(z_lower = results[, 1], z_p50 = results[, 2], z_upper = results[, 3], n = k_use)
    }

    if (!is.null(group_vars)) {
      groups <- unique(data_dt[, ..group_vars])
      for (i in seq_len(nrow(groups))) {
        grp     <- groups[i]
        grp_idx <- rep(TRUE, nrow(data_dt))
        for (gv in group_vars) grp_idx <- grp_idx & (data_dt[[gv]] == grp[[gv]])
        grp_data <- data_dt[grp_idx]
        if (nrow(grp_data) == 0) next

        if (!is.null(leave_out)) {
          for (loo_val in leave_out_vals) {
            test_idx <- grp_data[[leave_out]] == loo_val
            val_idx  <- grp_data[[leave_out]] != loo_val
            if (sum(test_idx) == 0 || sum(val_idx) == 0) next

            mats  <- prep_knn_matrices(
              X_all[grp_data$.idx[val_idx],  , drop = FALSE],
              X_all[grp_data$.idx[test_idx], , drop = FALSE],
              distance_metric
            )
            k_use <- if (is.null(k)) ceiling(sqrt(sum(val_idx))) else min(k, sum(val_idx))
            res   <- run_local_knn(mats$X_val, mats$X_test,
                                   grp_data$z[val_idx], grp_data$w[val_idx], k_use)

            data_dt[grp_data$.idx[test_idx], `:=`(
              z_lower_local = res$z_lower, z_p50_local = res$z_p50,
              z_upper_local = res$z_upper, n_local     = res$n
            )]
          }
        } else {
          mats  <- prep_knn_matrices(
            X_all[grp_data$.idx, , drop = FALSE],
            X_all[grp_data$.idx, , drop = FALSE],
            distance_metric
          )
          k_use <- if (is.null(k)) ceiling(sqrt(nrow(grp_data))) else min(k, nrow(grp_data))
          res   <- run_local_knn(mats$X_val, mats$X_test,
                                 grp_data$z, grp_data$w, k_use)

          data_dt[grp_data$.idx, `:=`(
            z_lower_local = res$z_lower, z_p50_local = res$z_p50,
            z_upper_local = res$z_upper, n_local     = res$n
          )]
        }
      }
    } else {
      if (!is.null(leave_out)) {
        for (loo_val in leave_out_vals) {
          test_idx <- leave_out_vec == loo_val
          val_idx  <- leave_out_vec != loo_val
          if (sum(test_idx) == 0 || sum(val_idx) == 0) next

          mats  <- prep_knn_matrices(
            X_all[val_idx,  , drop = FALSE],
            X_all[test_idx, , drop = FALSE],
            distance_metric
          )
          k_use <- if (is.null(k)) ceiling(sqrt(sum(val_idx))) else min(k, sum(val_idx))
          res   <- run_local_knn(mats$X_val, mats$X_test,
                                 z_vec[val_idx], w_vec[val_idx], k_use)

          data_dt[which(test_idx), `:=`(
            z_lower_local = res$z_lower, z_p50_local = res$z_p50,
            z_upper_local = res$z_upper, n_local     = res$n
          )]
        }
      } else {
        mats  <- prep_knn_matrices(X_all, X_all, distance_metric)
        k_use <- if (is.null(k)) ceiling(sqrt(nrow(data_dt))) else min(k, nrow(data_dt))
        res   <- run_local_knn(mats$X_val, mats$X_test, z_vec, w_vec, k_use)

        data_dt[, `:=`(
          z_lower_local = res$z_lower, z_p50_local = res$z_p50,
          z_upper_local = res$z_upper, n_local     = res$n
        )]
      }
    }

    data_dt[, .idx := NULL]
    avg_n <- round(mean(data_dt$n_local, na.rm = TRUE))
    message(sprintf("Computed localized %.1f%% intervals using k=%d neighbors (avg)",
                    (1 - alpha) * 100, avg_n))
  }

  # Apply thresholds to generate predictions with proper percentile formatting
  lower_percentile <- (alpha / 2) * 100
  upper_percentile <- (1 - alpha / 2) * 100

  if (lower_percentile == floor(lower_percentile)) {
    lower_str <- sprintf("%02d", as.integer(lower_percentile))
    upper_str <- sprintf("%02d", as.integer(upper_percentile))
  } else if (lower_percentile * 10 == floor(lower_percentile * 10)) {
    lower_str <- gsub("\\.", "", sprintf("%04.1f", lower_percentile))
    upper_str <- gsub("\\.", "", sprintf("%04.1f", upper_percentile))
  } else {
    lower_str <- gsub("\\.", "", sprintf("%05.2f", lower_percentile))
    upper_str <- gsub("\\.", "", sprintf("%05.2f", upper_percentile))
  }

  lower_col_global <- sprintf("pred_p%s_global", lower_str)
  upper_col_global <- sprintf("pred_p%s_global", upper_str)

  data_dt[, pred_p50_global     := pmax(0, pmin(1, get(est_col_name) + fifelse(z_p50_global <= 0, lw * z_p50_global, uw * z_p50_global)))]
  data_dt[, (lower_col_global)  := pmax(0, get(est_col_name) + lw * z_lower_global)]
  data_dt[, (upper_col_global)  := pmin(1, get(est_col_name) + uw * z_upper_global)]
  data_dt[, n_obs_global        := n_global]

  if (use_local) {
    lower_col_local <- sprintf("pred_p%s_local", lower_str)
    upper_col_local <- sprintf("pred_p%s_local", upper_str)

    data_dt[, pred_p50_local    := pmax(0, pmin(1, get(est_col_name) + fifelse(z_p50_local <= 0, lw * z_p50_local, uw * z_p50_local)))]
    data_dt[, (lower_col_local) := pmax(0, get(est_col_name) + lw * z_lower_local)]
    data_dt[, (upper_col_local) := pmin(1, get(est_col_name) + uw * z_upper_local)]
    data_dt[, n_obs_local       := n_local]
  }

  cols_to_remove <- c("z_lower_global", "z_p50_global", "z_upper_global", "n_global",
                      "lw", "uw", "err", "z", "w")
  if (use_local) cols_to_remove <- c(cols_to_remove, "z_lower_local", "z_p50_local", "z_upper_local", "n_local")
  data_dt[, (cols_to_remove) := NULL]

  return(data_dt[])
}
