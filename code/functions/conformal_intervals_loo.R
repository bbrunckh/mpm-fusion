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
#' @param k Number of nearest neighbors for local intervals (default = ceiling(sqrt(n_val)))
#' @param distance_metric "euclidean", "manhattan", or "mahalanobis" (default = "euclidean")
#' @param scale_features Scale features before distance computation (default = TRUE)
#' @param group_vars Character vector of grouping columns (default = NULL)
#' @param equal_weights Character vector for equal weighting (default = NULL)
#' @param leave_out Column name for leave-one-out grouping (default = NULL)
#'
#' @return data_dt with pred_p{percentile}_global/local, n_obs_global/local
#'
#' @note Local intervals use dbscan::kNN for Euclidean and Mahalanobis (via Cholesky
#'   pre-whitening). For single-feature cases, a sorted binary-search path replaces
#'   kd-tree construction. Manhattan distance uses the Euclidean path as an approximation.
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

  if (!is.null(leave_out)) {
    leave_out_vals <- unique(data_dt[[leave_out]])
    message(sprintf("Computing LOO intervals for %d unique %s values",
                    length(leave_out_vals), leave_out))
  } else {
    leave_out_vals <- NA
    message("No leave_out specified - using all data for each prediction")
  }

  message(sprintf("Computing global %.1f%% conformal intervals (n=%d observations)",
                  (1 - alpha) * 100, nrow(data_dt)))

  # Tag rows with their position; pre-split by group_vars once so each group
  # iteration works on a small slice rather than scanning all n_total rows per group.
  data_dt[, .row_idx := .I]
  grp_list <- if (!is.null(group_vars)) split(data_dt, by = group_vars, keep.by = TRUE) else list(data_dt)

  data_dt[, `:=`(z_lower_global = NA_real_, z_p50_global = NA_real_,
                 z_upper_global = NA_real_, n_global     = NA_integer_)]

  # === GLOBAL INTERVALS ===
  for (grp in grp_list) {
    if (nrow(grp) == 0) next
    z_g  <- grp$z
    w_g  <- grp$w
    rows <- grp$.row_idx

    if (!is.null(leave_out)) {
      loo_g <- grp[[leave_out]]

      # Sort once per group. Each LOO just filters the pre-sorted vectors —
      # no re-sort needed since removing elements from a sorted array stays sorted.
      fin   <- is.finite(z_g) & is.finite(w_g) & w_g > 0
      o     <- order(z_g[fin])
      z_s   <- z_g[fin][o]
      w_s   <- w_g[fin][o]
      loo_s <- loo_g[fin][o]

      for (loo_val in leave_out_vals) {
        test_mask <- loo_g == loo_val
        if (!any(test_mask)) next
        val_mask  <- loo_s != loo_val
        if (!any(val_mask)) next

        w_v  <- w_s[val_mask]
        w_v  <- w_v / sum(w_v)
        cumw <- cumsum(w_v)
        z_v  <- z_s[val_mask]
        qs   <- vapply(probs, function(p) z_v[which(cumw >= p)[1L]], numeric(1))

        data_dt[rows[test_mask], `:=`(
          z_lower_global = qs[1], z_p50_global = qs[2],
          z_upper_global = qs[3], n_global     = sum(loo_g != loo_val)
        )]
      }
    } else {
      qs <- get_quantiles(z_g, w_g, probs)
      data_dt[rows, `:=`(
        z_lower_global = qs[1], z_p50_global = qs[2],
        z_upper_global = qs[3], n_global     = length(z_g)
      )]
    }
  }

  # === LOCAL INTERVALS ===
  if (use_local) {
    stopifnot("Missing feature columns" = all(feature_cols %in% names(data_dt)))
    message(sprintf("Computing local %.1f%% conformal intervals using features",
                    (1 - alpha) * 100))

    data_dt[, `:=`(z_lower_local = NA_real_, z_p50_local = NA_real_,
                   z_upper_local = NA_real_, n_local     = NA_integer_)]

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

    # 1D fast path uses sort + findInterval instead of building a kd-tree.
    # Falls back to dbscan::kNN for multi-feature cases.
    run_local_knn <- function(X_val, X_test, z_val, w_val, k_use) {
      n_val <- nrow(X_val)

      if (ncol(X_val) == 1L) {
        x_v   <- X_val[, 1L]
        x_t   <- X_test[, 1L]
        o_val <- order(x_v)
        x_v_s <- x_v[o_val]
        z_v_s <- z_val[o_val]
        w_v_s <- w_val[o_val]

        # For each test point, the k nearest in 1D are near the insertion position.
        # Window [pos - k, pos + k + 1] is always wide enough to contain k neighbors.
        nn_ids <- t(vapply(x_t, function(xi) {
          pos   <- findInterval(xi, x_v_s)
          lo    <- max(1L, pos - k_use)
          hi    <- min(n_val, pos + k_use + 1L)
          cands <- seq.int(lo, hi)
          dists <- abs(x_v_s[cands] - xi)
          cands[order(dists, method = "radix")[seq_len(min(k_use, length(cands)))]]
        }, integer(k_use)))

        if (is.null(equal_weights)) {
          z_nn    <- matrix(z_v_s[nn_ids], nrow = nrow(nn_ids))
          results <- t(apply(z_nn, 1, function(z_row) get_quantiles_uniform(z_row, probs)))
        } else {
          results <- t(vapply(seq_len(nrow(nn_ids)), function(j) {
            idx <- nn_ids[j, ]
            get_quantiles(z_v_s[idx], w_v_s[idx], probs)
          }, numeric(3)))
        }
      } else {
        # Multi-feature: use dbscan kNN (kd-tree).
        # sort = FALSE: skip distance sorting since we only need indices for quantiles.
        nn <- dbscan::kNN(x = X_val, query = X_test, k = k_use, sort = FALSE)

        if (is.null(equal_weights)) {
          z_nn    <- matrix(z_val[nn$id], nrow = nrow(nn$id))
          results <- t(apply(z_nn, 1, function(z_row) get_quantiles_uniform(z_row, probs)))
        } else {
          results <- t(vapply(seq_len(nrow(nn$id)), function(j) {
            idx <- nn$id[j, ]
            get_quantiles(z_val[idx], w_val[idx], probs)
          }, numeric(3)))
        }
      }

      list(z_lower = results[, 1], z_p50 = results[, 2], z_upper = results[, 3], n = k_use)
    }

    for (grp in grp_list) {
      if (nrow(grp) == 0) next
      rows <- grp$.row_idx

      # Extract feature matrix directly from the group slice — no global X_all needed
      X_g <- as.matrix(grp[, ..feature_cols])
      storage.mode(X_g) <- "double"

      if (!is.null(leave_out)) {
        loo_g <- grp[[leave_out]]

        for (loo_val in leave_out_vals) {
          test_mask <- loo_g == loo_val
          val_mask  <- loo_g != loo_val
          if (!any(test_mask) || !any(val_mask)) next

          mats  <- prep_knn_matrices(
            X_g[val_mask,  , drop = FALSE],
            X_g[test_mask, , drop = FALSE],
            distance_metric
          )
          k_use <- if (is.null(k)) ceiling(sqrt(sum(val_mask))) else min(k, sum(val_mask))
          res   <- run_local_knn(mats$X_val, mats$X_test,
                                 grp$z[val_mask], grp$w[val_mask], k_use)

          data_dt[rows[test_mask], `:=`(
            z_lower_local = res$z_lower, z_p50_local = res$z_p50,
            z_upper_local = res$z_upper, n_local     = res$n
          )]
        }
      } else {
        mats  <- prep_knn_matrices(X_g, X_g, distance_metric)
        k_use <- if (is.null(k)) ceiling(sqrt(nrow(grp))) else min(k, nrow(grp))
        res   <- run_local_knn(mats$X_val, mats$X_test, grp$z, grp$w, k_use)

        data_dt[rows, `:=`(
          z_lower_local = res$z_lower, z_p50_local = res$z_p50,
          z_upper_local = res$z_upper, n_local     = res$n
        )]
      }
    }

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
                      "lw", "uw", "err", "z", "w", ".row_idx")
  if (use_local) cols_to_remove <- c(cols_to_remove, "z_lower_local", "z_p50_local", "z_upper_local", "n_local")
  data_dt[, (cols_to_remove) := NULL]

  return(data_dt[])
}
