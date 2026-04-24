library(highs)
library(data.table)

#' Estimate multidimensional poverty metrics and bounds after fusing data (OPTIMIZED)
#'
#' Estimates user-defined multidimensional poverty metrics and finds the 
#' theoretical minimum and maximum values by solving Linear Programming problems
#' subject to Fréchet-Hoeffding bounds and observed marginal probabilities.
#'
#' @param fused_dt Data.frame/data.table with joint probabilities and FH bounds
#' @param mpm_fns Named list of functions defining poverty metrics (return 1/0/NA)
#' @param ind_list List of indicator name vectors from original datasets
#' @param group_vars Column names for groups (NULL = single group)
#' @param prob_col_name Name of probability column (default = "prob")
#' @param solver HiGHS solver name (default = "choose")
#'
#' @return Data.table with estimates and bounds for each MPM

mpm_bounds <- function(fused_dt, mpm_fns, ind_list, group_vars = NULL, 
                       prob_col_name = "prob", solver = "choose") {
  
  # Setup
  fused_dt <- if (is.data.table(fused_dt)) fused_dt else as.data.table(fused_dt)
  lower_col <- paste0(prob_col_name, "_lower")
  upper_col <- paste0(prob_col_name, "_upper")
  mpm_names <- names(mpm_fns)
  
  # Validate inputs
  stopifnot(
    "Missing probability columns" = all(c(prob_col_name, lower_col, upper_col) %in% names(fused_dt)),
    "ind_list must be non-empty list" = is.list(ind_list) && length(ind_list) > 0,
    "Missing indicator columns" = all(unique(unlist(ind_list)) %in% names(fused_dt)),
    "mpm_fns must be named list" = is.list(mpm_fns) && length(mpm_fns) > 0 && 
      !is.null(mpm_names) && all(nzchar(mpm_names))
  )
  
  # Setup groups
  has_groups <- !is.null(group_vars) && length(group_vars) > 0
  
  if (!has_groups) {
    fused_dt[, .group_id := 1L]
    group_vars_internal <- ".group_id"
  } else {
    stopifnot("Missing group columns" = all(group_vars %in% names(fused_dt)))
    fused_dt[, .group_id := .GRP, by = group_vars]
    group_vars_internal <- group_vars
  }
  
  # Pre-compute MPM indicators once
  mpm_indicators <- lapply(mpm_fns, function(fn) {
    result <- fn(fused_dt)
    stopifnot(
      "MPM function must return numeric vector" = is.numeric(result),
      "MPM result length mismatch" = length(result) == nrow(fused_dt)
    )
    as.numeric(result)
  })
  
  # Calculate point estimates
  estimates <- compute_mpm(
    data = fused_dt,
    mpm_fns = mpm_fns,
    group_vars = if (has_groups) group_vars else NULL,
    prob_col_name = prob_col_name
  )
  
  # Pre-extract columns once (avoid repeated column lookups)
  prob_vec <- fused_dt[[prob_col_name]]
  lower_vec <- fused_dt[[lower_col]]
  upper_vec <- fused_dt[[upper_col]]
  group_vec <- fused_dt$.group_id
  
  # Build constraint matrix ONCE per group (reuse for all MPMs)
  # This is the key optimization since constraints don't depend on MPM
  build_constraints_optimized <- function(group_idx, prob_vals) {
    n_valid <- length(group_idx)
    if (n_valid == 0) return(list(A = matrix(0, nrow = 1, ncol = 0), lhs = 1, rhs = 1))
    
    total_valid_prob <- sum(prob_vals)
    
    # Pre-allocate with better initial size estimate
    n_datasets <- length(ind_list)
    est_rows <- 1 + n_datasets * 20  # rough estimate
    A_list <- vector("list", est_rows)
    lhs <- rhs <- numeric(est_rows)
    count <- 0
    
    # Constraint: probabilities sum to 1
    count <- count + 1
    A_list[[count]] <- rep(1, n_valid)
    lhs[count] <- rhs[count] <- 1
    
    # Pre-extract group data columns once
    group_data <- vector("list", length(ind_list))
    for (d in seq_along(ind_list)) {
      if (length(ind_list[[d]]) > 0) {
        group_data[[d]] <- lapply(ind_list[[d]], function(col) fused_dt[[col]][group_idx])
      }
    }
    
    # Marginal constraints - optimized matching
    for (d in seq_along(ind_list)) {
      dataset_vars <- ind_list[[d]]
      if (length(dataset_vars) == 0) next
      
      # Use data.table for fast unique combinations
      combo_dt <- as.data.table(group_data[[d]])
      setnames(combo_dt, dataset_vars)
      combos <- unique(combo_dt)
      
      for (i in seq_len(nrow(combos))) {
        # Vectorized matching (much faster than Reduce)
        match_vec <- rep(TRUE, n_valid)
        for (v in dataset_vars) {
          match_vec <- match_vec & (combo_dt[[v]] == combos[[v]][i])
        }
        match_idx <- which(match_vec)
        
        if (length(match_idx) == 0) next
        
        count <- count + 1
        if (count > length(A_list)) {
          # Grow arrays
          new_size <- length(A_list) * 2
          A_list <- c(A_list, vector("list", new_size - length(A_list)))
          lhs <- c(lhs, numeric(new_size - length(lhs)))
          rhs <- c(rhs, numeric(new_size - length(rhs)))
        }
        
        constraint_row <- numeric(n_valid)
        constraint_row[match_idx] <- 1
        target_prob_renorm <- sum(prob_vals[match_idx]) / total_valid_prob
        
        A_list[[count]] <- constraint_row
        lhs[count] <- rhs[count] <- target_prob_renorm
      }
    }
    
    list(
      A = do.call(rbind, A_list[1:count]), 
      lhs = lhs[1:count], 
      rhs = rhs[1:count]
    )
  }
  
  # Process all groups and MPMs
  groups <- sort(unique(group_vec))
  n_groups <- length(groups)
  n_mpms <- length(mpm_names)
  
  # Pre-allocate results
  bounds_list <- vector("list", n_groups * n_mpms)
  idx <- 1
  
  for (g in groups) {
    # Get indices for this group
    group_mask <- group_vec == g
    group_idx <- which(group_mask)
    n_total <- length(group_idx)
    
    if (n_total == 0) next
    
    # Extract group-level data once
    prob_vals <- prob_vec[group_idx]
    lower_vals <- lower_vec[group_idx]
    upper_vals <- upper_vec[group_idx]
    
    # Solve for each MPM
    # Note: We CAN'T fully reuse constraints across MPMs because each MPM
    # may have different NA patterns. However, we can still optimize by
    # caching constraint building per unique NA pattern.
    
    # Track constraint cache by NA pattern signature
    constraint_cache <- list()
    
    for (mpm_name in mpm_names) {
      L_full <- mpm_indicators[[mpm_name]][group_idx]
      
      # Determine valid indices for THIS MPM
      valid_mask <- if (anyNA(L_full)) !is.na(L_full) else rep(TRUE, n_total)
      
      if (!any(valid_mask)) {
        # All NA for this MPM in this group
        bounds_list[[idx]] <- data.table(
          .group_id = g, mpm = mpm_name, 
          lower_bound = NA_real_, upper_bound = NA_real_,
          status = "all_na"
        )
        idx <- idx + 1
        next
      }
      
      # Create a signature for this NA pattern to check cache
      na_sig <- paste(which(valid_mask), collapse = ",")
      
      # Check if we already built constraints for this pattern
      if (is.null(constraint_cache[[na_sig]])) {
        # Build constraints for this valid subset
        valid_idx <- group_idx[valid_mask]
        total_valid_prob <- sum(prob_vals[valid_mask])
        
        lower_bounds <- pmax(0, lower_vals[valid_mask] / total_valid_prob)
        upper_bounds <- pmin(1, upper_vals[valid_mask] / total_valid_prob)
        
        # Add small tolerance to avoid inconsistent bounds warnings
        # If bounds are within machine precision, slightly widen them
        tol <- 1e-10
        tight_mask <- abs(upper_bounds - lower_bounds) < tol
        if (any(tight_mask)) {
          mid <- (lower_bounds[tight_mask] + upper_bounds[tight_mask]) / 2
          lower_bounds[tight_mask] <- pmax(0, mid - tol)
          upper_bounds[tight_mask] <- pmin(1, mid + tol)
        }
        
        constraint_cache[[na_sig]] <- list(
          constraints = build_constraints_optimized(valid_idx, prob_vals[valid_mask]),
          lower_bounds = lower_bounds,
          upper_bounds = upper_bounds
        )
      }
      
      # Retrieve from cache
      cached <- constraint_cache[[na_sig]]
      constraints <- cached$constraints
      lower_bounds <- cached$lower_bounds
      upper_bounds <- cached$upper_bounds
      
      L <- L_full[valid_mask]
      
      # Solve min and max (suppress HiGHS warnings about tight bounds)
      min_sol <- suppressWarnings(highs_solve(
        L = L,
        lower = lower_bounds,
        upper = upper_bounds,
        A = constraints$A,
        lhs = constraints$lhs,
        rhs = constraints$rhs,
        maximum = FALSE, 
        control = list(solver = solver)
      ))
      
      max_sol <- suppressWarnings(highs_solve(
        L = L,
        lower = lower_bounds,
        upper = upper_bounds,
        A = constraints$A,
        lhs = constraints$lhs,
        rhs = constraints$rhs,
        maximum = TRUE, 
        control = list(solver = solver)
      ))
      
      # Check status
      min_ok <- min_sol$status == 7
      max_ok <- max_sol$status == 7
      
      status <- if (min_ok && max_ok) "optimal"
      else if (!min_ok && !max_ok) "both_failed"
      else if (!min_ok) "min_failed"
      else "max_failed"
      
      if (status != "optimal") {
        warning(sprintf("LP solver issue for group %d, MPM '%s': %s", g, mpm_name, status))
      }
      
      bounds_list[[idx]] <- data.table(
        .group_id = g, mpm = mpm_name,
        lower_bound = if (min_ok) min_sol$objective_value else NA_real_,
        upper_bound = if (max_ok) max_sol$objective_value else NA_real_,
        status = status
      )
      idx <- idx + 1
    }
  }
  
  bounds <- rbindlist(bounds_list[1:(idx-1)])
  
  # Merge and format results
  if (has_groups) {
    group_mapping <- unique(fused_dt[, c(".group_id", group_vars), with = FALSE])
    bounds <- merge(bounds, group_mapping, by = ".group_id", sort = FALSE)
    result <- merge(
      estimates,
      bounds[, .SD, .SDcols = !".group_id"],
      by = c(group_vars, "mpm"),
      sort = FALSE
    )
    setcolorder(result, c(group_vars, "mpm", paste0(prob_col_name), "lower_bound", 
                          "upper_bound", "status", 
                          paste0(prob_col_name, "_valid")))
  } else {
    result <- merge(
      estimates, 
      bounds[, .(mpm, lower_bound, upper_bound, status)], 
      by = "mpm", 
      sort = FALSE
    )
    setcolorder(result, c("mpm", paste0(prob_col_name), "lower_bound", "upper_bound", 
                          "status", paste0(prob_col_name, "_valid")))
  }
  
  # Cleanup temporary column
  fused_dt[, .group_id := NULL]
  
  result
}