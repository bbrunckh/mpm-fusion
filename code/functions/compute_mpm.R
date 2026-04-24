#' Compute multidimensional poverty point estimates
#'
#' Computes user-defined multidimensional poverty metrics as probability-weighted
#' averages, renormalizing over valid (non-NA) indicator rows within each group.
#'
#' @param data Data.frame/data.table with at least the probability column.
#' @param mpm_fns Named list of functions defining poverty metrics 
#'                (return 1/0/NA = poor/not poor/undefined for headcount metrics
#'                or a deprivation rate between 0 and 1 for AF method),
#'                each function must accept the full data and return a numeric vector
#'                the same length as nrow(data).
#' @param group_vars Character vector of column names to group by (NULL = single group).
#' @param prob_col_name Name of the probability column (default = "prob").
#'
#' @return data.table with columns: [group_vars...], mpm, estimate, <prob_col_name>_valid
#' 
compute_mpm <- function(data, mpm_fns, group_vars = NULL, prob_col_name = "prob") {
  dt <- if (is.data.table(data)) data else as.data.table(data)
  
  # Validate inputs
  if (!is.list(mpm_fns) || length(mpm_fns) == 0 || is.null(names(mpm_fns)) ||
      any(!nzchar(names(mpm_fns)))) {
    stop("'mpm_fns' must be a named, non-empty list")
  }
  if (!(prob_col_name %in% names(dt))) {
    stop(sprintf("Missing probability column '%s'", prob_col_name))
  }
  has_groups <- !is.null(group_vars) && length(group_vars) > 0
  if (has_groups && !all(group_vars %in% names(dt))) {
    missing <- paste(setdiff(group_vars, names(dt)), collapse = ", ")
    stop(sprintf("Missing group columns: %s", missing))
  }
  
  # Pre-extract probability as numeric vector (avoid repeated column access)
  prob <- as.numeric(dt[[prob_col_name]])
  n <- length(prob)
  
  # Build group id vector
  if (has_groups) {
    # Create temporary column for group ID
    dt[, .gid_tmp := .GRP, by = group_vars]
    gid <- dt$.gid_tmp
    n_groups <- max(gid)
  } else {
    gid <- rep(1L, n)
    n_groups <- 1L
  }
  
  # Pre-compute all indicators as a matrix (do this ONCE)
  mpm_names <- names(mpm_fns)
  k <- length(mpm_names)
  
  # Allocate matrix once
  ind_mat <- matrix(NA_real_, nrow = n, ncol = k)
  
  for (j in seq_len(k)) {
    res <- mpm_fns[[j]](dt)
    if (!is.numeric(res)) stop("MPM function must return a numeric vector")
    if (length(res) != n) stop("MPM result length mismatch")
    ind_mat[, j] <- as.numeric(res)
  }
  
  # Pre-allocate result arrays for all groups x all MPMs
  prob_valid_arr <- matrix(0, nrow = n_groups, ncol = k)
  numer_arr <- matrix(0, nrow = n_groups, ncol = k)
  
  # Single pass through data: accumulate by group for ALL MPMs at once
  for (i in seq_len(n)) {
    g <- gid[i]
    p <- prob[i]
    for (j in seq_len(k)) {
      if (!is.na(ind_mat[i, j])) {
        prob_valid_arr[g, j] <- prob_valid_arr[g, j] + p
        numer_arr[g, j] <- numer_arr[g, j] + p * ind_mat[i, j]
      }
    }
  }
  
  # Compute estimates
  estimate_arr <- ifelse(prob_valid_arr > 0, numer_arr / prob_valid_arr, NA_real_)
  
  # Build output data.table efficiently
  # Create long-format result
  res <- data.table(
    gid = rep(seq_len(n_groups), each = k),
    mpm = rep(mpm_names, times = n_groups),
    estimate = as.vector(t(estimate_arr)),
    prob_valid = as.vector(t(prob_valid_arr))
  )
  
  # Add group columns if needed
  if (has_groups) {
    group_map <- unique(dt[, c(".gid_tmp", group_vars), with = FALSE])
    setnames(group_map, ".gid_tmp", "gid")
    res <- merge(res, group_map, by = "gid", sort = FALSE)
    res[, gid := NULL]
    setcolorder(res, c(group_vars, "mpm", "estimate", "prob_valid"))
    # Clean up temporary column
    dt[, .gid_tmp := NULL]
  } else {
    res[, gid := NULL]
    setcolorder(res, c("mpm", "estimate", "prob_valid"))
  }
  setnames(res, "prob_valid", paste0(prob_col_name, "_valid"))
  setnames(res, "estimate", paste0(prob_col_name))
  
  res[]
}