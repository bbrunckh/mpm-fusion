#' Check if at least k deprivations are present
#'
#' Returns NA if data is missing for any deprivation indicator.
#'
#' @param data A data.table with deprivation indicator columns.
#' @param deps Character vector of column names for each deprivation indicator
#'   (e.g., \code{c("dep_poor1", "dep_educ_com", "dep_educ_enr")}).
#' @param k Integer. Minimum number of deprivations required to return 1.
#'
#' @return Numeric vector of 0/1/NA: 1 if row has at least k deprivations, NA if any indicator is missing.
atleast_k_deps <- function(data, deps, k) {
  X <- as.matrix(data[, ..deps])
  storage.mode(X) <- "numeric"
  # all_present: TRUE if no NA in row
  all_present <- rowSums(is.na(X)) == 0
  # sum of deprivations (NA treated as zero only when all_present)
  rs <- rowSums(X, na.rm = TRUE)
  ans <- as.numeric(rs >= k)
  ans[!all_present] <- NA_real_
  ans
}

#' Check if at least k dimensions have at least one deprivation
#'
#' Returns NA if data is missing for any deprivation indicator within any dimension.
#'
#' @param data A data.table with deprivation indicator columns.
#' @param dims A list where each element is a character vector of column names for that dimension
#'   (e.g., \code{list("dep_poor1", c("dep_educ_com", "dep_educ_enr"), c("dep_infra_elec", "dep_infra_impw", "dep_infra_imps"))}).
#' @param k Integer. Minimum number of dimensions required to have at least one deprivation.
#'
#' @return Numeric vector of 0/1/NA: 1 if row is deprived in at least k dimensions, NA if any indicator is missing.
atleast_k_dims <- function(data, dims, k) {
  n <- nrow(data)
  D <- length(dims)
  dim_ind <- matrix(NA_real_, n, D)
  for (d in seq_len(D)) {
    cols <- dims[[d]]
    X <- as.matrix(data[, ..cols])
    storage.mode(X) <- "numeric"
    # availability: any NA in the row => dimension missing
    any_na <- rowSums(is.na(X)) > 0
    # deprived in dimension: any indicator > 0 among available
    dep_in_dim <- rowSums((X > 0) & !is.na(X)) > 0
    v <- as.numeric(dep_in_dim)
    v[any_na] <- NA_real_
    dim_ind[, d] <- v
  }
  all_dims_present <- rowSums(is.na(dim_ind)) == 0
  k_or_more <- rowSums(dim_ind, na.rm = TRUE) >= k
  ans <- as.numeric(k_or_more)
  ans[!all_dims_present] <- NA_real_
  ans
}

#' Compute weighted headcount for the Multidimensional Poverty Measure (MPM)
#'
#' Returns 1 if a row's weighted deprivation score meets or exceeds the threshold k.
#' Returns NA if any dimension has no available indicator.
#'
#' @param data A data.table with deprivation indicator columns.
#' @param dims A list where each element is a character vector of column names for that dimension
#'   (e.g., \code{list(c("dep_poor1"), c("dep_educ_com", "dep_educ_enr"), c("dep_infra_elec", "dep_infra_impw", "dep_infra_imps"))}).
#' @param weights Optional numeric vector of dimension weights (should sum to 1). Defaults to equal weights.
#' @param k Numeric. Deprivation score threshold for being considered poor.
#'
#' @return Numeric vector of 0/1/NA: 1 if weighted deprivation score >= k, NA if any dimension is missing.
weighted_headcount <- function(data, dims, weights = NULL, k) {
  D <- length(dims)
  if (is.null(weights)) weights <- rep(1/D, D)
  n <- nrow(data)
  dep_scores <- numeric(n)
  any_dim_na <- logical(n)

  for (d in seq_len(D)) {
    cols <- dims[[d]]
    X <- as.matrix(data[, ..cols])
    storage.mode(X) <- "numeric"
    # available mask per row and count of available indicators
    avail <- !is.na(X)
    n_avail <- rowSums(avail)
    # if no indicator available in this dimension, mark NA
    dim_na <- n_avail == 0
    any_dim_na <- any_dim_na | dim_na
    # per-row, per-indicator weight = weights[d] / n_avail
    # Avoid division by zero: set to 0 where n_avail == 0
    per_ind_w <- (weights[d] / pmax(n_avail, 1))
    # contribution: sum(X * per_ind_w) over available indicators
    # Broadcast per_ind_w to columns
    contrib <- rowSums((X * per_ind_w) * avail, na.rm = TRUE)
    dep_scores <- dep_scores + contrib
  }

  # rows with any dimension NA -> NA score
  dep_scores[any_dim_na] <- NA_real_
  as.numeric(dep_scores >= k)
}

#' Compute adjusted headcount (AF method) for the Multidimensional Poverty Measure
#'
#' Returns the weighted deprivation score for poor individuals (score >= k), and 0 for the non-poor.
#' Returns NA if any dimension has no available indicator.
#'
#' @param data A data.table with deprivation indicator columns.
#' @param dims A list where each element is a character vector of column names for that dimension
#'   (e.g., \code{list(c("dep_poor1"), c("dep_educ_com", "dep_educ_enr"), c("dep_infra_elec", "dep_infra_impw", "dep_infra_imps"))}).
#' @param weights Optional numeric vector of dimension weights (should sum to 1). Defaults to equal weights.
#' @param k Numeric. Deprivation score threshold for being considered poor.
#'
#' @return Numeric vector: deprivation score for the poor, 0 for the non-poor, NA if any dimension is missing.
adjusted_headcount <- function(data, dims, weights = NULL, k) {
  D <- length(dims)
  if (is.null(weights)) weights <- rep(1/D, D)
  n <- nrow(data)
  dep_scores <- numeric(n)
  any_dim_na <- logical(n)

  for (d in seq_len(D)) {
    cols <- dims[[d]]
    X <- as.matrix(data[, ..cols])
    storage.mode(X) <- "numeric"
    avail <- !is.na(X)
    n_avail <- rowSums(avail)
    dim_na <- n_avail == 0
    any_dim_na <- any_dim_na | dim_na
    per_ind_w <- (weights[d] / pmax(n_avail, 1))
    contrib <- rowSums((X * per_ind_w) * avail, na.rm = TRUE)
    dep_scores <- dep_scores + contrib
  }

  dep_scores[any_dim_na] <- NA_real_

  is_poor <- dep_scores >= k
  ans <- dep_scores
  ans[!is_poor & !is.na(dep_scores)] <- 0
  ans
}
