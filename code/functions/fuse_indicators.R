#' Fuse Indicators and Calculate Bounds using data.table
#'
#' Sequentially fuses probability distributions using data.table syntax.
#'
#' @param df_list A list of dataframes or data.tables.
#' @param prob_col_name A string for the name of the probability column.
#'
#' @return A single datatable with the fused distribution and its bounds.

fuse_indicators <- function(df_list, prob_col_name = "prob") {

  # --- 1. Input Validation ---

  # Ensure df_list is a list with at least two dataframes to fuse.
  if (!is.list(df_list) || length(df_list) < 2) {
    stop("df_list must be a list containing at least two dataframes.")
  }
  
  # --- 2. Initialization ---
  
  # Convert all dataframes in the list to data.tables in-place for efficiency.
  lapply(df_list, setDT)
  
  # Create a copy of the first dataframe to start the fusion process.
  fused_dt <- copy(df_list[[1]])
  
  # Rename the probability column to a standard name ('prob') for internal use.
  setnames(fused_dt, prob_col_name, "prob")
  
  # Initialize the lower and upper probability bounds. For the first dataframe,
  # the bounds are equal to the probability itself.
  fused_dt[, `:=`(lower_prob = prob, upper_prob = prob)]
  
  # --- 3. Sequential Fusion Loop ---
  
  # Loop through the rest of the dataframes (from the second one onwards).
  for (i in 2:length(df_list)) {
    
    # Copy the next dataframe to avoid modifying the original input.
    next_dt <- copy(df_list[[i]])
    
    # Rename its probability column to 'p2' to distinguish it during the merge.
    setnames(next_dt, prob_col_name, "p2")
    
    # Automatically detect common columns to use as the join key.
    common_vars <- intersect(names(fused_dt), names(next_dt))
    
    # Remove special probability/bound columns from the join key.
    common_vars <- setdiff(common_vars, c("prob", "lower_prob", "upper_prob", "p2"))
    if (length(common_vars) == 0) stop("No common variables found for fusion.")
    
    # Compute P(Y|X_c) from the incoming dataset
    next_dt[, p2_cond := p2 / sum(p2, na.rm = TRUE), by = common_vars]

    # Compute P(X_c) from the fused dataset
    fused_dt[, prob_common := sum(prob, na.rm = TRUE), by = common_vars]

    fused_dt <- merge(fused_dt, next_dt, by = common_vars, all.x = TRUE, allow.cartesian = TRUE)

    # Update joint probability (conditional independence) and FH bounds
    fused_dt[, `:=`(
      # Lower Bound = max(P(X) + P(X_c)*P(Y|X_c) - P(X_c))
      lower_prob = pmax(0, lower_prob + prob_common*p2_cond - prob_common),
      # Upper Bound = min(P(X), P(X_c)*P(Y|X_c))
      upper_prob = pmin(upper_prob, prob_common*p2_cond),
      # Conditional independence = P(X) * P(Y|X_c)
      prob = prob * p2_cond,
      # Cleanup
      p2 = NULL,
      p2_cond = NULL,
      prob_common = NULL
    )]
  }
  
  # --- 4. Rename and Return ---

  new_names <- c(prob_col_name, paste0(prob_col_name, "_lower"), paste0(prob_col_name, "_upper"))
  setnames(fused_dt, c("prob", "lower_prob", "upper_prob"), new_names)

  return(fused_dt)
}
