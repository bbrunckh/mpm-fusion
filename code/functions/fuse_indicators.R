# Ensure necessary libraries are loaded
library(data.table) 
#' Fuse Indicators and Calculate Bounds using data.table
#'
#' Sequentially fuses a probability distributions using  data.table syntax.
#'
#' @param df_list A list of dataframes or data.tables.
#' @param vars_to_keep A character vector of column names for the final output.
#' @param prob_col_name A string for the name of the probability column.
#'
#' @return A single datatable with the fused distribution and its bounds.

fuse_indicators <- function(df_list, vars_to_keep = NULL, prob_col_name = "prob") {
  
  # --- 1. Input Validation ---
  
  # Ensure df_list is a list with at least two dataframes to fuse.
  if (!is.list(df_list) || length(df_list) < 2) {
    stop("df_list must be a list containing at least two dataframes.")
  }
  
  # If vars_to_keep is not provided, automatically determine them by finding all
  # unique column names across all dataframes, excluding the probability column.
  if (is.null(vars_to_keep)) {
    vars_to_keep <- setdiff(unique(unlist(lapply(df_list, names))), prob_col_name)
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
    
    # Calculate P(Y|X_c) for the independence calculation
    next_dt[, p2_cond := p2 / sum(p2, na.rm = TRUE), by = common_vars]
    
    # 2. Use the marginal probability of common variables from first/fused data
    fused_dt[, prob_common := sum(prob, na.rm = TRUE), by = common_vars]
    
    # 3. Perform the merge, first calculate probability of common vars
    fused_dt <- merge(fused_dt, next_dt, by = common_vars, all.x = TRUE, allow.cartesian = TRUE)
    
    # 4. Update Joint Probability and Bounds
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
  
  # --- 4. Marginalization and Final Output ---
  
  # Group by the 'vars_to_keep' and sum the probabilities and bounds to get the final marginalized distribution.
  final_dt <- fused_dt[, .(
    prob = sum(prob, na.rm = TRUE),
    lower_prob = sum(lower_prob, na.rm = TRUE),
    upper_prob = sum(upper_prob, na.rm = TRUE)
  ), by = vars_to_keep]
  
  # --- 5. Rename Final Columns ---
  
  # Create the new names for the output columns.
  new_names <- c(prob_col_name, paste0(prob_col_name, "_lower"), paste0(prob_col_name, "_upper"))
  
  # Rename the columns in the final data.table.
  setnames(final_dt, c("prob", "lower_prob", "upper_prob"), new_names)
  
  # Return the result
  return(final_dt)
}
