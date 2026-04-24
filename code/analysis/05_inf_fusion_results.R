# ============================================================================
# VALIDATION RESULTS
# ============================================================================

# Load required packages
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(scales)

# ============================================================================
# PART 1: VALIDATION METRICS ANALYSIS
# ============================================================================

# load data
mpm_val_national <- nanoparquet::read_parquet(here("output/fused_inf_val_national.parquet"))

# labels 
mpm_val_national <- mpm_val_national |>
  mutate(
    target = case_when(
      target == "pop_sh" ~ "Fused estimate",
      target == "pred_p50_global" ~ "Corrected (global)",
      target == "pred_p50_local" ~ "Corrected (local)"),
    data_level = if_else(data_level == "RuralUrban", "Rural-Urban",data_level),
    mpm = case_when(
      mpm == "al1_dep" ~ "At least 1",
      mpm == "al2_dep" ~ "At least 2",
      mpm == "all3_dep" ~ "All 3")) |>
  rename(N = n)

# ordering
s_list <- c("e-w-s", "e-ws", "es-w", "ew-s", "es-ws", "ew-es", "ew-ws")
est_list <- c("Fused estimate", "Corrected (global)", "Corrected (local)")
level_list <- c("National", "Rural-Urban", "Quintile", "Subnational")
mpm_list <- c("At least 1", "At least 2", "All 3")
mpm_val_national <-mpm_val_national |>
  mutate(scenario = factor(scenario, levels = s_list),
         est_list = factor(target, levels = est_list),
         data_level = factor(data_level, levels = level_list),
         mpm = factor(mpm, levels = mpm_list))


dt_val <- as.data.table(mpm_val_national)

# ============================================================================
# SUMMARY TABLES - Export to Excel with formatting
# ============================================================================

# Filter for sim_bias == 1
dt_bias1 <- dt_val[sim_bias == 1]

# Create workbook
wb_summary <- createWorkbook()

# Get unique data levels
data_levels <- unique(dt_bias1$data_level)

for (level in data_levels) {
  # Filter for this data level
  dt_level <- dt_bias1[data_level == level]
  
  # Get unique targets
  targets <- unique(dt_level$target)
  
  # Pivot wider for mae
  mae_wide <- dt_level %>%
    select(mpm, scenario, target, mae) %>%
    pivot_wider(names_from = target, values_from = mae, names_prefix = "mae_")
  
  # Pivot wider for mpiw
  mpiw_wide <- dt_level %>%
    select(mpm, scenario, target, mpiw) %>%
    pivot_wider(names_from = target, values_from = mpiw, names_prefix = "mpiw_")
  
  # Get n (should be same across targets for same scenario/mpm)
  n_data <- dt_level %>%
    group_by(mpm, scenario) %>%
    summarise(N = first(N), .groups = "drop")
  
  # Merge all together
  summary_table <- mae_wide %>%
    left_join(mpiw_wide, by = c("mpm", "scenario")) %>%
    left_join(n_data, by = c("mpm", "scenario")) %>%
    arrange(mpm, scenario)
  
  # Create combined MPM/Scenario column
  summary_table <- summary_table %>%
    mutate(mpm_scenario = scenario) %>%
    select(mpm, mpm_scenario, everything(), -scenario)
  
  # Format numeric columns to 1 decimal place (except N)
  numeric_cols <- sapply(summary_table, is.numeric)
  cols_to_format <- setdiff(names(summary_table)[numeric_cols], "N")
  summary_table[cols_to_format] <- lapply(summary_table[cols_to_format], round, 2)
  
  # Add sheet
  addWorksheet(wb_summary, level)
  
  # Calculate column positions
  mae_cols <- grep("^mae_", names(summary_table))
  mpiw_cols <- grep("^mpiw_", names(summary_table))
  n_col <- which(names(summary_table) == "N")
  
  # Write header row 1: Column groups (MPM, MAE, MPIW, N)
  writeData(wb_summary, level, "MPM", startCol = 1, startRow = 1)
  writeData(wb_summary, level, "MAE", startCol = 2, startRow = 1)
  writeData(wb_summary, level, "MPIW", startCol = 2 + length(targets), startRow = 1)
  writeData(wb_summary, level, "N", startCol = 2 + 2*length(targets), startRow = 1)
  
  # Write header row 2: Individual targets
  writeData(wb_summary, level, "MPM", startCol = 1, startRow = 2)
  for (i in seq_along(targets)) {
    writeData(wb_summary, level, targets[i], startCol = 1 + i, startRow = 2)
    writeData(wb_summary, level, targets[i], startCol = 1 + length(targets) + i, startRow = 2)
  }
  writeData(wb_summary, level, "n", startCol = 2 + 2*length(targets), startRow = 2)
  
  # Write data starting from row 3
  # For each MPM group, write MPM value once, then scenarios indented
  current_row <- 3
  mpms_in_table <- unique(summary_table$mpm)
  
  for (mpm_val in mpms_in_table) {
    mpm_data <- summary_table %>% filter(mpm == mpm_val)
    
    # Write MPM header row
    writeData(wb_summary, level, mpm_val, startCol = 1, startRow = current_row)
    
    # Apply bold style to MPM row
    bold_style <- createStyle(textDecoration = "bold")
    addStyle(wb_summary, level, bold_style, rows = current_row, cols = 1, 
             gridExpand = FALSE, stack = TRUE)
    
    current_row <- current_row + 1
    
    # Write scenario rows (indented)
    for (j in 1:nrow(mpm_data)) {
      # Write scenario name with indent
      writeData(wb_summary, level, paste0("  ", mpm_data$mpm_scenario[j]), 
                startCol = 1, startRow = current_row)
      
      # Write data columns (skip mpm and mpm_scenario columns)
      data_cols <- mpm_data[j, -c(1, 2)]
      writeData(wb_summary, level, data_cols, startCol = 2, startRow = current_row, colNames = FALSE)
      
      current_row <- current_row + 1
    }
  }
  
  # Merge cells for column groups
  mergeCells(wb_summary, level, cols = 2:(1 + length(targets)), rows = 1)
  mergeCells(wb_summary, level, cols = (2 + length(targets)):(1 + 2*length(targets)), rows = 1)
  mergeCells(wb_summary, level, cols = (2 + 2*length(targets)), rows = 1:2)
  mergeCells(wb_summary, level, cols = 1, rows = 1:2)
  
  # Apply styles
  # Header style (bold, centered)
  header_style <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                              border = "TopBottomLeftRight", borderStyle = "thin")
  
  # Apply header style to rows 1-2
  addStyle(wb_summary, level, header_style, rows = 1:2, cols = 1:(2 + 2*length(targets)), 
           gridExpand = TRUE, stack = TRUE)
  
  # Apply number formatting for numeric columns (1 decimal place)
  for (col_idx in 2:(ncol(summary_table) - 1)) {  # -1 to exclude N
    addStyle(wb_summary, level, 
             style = createStyle(numFmt = "0.00"),
             rows = 3:(current_row - 1), 
             cols = col_idx, 
             gridExpand = TRUE, stack = TRUE)
  }
  
  # Format n as integer (0 decimal places)
  n_col_position <- ncol(summary_table) - 1  # Position in written data
  addStyle(wb_summary, level,
           style = createStyle(numFmt = "0"),
           rows = 3:(current_row - 1),
           cols = n_col_position,
           gridExpand = TRUE, stack = TRUE)
  
  # Set column widths
  setColWidths(wb_summary, level, cols = 1:ncol(summary_table), widths = 15)
}
# Save summary workbook
saveWorkbook(wb_summary, here("output/tables/inf_summary_tables.xlsx"), overwrite = TRUE)

# ============================================================================
# EXTENDED TABLES - One per MPM
# ============================================================================

wb_extended <- createWorkbook()

mpms <- unique(dt_bias1$mpm)
metric_cols <- c("bias", "mae", "mape", "mse", "rmse", "r", "rho", "mpiw")

for (mpm_val in mpms) {
  # Filter for this mpm
  dt_mpm <- dt_bias1[mpm == mpm_val]
  
  # Get targets
  targets <- unique(dt_mpm$target)
  
  # Create long format with all metrics
  extended_table <- dt_mpm %>%
    select(data_level, scenario, target, all_of(metric_cols), N) %>%
    pivot_wider(
      names_from = target,
      values_from = all_of(metric_cols),
      names_glue = "{target}_{.value}"
    ) %>%
    arrange(data_level, scenario) |>
    select(data_level, scenario, starts_with(unique(dt_mpm$target)), N)
  
  # Create combined Level/Scenario column
  extended_table <- extended_table %>%
    mutate(level_scenario = scenario) %>%
    select(data_level, level_scenario, everything(), -scenario)
  
  # Format numeric columns with different precision
  # Identify columns by metric type (they have target prefix)
  all_cols <- names(extended_table)
  
  # Metrics with 2 decimal places
  metrics_2dp <- c("mae", "mape", "mse", "rmse", "mpiw", "mpiw")
  pattern_2dp <- paste0("_(", paste(metrics_2dp, collapse = "|"), ")$")
  cols_2dp <- all_cols[grepl(pattern_2dp, all_cols)]
  
  # Metrics with 3 decimal places
  metrics_3dp <- c("bias", "r", "rho")
  pattern_3dp <- paste0("_(", paste(metrics_3dp, collapse = "|"), ")$")
  cols_3dp <- all_cols[grepl(pattern_3dp, all_cols)]
  
  # Format values (keep as numeric)
  if (length(cols_2dp) > 0) {
    extended_table[cols_2dp] <- lapply(extended_table[cols_2dp], round, 2)
  }
  if (length(cols_3dp) > 0) {
    extended_table[cols_3dp] <- lapply(extended_table[cols_3dp], round, 3)
  }
  
  # Add sheet (clean up mpm name for sheet name)
  sheet_name <- substr(gsub("[^A-Za-z0-9]", "_", mpm_val), 1, 31)
  addWorksheet(wb_extended, sheet_name)
  
  # Create header structure
  # Row 1: Target names (merged across metrics)
  # Row 2: Metric names under each target
  
  start_col <- 2  # After level/scenario combined column
  writeData(wb_extended, sheet_name, "Level", startCol = 1, startRow = 1)
  
  # Write target headers (row 1) and metric headers (row 2)
  for (i in seq_along(targets)) {
    target <- targets[i]
    # Merge cells for this target across all its metrics
    target_start_col <- start_col + (i - 1) * length(metric_cols)
    target_end_col <- target_start_col + length(metric_cols) - 1
    
    writeData(wb_extended, sheet_name, target, startCol = target_start_col, startRow = 1)
    mergeCells(wb_extended, sheet_name, cols = target_start_col:target_end_col, rows = 1)
    
    # Write metric names for this target
    for (j in seq_along(metric_cols)) {
      metric_col <- target_start_col + j - 1
      writeData(wb_extended, sheet_name, metric_cols[j], startCol = metric_col, startRow = 2)
    }
  }
  
  # Write n header
  n_col_idx <- start_col + length(targets) * length(metric_cols)
  writeData(wb_extended, sheet_name, "N", startCol = n_col_idx, startRow = 1)
  mergeCells(wb_extended, sheet_name, cols = n_col_idx, rows = 1:2)
  
  # Merge Level cell vertically
  mergeCells(wb_extended, sheet_name, cols = 1, rows = 1:2)
  
  # Write data starting from row 3
  # For each Level group, write Level value once, then scenarios indented
  current_row <- 3
  levels_in_table <- unique(extended_table$data_level)
  
  for (level_val in levels_in_table) {
    level_data <- extended_table %>% filter(data_level == level_val)
    
    # Write Level header row
    writeData(wb_extended, sheet_name, level_val, startCol = 1, startRow = current_row)
    
    # Apply bold style to Level row
    bold_style <- createStyle(textDecoration = "bold")
    addStyle(wb_extended, sheet_name, bold_style, rows = current_row, cols = 1, 
             gridExpand = FALSE, stack = TRUE)
    
    current_row <- current_row + 1
    
    # Write scenario rows (indented)
    for (j in 1:nrow(level_data)) {
      # Write scenario name with indent
      writeData(wb_extended, sheet_name, paste0("  ", level_data$level_scenario[j]), 
                startCol = 1, startRow = current_row)
      
      # Write data columns (skip data_level and level_scenario columns)
      data_cols <- level_data[j, -c(1, 2)]
      writeData(wb_extended, sheet_name, data_cols, startCol = 2, startRow = current_row, colNames = FALSE)
      
      current_row <- current_row + 1
    }
  }
  
  # Apply styles
  # Header style
  header_style <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                              border = "TopBottomLeftRight", borderStyle = "thin")
  addStyle(wb_extended, sheet_name, header_style, rows = 1:2, cols = 1:n_col_idx,
           gridExpand = TRUE, stack = TRUE)
  
  # Apply number formatting to columns
  # For each target's metrics, apply appropriate formatting
  for (i in seq_along(targets)) {
    target_start_col <- start_col + (i - 1) * length(metric_cols)
    
    for (j in seq_along(metric_cols)) {
      metric <- metric_cols[j]
      col_position <- target_start_col + j - 1  # -1 for 0-indexed data cols
      
      if (metric %in% metrics_2dp) {
        addStyle(wb_extended, sheet_name,
                 style = createStyle(numFmt = "0.00"),
                 rows = 3:(current_row - 1),
                 cols = col_position,
                 gridExpand = TRUE, stack = TRUE)
      }
      else if (metric %in% metrics_3dp) {
        addStyle(wb_extended, sheet_name,
                 style = createStyle(numFmt = "0.000"),
                 rows = 3:(current_row - 1),
                 cols = col_position,
                 gridExpand = TRUE, stack = TRUE)
      }
    }
  }
  
  # Format n as integer (0 decimal places)
  addStyle(wb_extended, sheet_name,
           style = createStyle(numFmt = "0"),
           rows = 3:(current_row - 1),
           cols = n_col_idx,
           gridExpand = TRUE, stack = TRUE)
  
  # Set column widths
  setColWidths(wb_extended, sheet_name, cols = 2:n_col_idx, widths = "5.5")
}

# Save extended workbook
saveWorkbook(wb_extended, here("output/tables/inf_extended_tables.xlsx"), overwrite = TRUE)

# ============================================================================
# FIGURES - Define consistent color palettes (colorblind-friendly)
# ============================================================================

# Color palette for scenarios
scenario_colors <- c(
  "e-w-s" = "#440154",    # Dark Purple
  "e-ws" = "#3b528b",       # Blue-Purple
  "es-w" = "#21918c",        # Teal
  "ew-s" = "#5ec962",        # Green (center)
  "es-ws" = "#fde725",       # Yellow
  "ew-es" = "#fd9668",      # Orange
  "ew-ws" = "#b63679"      # Pink-Red
)

# Color palette for targets
target_colors <- c(
  "Fused estimate" = "#3b528b",       # Blue-Purple
  "Corrected (global)" = "#fd9668",      # Orange
  "Corrected (local)" = "#b63679"      # Pink-Red
)

# Color palette for MPMs (if needed)
mpm_colors <- c(
  "At least 1" = "#3b528b",       # Blue-Purple
  "At least 2" = "#fd9668",      # Orange
  "All 3" = "#b63679"      # Pink-Red
)

# ============================================================================
# HEATPLOT 1: Scenarios x Data_level for each metric and mpm
# ============================================================================

mpms <- unique(dt_bias1$mpm)
metric_cols <- c("mae")

dt_bias1 <- dt_bias1 |>
  mutate(data_level = factor(data_level, levels = rev(level_list)),
         target = factor(target, levels = c("Corrected (local)","Corrected (global)", "Fused estimate")))

for (mpm_val in mpms) {
  for (metric in metric_cols) {
    dt_plot <- dt_bias1[mpm == mpm_val]
    max <- max(dt_plot[[metric]], na.rm = TRUE)
    
    p <- ggplot(dt_plot, aes(x = scenario, y = data_level, fill = .data[[metric]])) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = round(.data[[metric]], 2)), size = 5, color = "black") +
      facet_wrap(~ target, ncol = 1, dir = "br", axes = "all_x") +
      scale_fill_gradient2(low = "#0072B2", mid = "#f7f7f7", high = "#b2182b",
                           midpoint = median(dt_plot[[metric]], na.rm = TRUE),
                           limits = c(0,max)) +
      theme_minimal(base_size = 16) +
      labs(x = "", y = "", fill = toupper(metric)) +
      theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16)
      )
    
    ggsave(here("output", 
                paste0("figures/INF/heatplot_", mpm_val, "_", metric, ".png")), 
           p, width = 12, height = 8, dpi = 300)
  }
}

# ============================================================================
# HEATPLOT 2: Sim_bias x Scenario for each metric and mpm (National only)
# ============================================================================

dt_national <- dt_val[data_level == "National"]
mpms <- unique(dt_national$mpm)
metric_cols <- c("mae")

dt_national <- dt_national |>
  mutate(scenario = factor(scenario, levels = rev(s_list)),
         data_level = factor(data_level, levels = rev(level_list)),
         target = factor(target, levels = c("Corrected (local)","Corrected (global)", "Fused estimate")))

for (mpm_val in mpms) {
  for (metric in metric_cols) {
    dt_plot <- dt_national[mpm == mpm_val]
    max <- max(dt_plot[[metric]], na.rm = TRUE)
    
    p <- ggplot(dt_plot, aes(x = as.factor(sim_bias), y = scenario, fill = .data[[metric]])) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = round(.data[[metric]], 2)), size = 5, color = "black") +
      facet_wrap(~ target, ncol = 1, dir = "br", axes = "all_x") +
      scale_fill_gradient2(low = "#0072B2", mid = "#f7f7f7", high = "#b2182b",
                           midpoint = median(dt_plot[[metric]], na.rm = TRUE),
                           limits = c(0,max)) +
      theme_minimal(base_size = 16) +
      labs(x = "Simulated bias", y = "", fill = toupper(metric)) +
      theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16)
      )
    
    ggsave(here("output", 
                paste0("figures/INF/heatplot_", mpm_val, "_", metric, "_samplebias.png")), 
           p, width = 12, height = 8, dpi = 300)
  }
}

# ============================================================================
# LINEPLOT 1: Scenarios x MAE, one line per target
# ============================================================================

dt_bias1 <- dt_bias1 |>
  mutate(data_level = factor(data_level, levels = level_list),
         target = factor(target, levels = c("Fused estimate", "Corrected (global)", "Corrected (local)"))) 

p1 <- ggplot(dt_bias1, aes(x = scenario, y = mae, color = target, group = target)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  facet_grid(data_level ~ mpm, scales = "free_y") +
  scale_color_manual(values = target_colors) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Scenario", y = "MAE", color = "") 

ggsave(here("output/figures/INF/lineplot_mae_scenarios.png"),
       p1, width = 16, height = 12, dpi = 300)

# ============================================================================
# LINEPLOT 2: Sim_bias x MAE, one line per scenario
# ============================================================================

dt_plot <- filter(dt_val, data_level =="National") |>
  mutate(target = factor(target, levels = c("Fused estimate", "Corrected (global)", "Corrected (local)")))

p2 <- ggplot(dt_plot, aes(x = sim_bias, y = mae, color = scenario, group = scenario)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  facet_grid(target ~ mpm) +
  scale_color_manual(values = scenario_colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Simulated Bias", y = "MAE", color = "Scenario")

ggsave(here("output/figures/INF/lineplot_mae_samplebias_national.png"),
       p2, width = 16, height = 12, dpi = 300)

# ============================================================================
# INTERVAL WIDTH COMPARISON: MPIW across targets
# ============================================================================

# Combined MPIW comparison by scenario and level
for (mpm_val in mpms) {
  dt_plot <- dt_bias1[mpm == mpm_val]
  available_scenarios_mpm <- unique(dt_plot$scenario)
  scenario_colors_mpm <- scenario_colors[names(scenario_colors) %in% available_scenarios_mpm]
  
  p <- ggplot(dt_plot, aes(x = target, y = mpiw, fill = scenario)) +
    geom_col(position = "dodge") +
    facet_wrap(~ data_level, scales = "free_y") +
    scale_fill_manual(values = scenario_colors_mpm)  +
    theme_minimal(base_size = 20) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "", y = "Mean Prediction Interval Width",
         fill = "Scenario") +
    guides(fill = guide_legend(nrow = 1))
  
  ggsave(here("output", paste0("figures/INF/mpiw_comparison_combined_", 
                                        gsub("[^A-Za-z0-9]", "_", mpm_val), ".png")), 
         p, width = 14, height = 10, dpi = 300)
}

# ============================================================================
# PART 2: FUSED MPM RESULTS VISUALIZATIONS
# ============================================================================

# load MPM fusion results
fused_mpm <- nanoparquet::read_parquet(here("output/fused_inf.parquet"))

#labels
fused_mpm <-  fused_mpm |>
  mutate(level = if_else(level == "RuralUrban", "Rural-Urban",level),
         mpm = case_when(
           mpm == "al1_dep" ~ "At least 1",
           mpm == "al2_dep" ~ "At least 2",
           mpm == "all3_dep" ~ "All 3")) 

# ordering
s_list <- c("e-w-s", "e-ws", "es-w", "ew-s", "es-ws", "ew-es", "ew-ws")
level_list <- c("National", "Rural-Urban", "Quintile", "Subnational")
mpm_list <- c("At least 1", "At least 2", "All 3")
fused_mpm <-fused_mpm |>
  mutate(scenario = factor(scenario, levels = s_list),
         level = factor(level, levels = level_list),
         mpm = factor(mpm, levels = mpm_list))

# Load fused data
dt_fused <- as.data.table(fused_mpm)

# Filter for sim_bias == 1
dt_fused_bias1 <- dt_fused[sim_bias == 1]

# Get unique combinations
mpms_fused <- unique(dt_fused_bias1$mpm)
levels_fused <- unique(dt_fused_bias1$level)
estimates <- c("pop_sh", "pred_p50_global", "pred_p50_local")

# Color palette for estimate types (same as targets for consistency)
estimate_colors <- target_colors

# ============================================================================
# SCATTERPLOT 1: Estimate vs True by Scenario
# ============================================================================

# Summary scatter plots - all scenarios in one plot per estimate
for (est in estimates) {
  dt_plot <- dt_fused_bias1
  
  if (nrow(dt_plot) > 0) {
    available_scenarios_plot <- unique(dt_plot$scenario)
    scenario_colors_plot <- scenario_colors[names(scenario_colors) %in% available_scenarios_plot]
    
    p <- ggplot(dt_plot, aes(x = pop_sh_true, y = .data[[est]], color = scenario)) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                  color = "gray30", linewidth = 0.5) +
      geom_point(alpha = 0.6, size = 1, shape = 16) +
      facet_grid(level ~ mpm, scales = "free") +
      scale_color_manual(values = scenario_colors_plot) +
      scale_x_continuous(labels = function(x) x * 100) +
      scale_y_continuous(labels = function(x) x * 100) +
      theme_minimal(base_size = 20) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
      ) +
      guides(color = guide_legend(override.aes = list(size = 4), nrow = 1)) +
      labs(
        x = "True poverty (%)",
        y = "Predicted poverty (%)",
        color = "Scenario"
      )
    
    filename <- paste0("figures/INF/scatter_all_", 
                       est, ".png")
    
    ggsave(here("output", filename), p, width = 14, height = 10, dpi = 300)
  }
}

# ============================================================================
# SCATTERPLOT with bounds: Error vs True with Prediction Intervals
# ============================================================================

for (mpm_val in mpms_fused) {
  for (scenario_val in unique(dt_fused_bias1$scenario)) {
    for (level_val in c("National", "Subnational")) {
      
      dt_plot <- dt_fused_bias1[mpm == mpm_val & scenario == scenario_val & level == level_val]
      
      if (nrow(dt_plot) > 0) {
        # Calculate errors for each estimate
        dt_plot[, error_pop_sh := pop_sh - pop_sh_true]
        dt_plot[, error_local := pred_p50_local - pop_sh_true]
        
        # Calculate interval widths for ribbons (relative to true value)
        dt_plot[, lower_pop_sh := lower_bound - pop_sh_true]
        dt_plot[, upper_pop_sh := upper_bound - pop_sh_true]
        dt_plot[, lower_local := pred_p025_local - pop_sh_true]
        dt_plot[, upper_local := pred_p975_local - pop_sh_true]
        
        # Create the plot
        p <- ggplot(dt_plot, aes(x = pop_sh_true)) +
          # Add ribbons for prediction intervals
          geom_linerange(aes(ymin = lower_pop_sh, ymax = upper_pop_sh, color = "pop_sh"), 
                         alpha = 0.2, linewidth = 1.5) +
          geom_linerange(aes(ymin = lower_local, ymax = upper_local, color = "pred_p50_local"), 
                         alpha = 0.2, linewidth = 1.5) +
          # Add error points
          geom_point(aes(y = error_pop_sh, color = "pop_sh"), alpha = 0.6, size = 1.5, shape = 16) +
          geom_point(aes(y = error_local,  color = "pred_p50_local"), alpha = 0.6, size = 1.5, shape = 16) +
          # Add horizontal line at zero error
          geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
          scale_color_manual(values = c("#fdae61", "#2c7bb6"),
                             labels = c("pop_sh" = "Fused estimate w/ bounds",
                                        "pred_p50_local" = "Corrected (local) w/ 95% prediction intervals")) +
          scale_x_continuous(labels = function(x) x * 100) +
          scale_y_continuous(labels = function(x) x * 100) +
          theme_minimal(base_size = 16) +
          theme(
            legend.position = "top",
            panel.grid.minor = element_blank(),
            legend.text = element_text(size = 16),
            axis.text = element_text(size = 16)
          ) +
          labs(x = "True poverty (%)",y = "Error (Predicted - True)", color = NULL) 
        
        filename <- paste0("figures/INF/errorplot_", 
                           gsub("[^A-Za-z0-9]", "_", mpm_val), "_",
                           gsub("[^A-Za-z0-9]", "_", scenario_val), "_",
                           gsub("[^A-Za-z0-9]", "_", level_val),".png")
        
        ggsave(here("output", filename), p, width = 12, height = 8, dpi = 300)
      }
    }
  }
}
