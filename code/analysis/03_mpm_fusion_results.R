# ============================================================================
# VALIDATION RESULTS
# ============================================================================

# ============================================================================
# PART 1: VALIDATION METRICS ANALYSIS
# ============================================================================

# load data
mpm_val_national <- as.data.table(nanoparquet::read_parquet(here("output/fused_mpm_val_national.parquet")))

# labels
mpm_val_national[, target := fcase(
  target == "pop_sh",          "Fused estimate",
  target == "pred_p50_global", "Corrected (global)",
  target == "pred_p50_local",  "Corrected (local)"
)]
mpm_val_national[, data_level := fifelse(data_level == "RuralUrban", "Rural-Urban", data_level)]
mpm_val_national[, mpm := fcase(
  mpm == "mpm",     "MPM",
  mpm == "mpm_af",  "MPM-AF",
  mpm == "al1_dep", "At least 1",
  mpm == "al3_dep", "At least 3"
)]
setnames(mpm_val_national, "n", "N")

# ordering
s_list     <- c("p-c-r-e-w-s", "p-cr-ews", "pcr-ews","pcre-ws",
                "pce-rews","pcew-rews","pcews-rews")
est_list   <- c("Fused estimate", "Corrected (global)", "Corrected (local)")
level_list <- c("National", "Rural-Urban", "Quintile", "Subnational")
mpm_list   <- c("MPM", "MPM-AF", "At least 1", "At least 3")

mpm_val_national[, scenario   := factor(scenario,   levels = s_list)]
mpm_val_national[, est_list   := factor(target,     levels = est_list)]
mpm_val_national[, data_level := factor(data_level, levels = level_list)]
mpm_val_national[, mpm        := factor(mpm,        levels = mpm_list)]

dt_val <- mpm_val_national

# ============================================================================
# SUMMARY TABLES - Export to Excel with formatting
# ============================================================================

# Filter for sim_bias == 1
dt_bias1 <- dt_val[sim_bias == 1]

create_summary_workbook(dt_bias1, "output/tables/mpm_summary_tables.xlsx")

# ============================================================================
# EXTENDED TABLES - One per MPM
# ============================================================================

create_extended_workbook(dt_bias1, "output/tables/mpm_extended_tables.xlsx")

# ============================================================================
# FIGURES - Define consistent color palettes (colorblind-friendly)
# ============================================================================

scenario_colors <- c(
  "p-c-r-e-w-s" = "#440154",    # Dark Purple
  "p-cr-ews"    = "#3b528b",    # Blue-Purple
  "pcr-ews"     = "#21918c",    # Teal
  "pcre-ws"     = "#5ec962",    # Green (center)
  "pce-rews"    = "#fde725",    # Yellow
  "pcew-rews"   = "#fd9668",    # Orange
  "pcews-rews"  = "#b63679"     # Pink-Red
)

target_colors <- c(
  "Fused estimate"    = "#3b528b",  # Blue-Purple
  "Corrected (global)" = "#fd9668", # Orange
  "Corrected (local)"  = "#b63679"  # Pink-Red
)

mpm_colors <- c(
  "MPM"       = "#440154",  # Dark Purple
  "MPM-AF"    = "#21918c",  # Teal
  "At least 1" = "#fd9668", # Orange
  "At least 3" = "#b63679"  # Pink-Red
)

# ============================================================================
# HEATPLOT 1: Scenarios x Data_level for each metric and mpm
# ============================================================================

mpms        <- unique(dt_bias1$mpm)
metric_cols <- c("mae")

dt_bias1[, data_level := factor(data_level, levels = rev(level_list))]
dt_bias1[, target     := factor(target, levels = c("Corrected (local)", "Corrected (global)", "Fused estimate"))]

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
                           limits = c(0, max)) +
      theme_minimal(base_size = 16) +
      labs(x = "", y = "", fill = toupper(metric)) +
      theme(
        axis.text      = element_text(size = 16),
        axis.title     = element_text(size = 16),
        legend.title   = element_text(size = 16),
        legend.text    = element_text(size = 16),
        strip.text     = element_text(size = 16)
      )

    ggsave(here("output", paste0("figures/MPM/heatplot_", mpm_val, "_", metric, ".png")),
           p, width = 12, height = 8, dpi = 300)
  }
}

# ============================================================================
# HEATPLOT 2: Sim_bias x Scenario for each metric and mpm (National only)
# ============================================================================

dt_national <- dt_val[data_level == "National"]
mpms        <- unique(dt_national$mpm)
metric_cols <- c("mae")

dt_national[, scenario   := factor(scenario,   levels = rev(s_list))]
dt_national[, data_level := factor(data_level, levels = rev(level_list))]
dt_national[, target     := factor(target, levels = c("Corrected (local)", "Corrected (global)", "Fused estimate"))]

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
                           limits = c(0, max)) +
      theme_minimal(base_size = 16) +
      labs(x = "Simulated bias", y = "", fill = toupper(metric)) +
      theme(
        axis.text      = element_text(size = 16),
        axis.title     = element_text(size = 16),
        legend.title   = element_text(size = 16),
        legend.text    = element_text(size = 16),
        strip.text     = element_text(size = 16)
      )

    ggsave(here("output", paste0("figures/MPM/heatplot_", mpm_val, "_", metric, "_samplebias.png")),
           p, width = 12, height = 8, dpi = 300)
  }
}

# ============================================================================
# LINEPLOT 1: Scenarios x MAE, one line per target
# ============================================================================

dt_bias1[, data_level := factor(data_level, levels = level_list)]
dt_bias1[, target     := factor(target, levels = c("Fused estimate", "Corrected (global)", "Corrected (local)"))]

p1 <- ggplot(dt_bias1, aes(x = scenario, y = mae, color = target, group = target)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  facet_grid(data_level ~ mpm, scales = "free_y") +
  scale_color_manual(values = target_colors) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x   = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y   = element_text(size = 20),
        axis.title.x  = element_text(size = 20),
        axis.title.y  = element_text(size = 20),
        strip.text.x  = element_text(size = 20),
        strip.text.y  = element_text(size = 20),
        legend.position = "bottom",
        legend.text   = element_text(size = 20),
        plot.title    = element_text(hjust = 0.5)) +
  labs(x = "Scenario", y = "MAE", color = "")

ggsave(here("output/figures/MPM/lineplot_mae_scenarios.png"),
       p1, width = 16, height = 12, dpi = 300)

# ============================================================================
# LINEPLOT 2: Sim_bias x MAE, one line per scenario
# ============================================================================

dt_plot <- dt_val[data_level == "National"]
dt_plot[, target := factor(target, levels = c("Fused estimate", "Corrected (global)", "Corrected (local)"))]

p2 <- ggplot(dt_plot, aes(x = sim_bias, y = mae, color = scenario, group = scenario)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  facet_grid(target ~ mpm) +
  scale_color_manual(values = scenario_colors) +
  guides(color = guide_legend(nrow = 1)) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x   = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y   = element_text(size = 20),
        axis.title.x  = element_text(size = 20),
        axis.title.y  = element_text(size = 20),
        strip.text.x  = element_text(size = 20),
        strip.text.y  = element_text(size = 20),
        legend.position = "bottom",
        legend.text   = element_text(size = 20),
        plot.title    = element_text(hjust = 0.5)) +
  labs(x = "Simulated Bias", y = "MAE", color = "Scenario")

ggsave(here("output/figures/MPM/lineplot_mae_samplebias_national.png"),
       p2, width = 16, height = 12, dpi = 300)

# ============================================================================
# INTERVAL WIDTH COMPARISON: MPIW across targets
# ============================================================================

for (mpm_val in mpms) {
  dt_plot <- dt_bias1[mpm == mpm_val]
  available_scenarios_mpm <- unique(dt_plot$scenario)
  scenario_colors_mpm <- scenario_colors[names(scenario_colors) %in% available_scenarios_mpm]

  p <- ggplot(dt_plot, aes(x = target, y = mpiw, fill = scenario)) +
    geom_col(position = "dodge") +
    facet_wrap(~ data_level, scales = "free_y") +
    scale_fill_manual(values = scenario_colors_mpm) +
    theme_minimal(base_size = 20) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "", y = "Mean Prediction Interval Width", fill = "Scenario") +
    guides(fill = guide_legend(nrow = 1))

  ggsave(here("output", paste0("figures/MPM/mpiw_comparison_combined_",
                               gsub("[^A-Za-z0-9]", "_", mpm_val), ".png")),
         p, width = 14, height = 10, dpi = 300)
}

# ============================================================================
# PART 2: FUSED MPM RESULTS VISUALIZATIONS
# ============================================================================

# load MPM fusion results
fused_mpm <- as.data.table(nanoparquet::read_parquet(here("output/fused_mpm.parquet")))

# labels
fused_mpm[, level := fifelse(level == "RuralUrban", "Rural-Urban", level)]
fused_mpm[, mpm := fcase(
  mpm == "mpm",     "MPM",
  mpm == "mpm_af",  "MPM-AF",
  mpm == "al1_dep", "At least 1",
  mpm == "al3_dep", "At least 3"
)]

# ordering
s_list     <- c("p-c-r-e-w-s", "p-cr-ews", "pcr-ews","pcre-ws",
                "pce-rews","pcew-rews","pcews-rews")
level_list <- c("National", "Rural-Urban", "Quintile", "Subnational")
mpm_list   <- c("MPM", "MPM-AF", "At least 1", "At least 3")

fused_mpm[, scenario := factor(scenario, levels = s_list)]
fused_mpm[, level    := factor(level,    levels = level_list)]
fused_mpm[, mpm      := factor(mpm,      levels = mpm_list)]

dt_fused <- fused_mpm

# Filter for sim_bias == 1
dt_fused_bias1 <- dt_fused[sim_bias == 1]

# Get unique combinations
mpms_fused    <- unique(dt_fused_bias1$mpm)
levels_fused  <- unique(dt_fused_bias1$level)
estimates     <- c("pop_sh", "pred_p50_global", "pred_p50_local")
estimate_colors <- target_colors

# ============================================================================
# SCATTERPLOT 1: Estimate vs True by Scenario
# ============================================================================

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
        panel.grid.minor = element_blank()
      ) +
      guides(color = guide_legend(override.aes = list(size = 4), nrow = 1)) +
      labs(x = "True poverty (%)", y = "Predicted poverty (%)", color = "Scenario")

    ggsave(here("output", paste0("figures/MPM/scatter_all_", est, ".png")),
           p, width = 14, height = 10, dpi = 300)
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
        dt_plot[, error_pop_sh := pop_sh - pop_sh_true]
        dt_plot[, error_local  := pred_p50_local - pop_sh_true]
        dt_plot[, lower_pop_sh := lower_bound - pop_sh_true]
        dt_plot[, upper_pop_sh := upper_bound - pop_sh_true]
        dt_plot[, lower_local  := pred_p025_local - pop_sh_true]
        dt_plot[, upper_local  := pred_p975_local - pop_sh_true]

        p <- ggplot(dt_plot, aes(x = pop_sh_true)) +
          geom_linerange(aes(ymin = lower_pop_sh, ymax = upper_pop_sh, color = "pop_sh"),
                         alpha = 0.2, linewidth = 1.5) +
          geom_linerange(aes(ymin = lower_local, ymax = upper_local, color = "pred_p50_local"),
                         alpha = 0.2, linewidth = 1.5) +
          geom_point(aes(y = error_pop_sh, color = "pop_sh"),        alpha = 0.6, size = 1.5, shape = 16) +
          geom_point(aes(y = error_local,  color = "pred_p50_local"), alpha = 0.6, size = 1.5, shape = 16) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
          scale_color_manual(values = c("#fdae61", "#2c7bb6"),
                             labels = c("pop_sh"        = "Fused estimate w/ bounds",
                                        "pred_p50_local" = "Corrected (local) w/ 95% prediction intervals")) +
          scale_x_continuous(labels = function(x) x * 100) +
          scale_y_continuous(labels = function(x) x * 100) +
          theme_minimal(base_size = 16) +
          theme(
            legend.position = "top",
            panel.grid.minor = element_blank(),
            legend.text = element_text(size = 16),
            axis.text   = element_text(size = 16)
          ) +
          labs(x = "True poverty (%)", y = "Error (Predicted - True)", color = NULL)

        filename <- paste0("figures/MPM/errorplot_",
                           gsub("[^A-Za-z0-9]", "_", mpm_val), "_",
                           gsub("[^A-Za-z0-9]", "_", scenario_val), "_",
                           gsub("[^A-Za-z0-9]", "_", level_val), ".png")

        ggsave(here("output", filename), p, width = 12, height = 8, dpi = 300)
      }
    }
  }
}
