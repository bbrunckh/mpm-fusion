# ============================================================================
# VALIDATION RESULTS
# ============================================================================

# ============================================================================
# PART 1: VALIDATION METRICS ANALYSIS
# ============================================================================

# load data
mpm_val_national <- as.data.table(nanoparquet::read_parquet(here("output/fused_inf_val_national.parquet")))

# labels
mpm_val_national[, target := fcase(
  target == "pop_sh",          "Fused estimate",
  target == "pred_p50_global", "Corrected (global)",
  target == "pred_p50_local",  "Corrected (local)"
)]
mpm_val_national[, data_level := fifelse(data_level == "RuralUrban", "Rural-Urban", data_level)]
mpm_val_national[, mpm := fcase(
  mpm == "al1_dep",  "At least 1",
  mpm == "al2_dep",  "At least 2",
  mpm == "all3_dep", "All 3"
)]
setnames(mpm_val_national, "n", "N")

# ordering
s_list     <- c("e-w-s", "e-ws", "es-w", "ew-s", "es-ws", "ew-es", "ew-ws")
est_list   <- c("Fused estimate", "Corrected (global)", "Corrected (local)")
level_list <- c("National", "Rural-Urban", "Quintile", "Subnational")
mpm_list   <- c("At least 1", "At least 2", "All 3")

mpm_val_national[, scenario   := factor(scenario,   levels = s_list)]
mpm_val_national[, target     := factor(target,     levels = est_list)]
mpm_val_national[, data_level := factor(data_level, levels = level_list)]
mpm_val_national[, mpm        := factor(mpm,        levels = mpm_list)]

dt_val <- mpm_val_national

# ============================================================================
# SUMMARY TABLES - Export to Excel with formatting
# ============================================================================

# Filter for sim_bias == 1
dt_bias1 <- dt_val[sim_bias == 1]

create_summary_workbook(dt_bias1, "output/tables/inf_summary_tables.xlsx")

# ============================================================================
# EXTENDED TABLES - One per MPM
# ============================================================================

create_extended_workbook(dt_bias1, "output/tables/inf_extended_tables.xlsx")

# ============================================================================
# FIGURES - Define consistent color palettes (colorblind-friendly)
# ============================================================================

scenario_colors <- c(
  "e-w-s" = "#440154",  # Dark Purple
  "e-ws"  = "#3b528b",  # Blue-Purple
  "es-w"  = "#21918c",  # Teal
  "ew-s"  = "#5ec962",  # Green (center)
  "es-ws" = "#fde725",  # Yellow
  "ew-es" = "#fd9668",  # Orange
  "ew-ws" = "#b63679"   # Pink-Red
)

target_colors <- c(
  "Fused estimate"     = "#3b528b",  # Blue-Purple
  "Corrected (global)" = "#fd9668",  # Orange
  "Corrected (local)"  = "#b63679"   # Pink-Red
)

mpm_colors <- c(
  "At least 1" = "#3b528b",  # Blue-Purple
  "At least 2" = "#fd9668",  # Orange
  "All 3"      = "#b63679"   # Pink-Red
)

# ============================================================================
# HEATPLOT 1: Scenarios x Data_level for each metric and mpm
# ============================================================================

mpms        <- c("At least 1")
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
        axis.text    = element_text(size = 16),
        axis.title   = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 16),
        strip.text   = element_text(size = 16)
      )

    ggsave(here("output", paste0("figures/INF/heatplot_", mpm_val, "_", metric, ".png")),
           p, width = 12, height = 8, dpi = 300)
  }
}

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
  theme(axis.text.x    = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y    = element_text(size = 20),
        axis.title.x   = element_text(size = 20),
        axis.title.y   = element_text(size = 20),
        strip.text.x   = element_text(size = 20),
        strip.text.y   = element_text(size = 20),
        legend.position = "bottom",
        legend.text    = element_text(size = 20),
        plot.title     = element_text(hjust = 0.5)) +
  labs(x = "Simulated Bias", y = "MAE", color = "Scenario")

ggsave(here("output/figures/INF/lineplot_mae_samplebias_national.png"),
       p2, width = 16, height = 12, dpi = 300)

# ============================================================================
# PART 2: FUSED INF RESULTS VISUALIZATIONS
# ============================================================================

# load infrastructure fusion results
fused_mpm <- as.data.table(nanoparquet::read_parquet(here("output/fused_inf.parquet")))

# labels
fused_mpm[, level := fifelse(level == "RuralUrban", "Rural-Urban", level)]
fused_mpm[, mpm := fcase(
  mpm == "al1_dep",  "At least 1",
  mpm == "al2_dep",  "At least 2",
  mpm == "all3_dep", "All 3"
)]

# ordering
s_list     <- c("e-w-s", "e-ws", "es-w", "ew-s", "es-ws", "ew-es", "ew-ws")
level_list <- c("National", "Rural-Urban", "Quintile", "Subnational")
mpm_list   <- c("At least 1", "At least 2", "All 3")

fused_mpm[, scenario := factor(scenario, levels = s_list)]
fused_mpm[, level    := factor(level,    levels = level_list)]
fused_mpm[, mpm      := factor(mpm,      levels = mpm_list)]

dt_fused <- fused_mpm

# Filter for sim_bias == 1
dt_fused_bias1 <- dt_fused[sim_bias == 1]

# Get unique combinations
mpms_fused   <- unique(dt_fused_bias1$mpm)
levels_fused <- unique(dt_fused_bias1$level)
estimates    <- c("pop_sh", "pred_p50_local")

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
        legend.position  = "bottom",
        panel.grid.minor = element_blank()
      ) +
      guides(color = guide_legend(override.aes = list(size = 4), nrow = 1)) +
      labs(x = "True poverty (%)", y = "Predicted poverty (%)", color = "Scenario")

    ggsave(here("output", paste0("figures/INF/scatter_all_", est, ".png")),
           p, width = 14, height = 10, dpi = 300)
  }
}

# ============================================================================
# SCATTERPLOT with bounds: Error vs True with Prediction Intervals
# ============================================================================

for (mpm_val in c("At least 1")) {
  for (scenario_val in c("ew-ws")) {
    for (level_val in c("Subnational")) {

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
                             labels = c("pop_sh"         = "Fused estimate w/ bounds",
                                        "pred_p50_local"  = "Corrected (local) w/ 95% prediction intervals")) +
          scale_x_continuous(labels = function(x) x * 100) +
          scale_y_continuous(labels = function(x) x * 100) +
          theme_minimal(base_size = 16) +
          theme(
            legend.position  = "top",
            panel.grid.minor = element_blank(),
            legend.text      = element_text(size = 16),
            axis.text        = element_text(size = 16)
          ) +
          labs(x = "True poverty (%)", y = "Error (Predicted - True)", color = NULL)

        filename <- paste0("figures/INF/errorplot_",
                           gsub("[^A-Za-z0-9]", "_", mpm_val), "_",
                           gsub("[^A-Za-z0-9]", "_", scenario_val), "_",
                           gsub("[^A-Za-z0-9]", "_", level_val), ".png")

        ggsave(here("output", filename), p, width = 12, height = 8, dpi = 300)
      }
    }
  }
}
