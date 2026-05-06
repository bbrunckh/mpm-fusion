# ============================================================================
# COMPARE RANK CORRELATIONS
# ============================================================================

# ============================================================================
# TRUE MONETARY POVERTY AND MPM (national level, by data scenario)
# ============================================================================

# Fusion MPM vs MPM rank correlation (national level, by data scenario)
mpm_val_national <- as.data.table(nanoparquet::read_parquet(here("output/fused_mpm_val_national.parquet")))
mpm_rho <- mpm_val_national[target == "pop_sh" & sim_bias == 1 & data_level == "National",
                             .(scenario, mpm, rho_fusion = rho)]

# Monetary poverty + MPM rank correlation (national level, by data scenario)

  # get survey level true mpm
  mpm <- as.data.table(nanoparquet::read_parquet(here("output/fused_mpm.parquet")))
  mpm <- mpm[sim_bias == 1 & level == "National",
             .(pop_sh_true = mean(pop_sh_true)), by = .(code, year, survname, mpm)]

  # get survey level monetary poverty
  pov <- as.data.table(nanoparquet::read_parquet(here("data/true_mpm_groups.parquet")))
  pov <- pov[level == "National",
             .(pov = sum(pop_sh[dep_poor1 == 1], na.rm = TRUE)),
             by = .(code, year, survname)]

  # merge
  pov_mpm <- merge(pov, mpm, by = c("code", "year", "survname"), all = FALSE)

  # compute rank correlation
  pov_rho <- as.data.table(compute_val_metrics(
    data           = pov_mpm,
    true_col_name  = "pop_sh_true",
    pred_col_names = "pov",
    equal_weights  = "code",
    group_vars     = "mpm"
  ))[, .(mpm, pov_rho = rho)]

  # N per mpm (number of surveys used in the correlation)
  n_per_mpm <- pov_mpm[, .(N = .N), by = mpm]

# compare rank correlations
rho_comparison <- merge(mpm_rho, pov_rho,  by = "mpm")
rho_comparison <- merge(rho_comparison, n_per_mpm, by = "mpm")
rho_comparison[, delta := rho_fusion - pov_rho]
setorder(rho_comparison, mpm, scenario)

# ---- relabel and order mpm --------------------------------------------------
mpm_levels  <- c("mpm", "mpm_af", "al1_dep", "al3_dep")
mpm_labels  <- c("MPM", "MPM-AF", "At least 1", "At least 3")
s_list <- c("p-c-r-e-w-s", "p-cr-ews", "pcr-ews", "pcre-ws",
            "pce-rews", "pcew-rews", "pcews-rews")

rho_comparison[, mpm := fcase(
  mpm == "mpm",     "MPM",
  mpm == "mpm_af",  "MPM-AF",
  mpm == "al1_dep", "At least 1",
  mpm == "al3_dep", "At least 3"
)]
rho_comparison[, mpm := factor(mpm, levels = mpm_labels)]
rho_comparison[, scenario := factor(scenario, levels = s_list)]
setorder(rho_comparison, mpm, scenario)

# ---- write Excel table ------------------------------------------------------
wb_rho <- createWorkbook()
addWorksheet(wb_rho, "Rank Correlations")

hdr_style   <- createStyle(textDecoration = "bold", halign = "center",
                            border = "TopBottom", borderColour = "#000000")
title_style <- createStyle(textDecoration = "bold", halign = "left")
num_style   <- createStyle(halign = "center", numFmt = "0.0000")
int_style   <- createStyle(halign = "center")
bot_style   <- createStyle(border = "Bottom", borderColour = "#000000")

col_names <- c("Scenario", "\u03c1 (Fused estimate)", "\u03c1 (Monetary poverty)", "\u0394", "N")

# single header row
writeData(wb_rho, "Rank Correlations",
          as.data.table(as.list(col_names)),
          startRow = 1, startCol = 1, colNames = FALSE)
addStyle(wb_rho, "Rank Correlations", hdr_style,
         rows = 1, cols = 1:5, gridExpand = TRUE)

current_row <- 2

for (lbl in mpm_labels) {

  tbl <- rho_comparison[mpm == lbl,
                        .(Scenario  = scenario,
                          rho_fusion = round(rho_fusion, 4),
                          pov_rho    = round(pov_rho,    4),
                          delta      = round(delta,      4),
                          N          = N)]
  setnames(tbl, col_names)

  n_data <- nrow(tbl)

  # title row
  writeData(wb_rho, "Rank Correlations", lbl,
            startRow = current_row, startCol = 1)
  addStyle(wb_rho, "Rank Correlations", title_style,
           rows = current_row, cols = 1)
  current_row <- current_row + 1

  # data rows
  writeData(wb_rho, "Rank Correlations", tbl,
            startRow = current_row, startCol = 1, colNames = FALSE)
  addStyle(wb_rho, "Rank Correlations", num_style,
           rows = current_row:(current_row + n_data - 1), cols = 2:4,
           gridExpand = TRUE)
  addStyle(wb_rho, "Rank Correlations", int_style,
           rows = current_row:(current_row + n_data - 1), cols = c(1, 5),
           gridExpand = TRUE)

  # bottom border on last data row
  addStyle(wb_rho, "Rank Correlations", bot_style,
           rows = current_row + n_data - 1, cols = 1:5,
           gridExpand = TRUE, stack = TRUE)

  current_row <- current_row + n_data
}

setColWidths(wb_rho, "Rank Correlations",
             cols = 1:5, widths = c(18, 22, 24, 10, 8))

saveWorkbook(wb_rho, here("output/tables/rank_correlations.xlsx"), overwrite = TRUE)

