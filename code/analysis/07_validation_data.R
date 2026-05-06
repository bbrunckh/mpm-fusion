# ============================================================================
# DISTRIBUTION OF SURVEYS USED IN VALIDATION BY REGION AND DECADE
# ============================================================================

# 573 surveys in raw validation data --> 571 used for national level MPM results:
  # MOZ 2008 and COL 2020 are dropped in final validation results because less
  # than 90% of (population weighted) sample have non-missing values across 
  # dimensions needed to compute MPM metrics. See 02_mpm_fusion.R.

#load final validation data, filter to national level, mpm target metric
fused_mpm <- as.data.table(nanoparquet::read_parquet(here("output/fused_mpm.parquet")))
fused_mpm <- fused_mpm[level == "National" & mpm == "mpm"]

# get distinct surveys in fused_mpm results
surveys <- unique(fused_mpm[, .(code, survname, year)])
setDT(surveys)

# get World Bank regions from pipr package
regions <- pipr::get_aux("countries")
setDT(regions)
setnames(regions, c("country_code"), c("code"))

# merge regions into surveys
surveys <- merge(surveys, regions[, .(code, region)], by = "code", all.x = TRUE)

# summarise number and % of surveys by region and decade (<2000, 2000s, 2010s, 2020s)

surveys[, decade := fcase(
  year < 2000, "Pre-2000",
  year < 2010, "2000s",
  year < 2020, "2010s",
  default     = "2020s"
)]
surveys[, decade := factor(decade, levels = c("Pre-2000", "2000s", "2010s", "2020s"))]

decade_cols <- c("Pre-2000", "2000s", "2010s", "2020s")

# ---- helper: build wide table with row/column totals and rename first col ----
make_wide_table <- function(dt, value_var, label_col = "Region") {
  wide <- dcast(dt, region ~ decade, value.var = value_var, fill = 0)
  wide[, Total := rowSums(.SD), .SDcols = decade_cols]
  tot  <- wide[, lapply(.SD, sum), .SDcols = c(decade_cols, "Total")]
  tot  <- cbind(data.table(region = "Total"), tot)
  wide <- rbindlist(list(wide, tot), use.names = TRUE)
  setnames(wide, "region", label_col)
  wide
}

# ---- 1. Survey counts -------------------------------------------------------
dist_surveys <- surveys[, .(n = .N), by = .(region, decade)]
dist_wide    <- make_wide_table(dist_surveys, "n")

# add % share of total surveys
n_total  <- nrow(surveys)
pct_cols <- paste0(decade_cols, "_pct")
dist_wide[, (pct_cols) := lapply(decade_cols, function(d) round(.SD[[d]] / n_total * 100, 1)),
          .SDcols = decade_cols]

# ---- 2. Country counts (distinct countries with >=1 survey) ----------------
dist_countries <- surveys[, .(n = uniqueN(code)), by = .(region, decade)]
ctry_wide      <- make_wide_table(dist_countries, "n")

# fix totals: sum across decades double-counts countries appearing in multiple
# decades, so recompute Total as distinct countries per region (and overall)
ctry_region_total <- surveys[, .(Total = uniqueN(code)), by = region]
ctry_wide[ctry_region_total, on = .(Region = region), Total := i.Total]
ctry_wide[Region == "Total", Total := uniqueN(surveys$code)]

# ---- write Excel workbook ---------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Validation data")

# styles
hdr_style   <- createStyle(
  fontColour = "#000000",
  halign = "center", textDecoration = "bold",
  border = "TopBottom", borderColour = "#000000"
)
title_style <- createStyle(textDecoration = "bold", halign = "left")
num_style   <- createStyle(halign = "center")
bot_style   <- createStyle(border = "Bottom", borderColour = "#000000")
tot_style   <- createStyle(textDecoration = "bold", halign = "center",
                           border = "Bottom", borderColour = "#000000")

all_cols  <- length(decade_cols) + 2
col_names <- c("Region", decade_cols, "Total")

# single shared header row
writeData(wb, "Validation data",
          as.data.table(as.list(col_names)),
          startRow = 1, startCol = 1, colNames = FALSE)
addStyle(wb, "Validation data", hdr_style, rows = 1, cols = 1:all_cols, gridExpand = TRUE)

current_row <- 2

write_dist_table <- function(tbl, title) {
  n_data <- nrow(tbl)

  # title row
  writeData(wb, "Validation data", title, startRow = current_row, startCol = 1)
  addStyle(wb, "Validation data", title_style, rows = current_row, cols = 1)
  current_row <<- current_row + 1

  # data rows
  writeData(wb, "Validation data", tbl,
            startRow = current_row, startCol = 1, colNames = FALSE)
  addStyle(wb, "Validation data", num_style,
           rows = current_row:(current_row + n_data - 1), cols = 2:all_cols,
           gridExpand = TRUE)

  # bold totals row + bottom border on last row
  addStyle(wb, "Validation data", tot_style,
           rows = current_row + n_data - 1, cols = 1:all_cols,
           gridExpand = TRUE, stack = TRUE)

  current_row <<- current_row + n_data
}

write_dist_table(
  ctry_wide[, c("Region", decade_cols, "Total"), with = FALSE],
  "Number of countries with at least one survey"
)

write_dist_table(
  dist_wide[, c("Region", decade_cols, "Total"), with = FALSE],
  "Number of surveys"
)

pct_table <- copy(dist_wide[, c("Region", pct_cols, "Total"), with = FALSE])
setnames(pct_table, pct_cols, paste0(decade_cols, " (%)"))
write_dist_table(pct_table, "Share of total surveys (%)")

setColWidths(wb, "Validation data", cols = 1:all_cols,
             widths = c(22, rep(10, all_cols - 1)))

saveWorkbook(wb, here("output/tables/validation_data.xlsx"), overwrite = TRUE)