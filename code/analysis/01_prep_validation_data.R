# Prepare multidimensional poverty validation data

# ============================================================================
# PREPARE VALIDATION DATA
# ============================================================================

# load raw distribution
gmd_data <- haven::read_dta(here("data/Survey_mpm_groups_true.dta"))

# get WDI population
wdi_pop <- as.data.table(wbstats::wb_data("SP.POP.TOTL"))
setnames(wdi_pop, c("iso3c", "date", "SP.POP.TOTL"), c("code", "year", "pop"))
wdi_pop <- wdi_pop[, .(code, year, pop)]

# clean variables 
gmd_data_clean <- as.data.table(gmd_data)

gmd_data_clean[, level := fcase(
  level == "_all_",   "National",
  level == "_urban_", "RuralUrban",
  level == "q5ind",   "Quintile",
  default = "Subnational"
)]

gmd_data_clean[, sample := fcase(
  level == "RuralUrban" & sample == "0.Rural", "Rural",
  level == "RuralUrban" & sample == "1.Urban", "Urban",
  level == "RuralUrban" & sample == "0",       "Rural",
  level == "RuralUrban" & sample == "1",       "Urban",
  level == "RuralUrban" & sample == "",        "Unknown",
  level == "Subnational" & sample == ".",      "Unknown",
  default = sample
)]

# replace -1 with NA = missing
dep_cols <- grep("^dep_", names(gmd_data_clean), value = TRUE)
gmd_data_clean[, (dep_cols) := lapply(.SD, function(x) fifelse(x == -1, NA_real_, x)),
               .SDcols = dep_cols]

# add WDI population (use this instead of totalind)
gmd_data_clean <- merge(gmd_data_clean, wdi_pop, by = c("code", "year"), all.x = TRUE)

# add population and population share of sample
gmd_data_clean[, `:=`(
  pop_sample = sum(sh_pop * pop, na.rm = TRUE),
  pop_sh     = sh_pop / sum(sh_pop, na.rm = TRUE)
), by = c("code", "year", "survname", "level", "sample")]

keep_cols <- c("code", "year", "survname", "level", "sample", "pop_sample", dep_cols, "pop_sh")
gmd_data_clean <- gmd_data_clean[, ..keep_cols]

nanoparquet::write_parquet(gmd_data_clean, here("data/true_mpm_groups.parquet"))
