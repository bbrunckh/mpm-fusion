# Prepare multidimensional poverty validation data

# ============================================================================
# PREPARE VALIDATION DATA
# ============================================================================

# load raw distribution
gmd_data <- haven::read_dta(here("data/Survey_mpm_groups_true.dta"))

# get World Bank population from pipr package
pip_pop <- pipr::get_aux("pop")
setDT(pip_pop)
setnames(pip_pop, c("country_code", "value"), c("code", "pop"))
pip_pop <- pip_pop[data_level == "national", .(code, year = as.numeric(as.character(year)), pop)]

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

# add PIP population (use this instead of totalind)
gmd_data_clean <- merge(gmd_data_clean, pip_pop, by = c("code", "year"), all.x = TRUE)

# add population and population share of sample
gmd_data_clean[, `:=`(
  pop_sample = sum(sh_pop * pop, na.rm = TRUE),
  pop_sh     = sh_pop / sum(sh_pop, na.rm = TRUE)
), by = c("code", "year", "survname", "level", "sample")]

keep_cols <- c("code", "year", "survname", "level", "sample", "pop_sample", dep_cols, "pop_sh")
gmd_data_clean <- gmd_data_clean[, ..keep_cols]

nanoparquet::write_parquet(gmd_data_clean, here("data/true_mpm_groups.parquet"))
