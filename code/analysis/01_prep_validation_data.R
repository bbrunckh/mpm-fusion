# Prepare mdpov validation data

library(here)
library(dplyr)

#------------------------------------------------------------------------------#

# load raw distribution
gmd_data <- haven::read_dta(here("data/Survey_mpm_groups_true.dta"))

# get WDI population
wdi_pop <- wbstats::wb_data("SP.POP.TOTL") |>
  rename(code = iso3c, year = date, pop = SP.POP.TOTL) |>
  select(code, year, pop)

# clean up 
gmd_data_clean <- gmd_data |>
  mutate(
    level = case_when(
      level == "_all_" ~ "National",
      level == "_urban_" ~ "RuralUrban",
      level == "q5ind" ~ "Quintile",
      .default = "Subnational"),
    sample = case_when(
      level == "RuralUrban" & sample == "0.Rural" ~ "Rural",
      level == "RuralUrban" & sample == "1.Urban" ~ "Urban",
      level == "RuralUrban" & sample == "0"  ~ "Rural",
      level == "RuralUrban" & sample == "1" ~ "Urban",
      level == "RuralUrban" &sample == "" ~ "Unknown",
      level == "Subnational" & sample == "." ~ "Unknown",
        .default = sample),
    # replace -1 with NA = missing
    across(starts_with("dep_"), ~if_else(.x == - 1, NA, .))
    ) |>
  # add WDI population (use this instead of totalind)
  left_join(wdi_pop) |> 
  # add population and population share of sample
  group_by(code, year, survname, level, sample) |>
  mutate(pop_sample = sum(sh_pop * pop),
         pop_sh = sh_pop/sum(sh_pop)
         ) |>
  select(code, year, survname, level, sample, pop_sample,
         starts_with("dep"), pop_sh)

haven::write_dta(gmd_data_clean, here("data/true_mpm_groups.dta"))