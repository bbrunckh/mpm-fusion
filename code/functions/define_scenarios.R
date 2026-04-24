# Ensure the necessary libraries are loaded
# You might need to run install.packages(c("tibble", "dplyr", "purrr", "stringr"))
library(tibble)
library(dplyr)
library(purrr)
library(stringr)

define_scenarios <- function(scenario_strings) {
  
  # Define the mapping from single letters to full variable names
  indicator_map <- c(
    p = "dep_poor1",
    c = "dep_educ_com",
    r = "dep_educ_enr",
    e = "dep_infra_elec",
    w = "dep_infra_impw",
    s = "dep_infra_imps"
  )
  
  # Validate the input characters
  all_chars <- scenario_strings %>%
    str_remove_all("-") %>%
    str_split("") %>%
    unlist() %>%
    unique()
  
  invalid_chars <- setdiff(all_chars, names(indicator_map))
  
  if (length(invalid_chars) > 0) {
    warning_message <- paste(
      "Invalid characters detected in input and will be ignored:",
      paste(invalid_chars, collapse = ", ")
    )
    warning(warning_message)
  }
  
  # Process the scenario strings to build the tibble
  # The map_dfr function iterates over each scenario string and stacks the results
  # into a single data frame.
  scenario_tibble <- map_dfr(scenario_strings, function(scenario) {
    
    # Split the scenario string by the hyphen to get data sources
    data_sources <- str_split(scenario, "-", simplify = TRUE) %>% as.character()
    
    # For each data source, create a row in a temporary tibble
    map_dfr(data_sources, function(source) {
      
      # Split the data source string into individual characters
      indicators <- str_split(source, "")[[1]]
      
      # Translate the characters to full variable names using the map
      # and filter out any invalid (unmapped) characters
      indicator_list <- unname(indicator_map[indicators])
      indicator_list <- indicator_list[!is.na(indicator_list)]
      
      # Create a one-row tibble for the current data source
      tibble(
        scenario = scenario,
        data_source = source,
        ind_list = list(indicator_list)
      )
    })
  })
  
  return(scenario_tibble)
}

#------------------------------------------------------------------------------#
# # Example Use
# 
# # Define the input vector of scenario strings
# scenarios_input <- c("pcr-ews", "pcre-ews")
# 
# # Call the function to generate the tibble
# simulation_scenarios <- define_scenarios(scenarios_input)
# 
# # Print the resulting tibble to the console
# print(simulation_scenarios)