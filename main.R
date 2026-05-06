# main.R - Fusing data sources to measure multidimensional poverty
#
# See README.md for full instructions to replicate.

# Restore R environment
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()

# Set root path (change this line for your machine)
root <- here::here()

# Source all function definitions
fn_path <- file.path(root, "code", "functions")
for (f in list.files(fn_path, pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# Load packages
library(here)
library(data.table)
library(stringr)
library(ggplot2)
library(openxlsx)
library(scales)
library(highs)
library(dbscan)

# Run analysis pipeline
analysis_path <- file.path(root, "code", "analysis")

  # Prep validation data
  source(file.path(analysis_path, "01_prep_validation_data.R"))

  # MPM fusion (main text)
  source(file.path(analysis_path, "02_mpm_fusion.R"))
  source(file.path(analysis_path, "03_mpm_fusion_results.R"))

  # Infrastructure fusion (annex)
  source(file.path(analysis_path, "04_inf_fusion.R"))
  source(file.path(analysis_path, "05_inf_fusion_results.R"))

  # Compare rank correlations (annex)
  source(file.path(analysis_path, "06_compare_rank_correlations.R"))

  # Distribution of surveys by region and decade (annex)
  source(file.path(analysis_path, "07_survey_distribution.R"))
