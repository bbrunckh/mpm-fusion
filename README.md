# Reproducibility Package

## Overview

This package contains the code and data to reproduce the results in **Fusing data sources to measure multidimensional poverty** by Ben J. Brunckhorst, Minh C. Nguyen, Nishant Yonzan, Hai-Anh Dang & Christoph Lakner. 

To replicate, follow the [Instructions for Replicators](#instructions-for-replicators) below.

---

## Data Availability
Some data cannot be made publicly available.

### Data Sources

- **Filename:** `Survey_mpm_groups_true.dta`
  - **Source:** Global Monitoring Database (GMD), World Bank
  - **Access year:** 2025
  - **Access:** Restricted

- **Filename:** World Bank population data (accessed programmatically)
  - **Source:** PIP
  - **URL:** 
  - **Access year:** 2026
  - **License:** Creative Commons Attribution 4.0 International license (CC-BY 4.0)
  - **Access:** Accessed via the `pipr` R package

The authors of the manuscript have legitimate access to and permission to use the data used in this manuscript.

---

## Instructions for Replicators

1. Users need access to the input data file: `Survey_mpm_groups_true.dta`. If not included in the package, place it in the `data/` folder.

2. Run `main.R`
  - `renv::restore()` will install all required R packages at the exact versions used. 
  - If needed, update the `root` path to the package directory (by default it uses `here::here()` which auto-detects the project root).

---

## List of Exhibits

The provided code reproduces all tables and figures in the paper.

| Exhibit name | Output filename | Script | Note |
|---|---|---|---|
| [TODO: match to manuscript exhibit number] | `mpm_summary_tables.xlsx` | `code/analysis/03_mpm_fusion_results.R` | Found in `output/tables/` |
| [TODO: match to manuscript exhibit number] | `mpm_extended_tables.xlsx` | `code/analysis/03_mpm_fusion_results.R` | Found in `output/tables/` |
| [TODO: match to manuscript exhibit number] | `inf_summary_tables.xlsx` | `code/analysis/05_inf_fusion_results.R` | Found in `output/tables/` |
| [TODO: match to manuscript exhibit number] | `inf_extended_tables.xlsx` | `code/analysis/05_inf_fusion_results.R` | Found in `output/tables/` |
| [TODO: match to manuscript exhibit number] | Heatplot(s) | `code/analysis/03_mpm_fusion_results.R` | Found in `output/figures/MPM/` |
| [TODO: match to manuscript exhibit number] | Lineplot(s) | `code/analysis/03_mpm_fusion_results.R` | Found in `output/figures/MPM/` |
| [TODO: match to manuscript exhibit number] | Scatterplot(s) | `code/analysis/03_mpm_fusion_results.R` | Found in `output/figures/MPM/` |
| [TODO: match to manuscript exhibit number] | Errorplot(s) | `code/analysis/03_mpm_fusion_results.R` | Found in `output/figures/MPM/` |
| [TODO: match to manuscript exhibit number] | Heatplot(s) | `code/analysis/05_inf_fusion_results.R` | Found in `output/figures/INF/` |
| [TODO: match to manuscript exhibit number] | Lineplot(s) | `code/analysis/05_inf_fusion_results.R` | Found in `output/figures/INF/` |
| [TODO: match to manuscript exhibit number] | Scatterplot(s) | `code/analysis/05_inf_fusion_results.R` | Found in `output/figures/INF/` |
| [TODO: match to manuscript exhibit number] | Errorplot(s) | `code/analysis/05_inf_fusion_results.R` | Found in `output/figures/INF/` |

---

## Requirements

### Software

- **R version 4.5.3 (2026-03-11)**, available at https://cran.r-project.org/
  - Platform: aarch64-apple-darwin20
  - Earlier versions of R can be used
  - All package versions are managed via `renv`. 
  - Key R packages: `data.table`, `ggplot2`, `haven`, `here`, `highs`, `Matrix`, `nanoparquet`, `openxlsx`, `pipr`, `Hmisc`, `renv`, `scales`, `stringr`

### Operating System

- Run on macOS 26.4.1 (M2 chip, 16GB memory)

### Runtime

- 245 minutes

### Storage

- The total size of all outputs and code is [TODO: approximate total size]

## Code Description

### `code/functions/` — Pure function definitions

| File | Purpose |
|---|---|
| `mpm_fns.R` | Core poverty metric functions: `atleast_k_deps()`, `atleast_k_dims()`, `weighted_headcount()`, `adjusted_headcount()` |
| `compute_mpm.R` | Computes MPM point estimates with validity flags |
| `fuse_indicators.R` | Fuses probability distributions under conditional independence; computes Fréchet-Hoeffding (lower/upper) bounds |
| `define_scenarios.R` | Parses scenario strings (e.g., `"p-c-r-e-w-s"`) into indicator subsets |
| `mpm_bounds.R` | Solves LP problems (HiGHS) to get theoretical min/max bounds on poverty metrics |
| `conformal_intervals_loo.R` | Leave-one-out conformal prediction intervals (global and localized) |
| `validation_metrics.R` | `compute_val_metrics()`: bias, MAE, MAPE, RMSE, correlation, MPIW |
| `run_fusion_scenarios.R` | Orchestrates the loop over fusion scenarios and bias factors |
| `run_validation.R` | Shared validation pipeline: computes level and national metrics, saves outputs |
| `results_tables.R` | Shared Excel workbook generation (`create_summary_workbook`, `create_extended_workbook`) |

### `code/analysis/` — Pipeline scripts (executed in order by `main.R`)

| File | Purpose |
|---|---|
| `01_prep_validation_data.R` | Loads GMD survey data, merges WDI population data, computes weights |
| `02_mpm_fusion.R` | Runs MPM fusion across 7 scenarios × 11 bias factors; saves `fused_mpm.parquet`; computes MAE, MAPE, RMSE, and coverage metrics |
| `03_mpm_fusion_results.R` | Exports MPM summary/extended tables (Excel) and figures (heatplots, lineplots, scatterplots, errorplots) |
| `04_inf_fusion.R` | Mirrors `02_mpm_fusion.R` for infrastructure indicators (electricity, water, sanitation); includes validation metrics |
| `05_inf_fusion_results.R` | Exports infrastructure tables and figures |
| `06_compare_rank_correlations.R` | Generates table comparing rank correlations |
| `07_validation_data.R` | Generates table summarizing validation data by region and decade |

---

## Folder Structure

```
mpm-fusion/
├── main.R                          # Entry point: run this to replicate all results
├── README.md                       # This file
├── LICENSE
├── data/
│   └── Survey_mpm_groups_true.dta  # GMD survey microdata [TODO: confirm inclusion]
├── code/
│   ├── functions/                  # Pure function definitions (sourced by main.R)
│   │   ├── mpm_fns.R
│   │   ├── compute_mpm.R
│   │   ├── fuse_indicators.R
│   │   ├── define_scenarios.R
│   │   ├── mpm_bounds.R
│   │   ├── conformal_intervals_loo.R
│   │   ├── validation_metrics.R
│   │   ├── run_fusion_scenarios.R
│   │   ├── run_validation.R
│   │   └── results_tables.R
│   └── analysis/                   # Pipeline scripts (run in numbered order)
│       ├── 01_prep_validation_data.R
│       ├── 02_mpm_fusion.R
│       ├── 03_mpm_fusion_results.R
│       ├── 04_inf_fusion.R
│       ├── 05_inf_fusion_results.R
│       ├── 06_compare_rank_correlations.R  
│       └── 07_validation_data.R
└── output/
    ├── tables/                    # Excel output tables
    └── figures/
       ├── MPM/                    # MPM fusion figures
       └── INF/                    # Infrastructure fusion figures

```

---

## References

Barrett T, Dowle M, Srinivasan A, Gorecki J, Chirico M, Hocking T, Schwendinger B (2025). *data.table: Extension of `data.frame`*. R package version 1.18.2.1. URL https://r-datatable.com.

Hahsler M, Piekenbrock M, Doran D (2019). dbscan: Fast Density-Based Clustering with R. *Journal of Statistical Software*, 91(1), 1–30. doi:10.18637/jss.v091.i01.

Huangfu Q, Hall JAJ (2018). Parallelizing the dual revised simplex method. *Mathematical Programming Computation*, 10(1), 119–142. doi:10.1007/s12532-017-0130-5.

R Core Team (2026). *R: A Language and Environment for Statistical Computing*. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Schumacher D (2025). *highs: R Interface to HiGHS*. R package version 1.12.0-3. URL https://cran.r-project.org/package=highs.

Wickham H (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag New York. ISBN 978-3-319-24277-4. URL https://ggplot2.tidyverse.org.
