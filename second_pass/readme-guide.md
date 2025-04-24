# Welfare Analysis Framework: User Guide

This guide will walk you through the modular welfare analysis framework I've created for analyzing human and animal welfare data with different quantification methods.

## Overview

The framework allows you to:

1. Process both human and animal welfare data
2. Choose between different welfare level quantification methods:
   - **Isoelastic utility**: Continuous welfare level using isoelastic utility function
   - **32-82 method**: Binary welfare level (32 or 82) based on GDP threshold
3. Choose between different welfare potential quantification methods:
   - **WR (Welfare Range)**: Using welfare range potential values
   - **NC (Neuron Count)**: Using neuron count-based potential values
4. Generate standardized analyses and visualizations

## File Structure

The framework consists of three main script files:

1. `welfare_analysis_framework.R`: Core functions for processing and analyzing welfare data
2. `human_welfare_module.R`: Functions for calculating human welfare levels
3. `integration_script.R`: Functions for integrating human and animal welfare analyses

## Getting Started

### Installation

1. Save all three script files to your working directory
2. Make sure you have the required R packages installed:

```r
install.packages(c("tidyverse", "dplyr", "readr", "ggplot2", "gridExtra", 
                  "png", "mgcv", "tidyselect", "stringr", "readxl", 
                  "openxlsx", "foreign", "broom", "knitr", "data.table", "dlm"))
```

### Basic Usage

To run a complete welfare analysis with a specific combination of methods:

```r
# Source the integration script (which sources the other scripts)
source("integration_script.R")

# Run analysis with isoelastic welfare levels and welfare range (WR) potential
results <- run_complete_welfare_analysis(
  human_data_path = "dat/world_bank/world_bank_pop_gdp_clean.xlsx",
  animal_data_path = "first_pass/calc_tseries.xlsx",
  welfare_level_method = "isoelastic",
  welfare_potential_method = "WR",
  output_base_dir = "first_pass/welfare_results",
  create_visualizations = TRUE
)
```

To run analysis with all four combinations of methods:

```r
all_results <- run_all_welfare_method_combinations(
  human_data_path = "dat/world_bank/world_bank_pop_gdp_clean.xlsx",
  animal_data_path = "first_pass/calc_tseries.xlsx",
  output_base_dir = "first_pass/welfare_results",
  create_visualizations = TRUE
)
```

## Input Data Requirements

### Human Welfare Data (World Bank data)

Required columns:
- `Year`: Year of observation
- `Country`: Country name
- `Population`: Population count
- `GDP_filled`: GDP per capita (PPP, current international $)

### Animal Welfare Data (calc_tseries)

Required columns:
- `Year`: Year of observation
- `Category`: Animal category (e.g., "Chickens", "Cattle")
- `Group`: Animal group classification
- `aliveatanytime`: Population count
- `Welfare_level`: Welfare level score
- `WR_potential`: Welfare range potential value (if using WR method)
- `forebrain_neurons`: Forebrain neuron count (if using NC method)

## Output Structure

For each analysis, the following outputs are generated:

1. Human welfare calculations:
   - `human_wWL_[method].xlsx`: Human welfare levels by year

2. Integrated data:
   - `integrated_calc_tseries.xlsx`: Combined human and animal data with calculated metrics

3. Analysis results:
   - `net_series_[methods].xlsx`: Net utility and population by year
   - `net_series_nh_[methods].xlsx`: Net utility and population by year (excluding humans)
   - `cor_and_elas_[methods].xlsx`: Correlations and elasticities between human and animal metrics
   - `f_change_[methods].xlsx`: Factor changes in population over time

4. Visualizations:
   - Population trends
   - Utility trends
   - Net utility trends
   - Comparative analyses

## Method Details

### Welfare Level Methods

#### Isoelastic Utility Method
Uses a continuous function to calculate welfare levels based on GDP:
```
welfare_level = ubar + (GDP^(1-gamma))/(1-gamma)
```
Where `ubar = -22.1713` and `gamma = 0.674252`

#### 32-82 Method
Assigns welfare level scores based on a GDP threshold:
- If GDP ≤ threshold: Welfare level = 32
- If GDP > threshold: Welfare level = 82

Where threshold is the log midpoint between India's and Canada's 2018 GDP.

### Welfare Potential Methods

#### Welfare Range (WR) Method
Uses predefined welfare range potential values for each category.

#### Neuron Count (NC) Method
Calculates welfare potential as:
```
welfare_potential = forebrain_neurons / human_forebrain_neurons
```

## Example Analysis

Our demonstration with sample data showed how different welfare potential methods can lead to very different conclusions:

With WR method (2010):
- Humans: 108.07% contribution to total utility
- Chickens: -11.00% contribution (negative due to negative welfare)
- Cattle: 2.93% contribution

With NC method (2010):
- Humans: 99.99% contribution to total utility
- Chickens: -0.15% contribution
- Cattle: 0.16% contribution

This illustrates how the choice of welfare potential method significantly impacts the relative weight given to different species in your analysis.

## Customization

The framework is designed to be modular and extensible. You can:

1. Add new welfare level calculation methods
2. Add new welfare potential calculation methods
3. Create custom visualizations
4. Extend the analysis to include other metrics

## Troubleshooting

Common issues:

1. **Missing columns**: Ensure your input data has all required columns
2. **Data inconsistencies**: Check that data types are consistent (e.g., numeric values for populations)
3. **Year mismatches**: Verify that human and animal data have overlapping years

For additional support, refer to the detailed comments within each script file.
