#Install necessary packages
# install.packages(c("tidyverse", "dplyr", "readr", "ggplot2", "gridExtra", 
#                    "png", "mgcv", "tidyselect", "stringr", "readxl", 
#                    "openxlsx", "foreign", "broom", "knitr", "data.table", "dlm"))

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)

#######################################################
######################## BASIC USAGE #################################
#######################################################
## To run a complete welfare analysis with a specific combination of methods:
#############################################################################
# Source the integration script (which sources the other scripts)
source("second_pass/integration.R")

#To run analysis with all four combinations of methods:
#######################################################
all_results <- run_all_welfare_method_combinations(
  human_data_path = "dat/world_bank/world_bank_pop_gdp_clean.xlsx",
  farmed_animal_data_path = "first_pass/calc_tseries.xlsx",
  wild_animal_data_path = "second_pass/wild_calc_tseries.xlsx",
  output_base_dir = "second_pass/welfare_results",
  create_visualizations = TRUE
)


# Run analysis with isoelastic welfare levels and welfare range (WR) potential
results <- run_complete_welfare_analysis(
  human_data_path = "dat/world_bank/world_bank_pop_gdp_clean.xlsx",
  farmed_animal_data_path = "first_pass/calc_tseries.xlsx",
  wild_animal_data_path = "second_pass/wild_calc_tseries.xlsx",
  welfare_level_method = "isoelastic",
  welfare_potential_method = "WR",
  output_base_dir = "first_pass/welfare_results",
  create_visualizations = TRUE
)


