#Install necessary packages
# install.packages(c("tidyverse", "dplyr", "readr", "ggplot2", "gridExtra", 
#                    "png", "mgcv", "tidyselect", "stringr", "readxl", 
#                    "openxlsx", "foreign", "broom", "knitr", "data.table", "dlm"))

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)


# Source the integration script (which sources the other scripts)
source("second_pass/integration.R")
source("second_pass/welfare_analysis_framework.R")

# Run analysis with all four combinations of methods:
all_results <- run_all_welfare_method_combinations(
  human_data_path = "dat/world_bank/world_bank_pop_gdp_clean.xlsx",
  farmed_animal_data_path = "first_pass/calc_tseries.xlsx",
  wild_animal_data_path = "second_pass/wild_calc_tseries.xlsx",
  output_base_dir = "third_pass/welfare_results",
  create_visualizations = TRUE
)
