# Complete Welfare Analysis Integration
# This script integrates the human welfare calculation with animal welfare analysis

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)

# Source required functions
source("second_pass/welfare_analysis_framework.R")  # Main analysis functions
source("second_pass/human_welfare_module.R")  # Human welfare calculations

#' Run the complete welfare analysis pipeline
#'
#' @param human_data_path Path to the World Bank human data
#' @param animal_data_path Path to the raw animal data
#' @param welfare_level_method Method for welfare level calculation ("isoelastic" or "3282")
#' @param welfare_potential_method Method for welfare potential calculation ("NC" or "WR")
#' @param output_base_dir Base directory for outputs
#' @param create_visualizations Whether to create visualizations
#' @return List of results from the analysis
run_complete_welfare_analysis <- function(human_data_path,
                                          animal_data_path,
                                          welfare_level_method = "isoelastic",
                                          welfare_potential_method = "WR",
                                          output_base_dir = "welfare_analysis_results",
                                          create_visualizations = TRUE) {
  
  # Create method-specific output directory
  output_dir <- file.path(output_base_dir, paste0(welfare_level_method, "_", welfare_potential_method))
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Step 1: Calculate human welfare
  cat("Step 1: Calculating human welfare levels using", welfare_level_method, "method...\n")
  human_welfare_dir <- file.path(output_dir, "human_welfare")
  
  human_welfare_results <- calculate_human_welfare_levels(
    data_path = human_data_path,
    method = welfare_level_method,
    output_dir = human_welfare_dir,
    visualize = create_visualizations
  )
  
  # Step 2: Prepare human welfare data for integration
  cat("Step 2: Preparing human welfare data for integration...\n")
  human_data_formatted <- prepare_human_data_for_integration(
    human_welfare_results,
    output_file = file.path(output_dir, "human_data_formatted.xlsx")
  )
  
  # Step 3: Read and preprocess animal data
  cat("Step 3: Reading and preprocessing animal data...\n")
  animal_data <- read_excel(animal_data_path)
  
  # Step 4: Integrate human and animal data
  cat("Step 4: Integrating human and animal data...\n")
  
  # Extract needed columns from animal data
  # This assumes animal data has at least these columns: Year, Category, Group, aliveatanytime
  required_cols <- c("Year", "Category", "Group", "aliveatanytime")
  animal_cols <- names(animal_data)
  
  # Check if all required columns exist
  if(!all(required_cols %in% animal_cols)) {
    missing_cols <- setdiff(required_cols, animal_cols)
    stop("Missing required columns in animal data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create integrated dataset
  if("Humans" %in% animal_data$Category) {
    # If humans already in the dataset, replace with new welfare data
    integrated_data <- animal_data %>%
      filter(Category != "Humans") %>%
      bind_rows(human_data_formatted)
  } else {
    # If no humans in the dataset, simply append
    integrated_data <- bind_rows(animal_data, human_data_formatted)
  }
  
  # Step 5: Ensure dataset has required potential and utility columns
  cat("Step 5: Calculating welfare potential and utility metrics...\n")
  
  # Ensure we have all necessary columns for both methods - always ensure both
  # sets of columns exist to avoid missing column errors in analysis
  integrated_data <- ensure_nc_columns(integrated_data)
  integrated_data <- ensure_wr_columns(integrated_data)
  
  # Save integrated data
  write.xlsx(integrated_data, file.path(output_dir, "second_pass/integrated_calc_tseries.xlsx"))
  
  # Step 6: Run analysis on integrated data
  cat("Step 6: Running welfare analysis on integrated data...\n")
  analysis_results <- analyze_welfare_data(
    data_path = file.path(output_dir, "integrated_calc_tseries.xlsx"),
    welfare_level_method = welfare_level_method,
    welfare_potential_method = welfare_potential_method,
    output_dir = file.path(output_dir, "analysis_results"),
    create_visualizations = create_visualizations
  )
  
  cat("\nComplete welfare analysis pipeline completed successfully!\n")
  return(list(
    human_welfare = human_welfare_results,
    integrated_data = integrated_data,
    analysis_results = analysis_results
  ))
}

#' Run all method combinations in a single function
#'
#' @param human_data_path Path to the World Bank human data
#' @param animal_data_path Path to the raw animal data
#' @param output_base_dir Base directory for outputs
#' @param create_visualizations Whether to create visualizations
#' @return List of results from all analyses
run_all_welfare_method_combinations <- function(human_data_path,
                                                animal_data_path,
                                                output_base_dir = "welfare_analysis_results",
                                                create_visualizations = TRUE) {
  
  welfare_level_methods <- c("isoelastic", "3282")
  welfare_potential_methods <- c("WR", "NC")
  
  results_list <- list()
  
  for(wl_method in welfare_level_methods) {
    for(wp_method in welfare_potential_methods) {
      cat("\n\n========================================================\n")
      cat(paste0("Running analysis with welfare level method '", wl_method, 
                 "' and welfare potential method '", wp_method, "'...\n"))
      cat("========================================================\n\n")
      
      # Run complete analysis with this combination
      results <- run_complete_welfare_analysis(
        human_data_path = human_data_path,
        animal_data_path = animal_data_path,
        welfare_level_method = wl_method,
        welfare_potential_method = wp_method,
        output_base_dir = output_base_dir,
        create_visualizations = create_visualizations
      )
      
      # Store results
      results_list[[paste0(wl_method, "_", wp_method)]] <- results
      
      cat(paste0("\nAnalysis complete for '", wl_method, "_", wp_method, "'.\n"))
    }
  }
  
  return(results_list)
}

# Example usage:
# # Run a single analysis combination
# results_isoelastic_WR <- run_complete_welfare_analysis(
#   human_data_path = "dat/world_bank/world_bank_pop_gdp_clean.xlsx",
#   animal_data_path = "first_pass/calc_tseries.xlsx",
#   welfare_level_method = "isoelastic",
#   welfare_potential_method = "WR",
#   output_base_dir = "first_pass/welfare_results",
#   create_visualizations = TRUE
# )
# 
# # Or run all combinations at once
# all_results <- run_all_welfare_method_combinations(
#   human_data_path = "dat/world_bank/world_bank_pop_gdp_clean.xlsx",
#   animal_data_path = "first_pass/calc_tseries.xlsx",
#   output_base_dir = "first_pass/welfare_results",
#   create_visualizations = TRUE
# )