# Complete Welfare Analysis Integration
# This script integrates the human welfare calculation with animal welfare analysis

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)

# Source required functions
source(paste0(pass_number, "/welfare_analysis_framework.R"))  # Main analysis functions
source(paste0(pass_number, "/human_welfare_module.R"))  # Human welfare calculations
source(paste0(pass_number, "/wild_animal_module.R"))  # Wild animal welfare calculations
source(paste0(pass_number, "/extend_data.R")) # Extends and truncates data in preparation for net series

#' Run the complete welfare analysis pipeline
#'
#' @param human_data_path Path to the World Bank human data
#' @param farmed_animal_data_path Path to the raw animal data
#' @param wild_animal_data_path Path to the raw wild animal data
#' @param welfare_level_method Method for welfare level calculation ("isoelastic" or "3282")
#' @param welfare_potential_method Method for welfare potential calculation (set to "ALL" for both NC and WR)
#' @param pass_number Which pass of coding we're up to. Passes straight through and is only relevant for prepare-data-for-net-series
#' @param output_base_dir Base directory for outputs
#' @param create_visualizations Whether to create visualizations
#' @param skip_population_plots Whether to skip population trend visualizations
#' @return List of results from the analysis
run_complete_welfare_analysis <- function(human_data_path,
                                          farmed_animal_data_path,
                                          wild_animal_data_path,
                                          welfare_level_method = "isoelastic",
                                          welfare_potential_method = "ALL",
                                          pass_number,
                                          output_base_dir,  # typically pass_number/welfare_results
                                          create_visualizations = TRUE) {
  
  # Create method-specific output directory
  # Modified to just use welfare_level_method since both NC and WR are calculated
  output_dir <- file.path(output_base_dir, welfare_level_method) # typically pass_number/welfare_results/3282
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
  # Note that wild animal data is already ready for integration
  
  # Step 3: Read and preprocess animal data
  cat("Step 3: Reading and preprocessing animal data...\n")
  farmed_animal_data <- read_excel(farmed_animal_data_path)
  wild_animal_data <- read_excel(wild_animal_data_path)
  
  # Step 4: Integrate human and animal data
  cat("Step 4: Integrating human and animal data...\n")
  
  # Extract needed columns from animal data
  # This assumes animal data has at least these columns: Year, Category, Group, aliveatanytime
  required_cols <- c("Year", "Category", "Group", "aliveatanytime")
  animal_cols <- names(farmed_animal_data)
  
  # Check if all required columns exist
  if(!all(required_cols %in% animal_cols)) {
    missing_cols <- setdiff(required_cols, animal_cols)
    stop("Missing required columns in animal data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Integrate farmed animal data with human data
  if("Humans" %in% farmed_animal_data$Group) {
    # If humans already in the dataset, replace with new welfare data
    integrated_hf_data <- farmed_animal_data %>%
      filter(Group != "Humans") %>%
      bind_rows(human_data_formatted)
  } else {
    # If no humans in the dataset, simply append
    integrated_hf_data <- bind_rows(farmed_animal_data, human_data_formatted)
  }
  
  # Integrate farmed animal and human data with wild animal data
  integrated_data <- integrate_wild_animal_data(wild_data = wild_animal_data, 
                                                calc_tseries = integrated_hf_data, 
                                                output_file = file.path(output_dir, "integrated_calc_tseries.xlsx"))
  
  
  #Step 5: Create extension dataset
  cat("Step 5: Create extension dataset")
  # Extend and truncate trends to produce data appropriate for net series
  extended_data <- prepare_data_for_net_series(data = integrated_data, 
                                               pass_number = pass_number, 
                                               target_year_range = 1960:2023, 
                                               endpoint_years = 5)
  
  # Step 6: Ensure dataset has required potential and utility columns
  cat("Step 6: Calculating welfare potential and utility metrics...\n")
  # Ensure we have all necessary columns for both methods - always ensure both
  # sets of columns exist to avoid missing column errors in analysis
  integrated_data <- ensure_nc_columns(integrated_data)
  integrated_data <- ensure_wr_columns(integrated_data)
  extended_data <- ensure_nc_columns(extended_data)
  extended_data <- ensure_wr_columns(extended_data)
  
  # Save integrated data - with fixed path
  integrated_data_path <- file.path(output_dir, "integrated_calc_tseries.xlsx") 
  write.xlsx(integrated_data, integrated_data_path)
  
  # Step 7: Run analysis on integrated data
  cat("Step 7: Running welfare analysis on integrated data...\n")
  analysis_results <- analyze_welfare_data(
    calc_tseries = integrated_data,
    welfare_level_method = welfare_level_method,
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
#' @param farmed_animal_data_path Path to the raw farmed animal data
#' @param wild_animal_data_path Path to the raw wild animal data
#' @param output_base_dir Base directory for outputs
#' @param create_visualizations Whether to create visualizations
#' @return List of results from all analyses
run_all_welfare_method_combinations <- function(human_data_path,
                                                farmed_animal_data_path,
                                                wild_animal_data_path,
                                                pass_number,
                                                output_base_dir,
                                                create_visualizations = TRUE) {
  
  # Run the analyses for each welfare level method first to get integrated data
  welfare_level_methods <- c("isoelastic", "3282")
  results_list <- list()
  
  for(wl_method in welfare_level_methods) {
    cat("\n\n========================================================\n")
    cat(paste0("Running analysis with welfare level method '", wl_method, "'...\n"))
    cat("========================================================\n\n")
    
    # Run complete analysis with this method
    results <- run_complete_welfare_analysis(
      human_data_path = human_data_path,
      farmed_animal_data_path = farmed_animal_data_path,
      wild_animal_data_path = wild_animal_data_path,
      welfare_level_method = wl_method,
      welfare_potential_method = "ALL",
      pass_number = pass_number,
      output_base_dir = output_base_dir,
      create_visualizations = create_visualizations
    )
    
    # Store results
    results_list[[wl_method]] <- results
    
    cat(paste0("\nAnalysis complete for '", wl_method, "'.\n"))
  }
  
  # After analysis, create top-level population trend visualizations
  # using the integrated data from the first result (both methods have same integrated data)
  cat("Creating top-level population trend visualizations...\n")
  
  # Get integrated data from first analysis
  integrated_data <- results_list[[welfare_level_methods[1]]]$integrated_data
  
  # Create population trends directory
  pop_trends_dir <- file.path(output_base_dir, "population_trends")
  if(!dir.exists(pop_trends_dir)) {
    dir.create(pop_trends_dir, recursive = TRUE)
  }
  
  # Filter out rows with NA values for population
  filtered_data <- integrated_data %>%
    filter(!is.na(aliveatanytime))
  
  # Plot 1: All population trends
  p1 <- ggplot(filtered_data, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Population Over Time", 
         y = "Population (alive at any time)", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(pop_trends_dir, "population_trends.pdf"), 
         plot = p1, width = 10, height = 6)
  
  # No wild terrestrial arthropods
  filtered_n_wta <- filtered_data %>% 
    filter(Category != "Wild terrestrial arthropods")
  
  p_n_wta <- ggplot(filtered_n_wta, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Population Over Time (no wt. arthropods)", 
         y = "Population (alive at any time)", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(pop_trends_dir, "population_trends_n_wta.pdf"), 
         plot = p_n_wta, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish
  filtered_n_wta_wfi <- filtered_n_wta %>% 
    filter(Category != "Wild fish")
  
  p_n_wta_wfi <- ggplot(filtered_n_wta_wfi, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Population Over Time (no wt. arthropods, no w. fish)", 
         y = "Population (alive at any time)", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(pop_trends_dir, "population_trends_n_wta_wfi.pdf"), 
         plot = p_n_wta_wfi, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no bees
  filtered_n_wta_wfi_fbe <- filtered_n_wta_wfi %>% 
    filter(Category != "Bees")
  
  p_n_wta_wfi_fbe <- ggplot(filtered_n_wta_wfi_fbe, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Population Over Time (no wt. arthropods, no w. fish, no bees)", 
         y = "Population (alive at any time)", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(pop_trends_dir, "population_trends_n_wta_wfi_fbe.pdf"), 
         plot = p_n_wta_wfi_fbe, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no bees, no farmed fish
  filtered_n_wta_wfi_fbe_ffi <- filtered_n_wta_wfi_fbe %>% 
    filter(Category != "Fish")
  
  p_n_wta_wfi_fbe_ffi <- ggplot(filtered_n_wta_wfi_fbe_ffi, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Population Over Time (no wt. arthropods, no w. fish, no bees, no f. fish)", 
         y = "Population (alive at any time)", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(pop_trends_dir, "population_trends_n_wta_wfi_fbe_ffi.pdf"), 
         plot = p_n_wta_wfi_fbe_ffi, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no wild terrestrial mammals
  filtered_n_wta_wfi_fbe_ffi_wtm <- filtered_n_wta_wfi_fbe_ffi %>% 
    filter(Category != "Wild terrestrial mammals")
  
  p_n_wta_wfi_fbe_ffi_wtm <- ggplot(filtered_n_wta_wfi_fbe_ffi_wtm, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Population Over Time (no wt. arthropods, no w. fish, no bees, no f. fish, no wt. mammals)", 
         y = "Population (alive at any time)", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(pop_trends_dir, "population_trends_n_wta_wfi_fbe_ffi_wtm.pdf"), 
         plot = p_n_wta_wfi_fbe_ffi_wtm, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no wt. mammals, no wild birds
  filtered_n_wta_wfi_fbe_ffi_wtm_wbi <- filtered_n_wta_wfi_fbe_ffi_wtm %>% 
    filter(Category != "Wild birds")
  
  p_n_wta_wfi_fbe_ffi_wtm_wbi <- ggplot(filtered_n_wta_wfi_fbe_ffi_wtm_wbi, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Population Over Time (no wt. arthropods, no w. fish, no bees, no f. fish, no wt. mammals, no w. birds)", 
         y = "Population (alive at any time)", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(pop_trends_dir, "population_trends_n_wta_wfi_fbe_ffi_wtm_wbi.pdf"), 
         plot = p_n_wta_wfi_fbe_ffi_wtm_wbi, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no wt. mammals, no w. birds, no chickens
  filtered_n_wta_wfi_fbe_ffi_wtm_wbi_fch <- filtered_n_wta_wfi_fbe_ffi_wtm_wbi %>% 
    filter(Category != "Chickens")
  
  p_n_wta_wfi_fbe_ffi_wtm_wbi_fch <- ggplot(filtered_n_wta_wfi_fbe_ffi_wtm_wbi_fch, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Population Over Time (no wt. arthropods, no w. fish, no bees, no f. fish, no wt. mammals, no w. birds, no chickens)", 
         y = "Population (alive at any time)", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(pop_trends_dir, "population_trends_n_wta_wfi_fbe_ffi_wtm_wbi_fch.pdf"), 
         plot = p_n_wta_wfi_fbe_ffi_wtm_wbi_fch, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no wt. mammals, no w. birds, no chickens, no humans
  filtered_n_wta_wfi_fbe_ffi_wtm_wbi_fch_hum <- filtered_n_wta_wfi_fbe_ffi_wtm_wbi_fch %>% 
    filter(Category != "Humans")
  
  p_n_wta_wfi_fbe_ffi_wtm_wbi_fch_hum <- ggplot(filtered_n_wta_wfi_fbe_ffi_wtm_wbi_fch_hum, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Population Over Time (no wt. arthropods, no w. fish, no bees, no f. fish, no wt. mammals, no w. birds, no chickens, no humans)", 
         y = "Population (alive at any time)", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(pop_trends_dir, "population_trends_n_wta_wfi_fbe_ffi_wtm_wbi_fch_hum.pdf"), 
         plot = p_n_wta_wfi_fbe_ffi_wtm_wbi_fch_hum, width = 10, height = 6)
  
  return(results_list)
}
