# Modular Welfare Analysis Framework
# This script provides a flexible framework for analyzing welfare data
# with different quantification methods for both welfare levels and potentials

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)

#' Main function to process and analyze welfare time series data
#' 
#' @param data_path Path to the input time series data
#' @param welfare_level_method Method for welfare level quantification ("isoelastic" or "3282")
#' @param welfare_potential_method Method for welfare potential ("NC" or "WR")
#' @param output_dir Directory for output files
#' @param create_visualizations Whether to create visualizations
#' @return List of calculated results
#' Main function to process and analyze welfare time series data
#' 
#' @param data_path Path to the input time series data
#' @param welfare_level_method Method for welfare level quantification ("isoelastic" or "3282")
#' @param welfare_potential_method Method for welfare potential ("NC" or "WR")
#' @param output_dir Directory for output files
#' @param create_visualizations Whether to create visualizations
#' @param skip_population_plots Whether to skip population trend visualizations
#' @return List of calculated results
analyze_welfare_data <- function(data_path, 
                                 welfare_level_method = "isoelastic",
                                 output_dir = "welfare_analysis_results",
                                 create_visualizations = TRUE) {
  
  # Create output directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Read and preprocess data
  cat("Reading and preprocessing data...\n")
  calc_tseries <- read_excel(data_path)
  
  # 2. Apply welfare level method if needed
  if(welfare_level_method == "isoelastic") {
    cat("Applying isoelastic welfare level method...\n")
    # Logic for isoelastic method would go here if needed
    # Typically this would already be in the input data
  } else if(welfare_level_method == "3282") {
    cat("Applying 32-82 welfare level method...\n")
    # Logic for 32-82 method would go here if needed
    # Typically this would already be in the input data
  } else {
    stop("Invalid welfare level method. Use 'isoelastic' or '3282'.")
  }
  
  # 3. Calculate all welfare metrics (NC and WR)
  cat("Processing both NC and WR welfare potential methods...\n")
  calc_tseries <- ensure_nc_columns(calc_tseries)
  calc_tseries <- ensure_wr_columns(calc_tseries)
  
  # 4. Calculate net series
  cat("Calculating net series...\n")
  net_series <- calculate_net_series(calc_tseries)
  
  # 5. Calculate correlations and elasticities
  cat("Calculating correlations and elasticities...\n")
  cor_and_elasticity <- calculate_correlations_elasticities(calc_tseries)
  
  # 6. Calculate factor changes
  cat("Calculating factor changes...\n")
  f_change <- calculate_factor_changes(calc_tseries)
  
  # 7. Create visualizations if requested
  if(create_visualizations) {
    cat("Creating visualizations...\n")
    vis_dir <- file.path(output_dir, "visualizations")
    if(!dir.exists(vis_dir)) {
      dir.create(vis_dir, recursive = TRUE)
    }
    
    create_utility_visualizations(calc_tseries, 
                                  net_series,
                                  output_dir = vis_dir)
  }
  
  # 8. Save results
  cat("Saving results...\n")
  method_suffix <- paste0("_", tolower(welfare_level_method))
  
  write.xlsx(net_series, file.path(output_dir, paste0("net_series", method_suffix, ".xlsx")))
  write.xlsx(cor_and_elasticity, file.path(output_dir, paste0("cor_and_elas", method_suffix, ".xlsx")))
  write.xlsx(f_change, file.path(output_dir, paste0("f_change", method_suffix, ".xlsx")))
  
  # 9. Return results
  cat("Analysis complete!\n")
  return(list(
    calc_tseries = calc_tseries,
    net_series = net_series,
    cor_and_elasticity = cor_and_elasticity,
    f_change = f_change
  ))
}

#' Ensure the dataset has all required NC columns
#' 
#' @param data The dataset to check and modify
#' @return Updated dataset with all NC columns
ensure_nc_columns <- function(data) {
  # Check if NC columns already exist
  if(!("NC_utility" %in% colnames(data)) || 
     !("NC_tot" %in% colnames(data))) {
    
    # If not, calculate them based on available data
    if("forebrain_neurons" %in% colnames(data) && 
       "aliveatanytime" %in% colnames(data) && 
       "Welfare_level" %in% colnames(data)) {
      
      # Get human forebrain neurons for relative scaling
      human_fneurons <- data %>%
        filter(Category == "Humans") %>%
        pull(forebrain_neurons) %>%
        unique() %>%
        .[1]  # Use just the first value
      
      # Calculate NC columns
      data <- data %>%
        mutate(
          NC_potential = forebrain_neurons / human_fneurons,
          NC_utility = aliveatanytime * NC_potential * Welfare_level,
          NC_tot = aliveatanytime * forebrain_neurons
        )
    } else {
      stop("Required columns for NC calculations are missing.")
    }
  }
  
  return(data)
}

#' Ensure the dataset has all required WR columns
#' 
#' @param data The dataset to check and modify
#' @return Updated dataset with all WR columns
ensure_wr_columns <- function(data) {
  # Check if WR columns already exist
  if(!("WR_utility" %in% colnames(data))) {
    
    # If not, calculate them based on available data
    if("WR_potential" %in% colnames(data) && 
       "aliveatanytime" %in% colnames(data) && 
       "Welfare_level" %in% colnames(data)) {
      
      # Calculate WR columns
      data <- data %>%
        mutate(
          WR_utility = aliveatanytime * WR_potential * Welfare_level
        )
    } else {
      stop("Required columns for WR calculations are missing.")
    }
  }
  
  return(data)
}



#' Calculate net series for all categories
#' 
#' @param data The input dataset
#' @return Dataframe with net series by year
calculate_net_series <- function(data) {
  
  # Determine the common years across all categories
  year_bounds <- data %>%
    group_by(Category) %>%
    summarize(
      min_y = min(Year, na.rm = TRUE),
      max_y = max(Year, na.rm = TRUE)
    )
  
  common_window <- year_bounds %>%
    summarize(
      start = max(min_y),
      end = min(max_y)
    )
  
  # Filter to only include years in the common window
  data_global <- data %>%
    filter(Year >= common_window$start,
           Year <= common_window$end)
  
  # Compute net series
  net_series <- data_global %>%
    group_by(Year) %>%
    summarize(
      WR_utility = sum(WR_utility, na.rm = FALSE),  # Keep naming consistent with original columns
      NC_utility = sum(NC_utility, na.rm = FALSE),
      NC_tot = sum(NC_tot, na.rm = FALSE),
      total_population = sum(aliveatanytime, na.rm = FALSE)
    )
  
  return(net_series)
}

#' Calculate correlations and elasticities between human and animal populations/utilities
#' for both WR and NC methods in a single combined dataframe
#' 
#' @param data The input dataset
#' @return Dataframe with correlations and elasticities for both methods
calculate_correlations_elasticities <- function(data) {
  
  # Get non-human categories
  nh_cat <- setdiff(unique(data$Category), "Humans")
  
  # Initialize results dataframe
  cor_and_elasticity <- data.frame(
    Category = character(), 
    WR_cor_pop = numeric(), 
    WR_cor_u = numeric(), 
    WR_cor_hpop_au = numeric(), 
    WR_cor_hu_apop = numeric(),
    WR_e_pop = numeric(), 
    WR_e_u = numeric(), 
    WR_e_hpop_au = numeric(), 
    WR_e_hu_apop = numeric(),
    NC_cor_pop = numeric(), 
    NC_cor_u = numeric(), 
    NC_cor_hpop_au = numeric(), 
    NC_cor_hu_apop = numeric(),
    NC_e_pop = numeric(), 
    NC_e_u = numeric(), 
    NC_e_hpop_au = numeric(), 
    NC_e_hu_apop = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Get human vectors with appropriate columns
  human_vec <- data %>% 
    filter(Category == "Humans") %>% 
    select(Year, aliveatanytime, WR_utility, NC_utility)
  
  # Calculate correlations and elasticities for each non-human category
  for (category in nh_cat) {
    # Get category vector with appropriate columns
    cat_vec <- data %>% 
      filter(Category == category) %>% 
      select(Year, aliveatanytime, WR_utility, NC_utility)
    
    # Identify min and max year for the correlation
    min_year <- max(min(cat_vec$Year), min(human_vec$Year), na.rm = TRUE)
    max_year <- min(max(cat_vec$Year), max(human_vec$Year), na.rm = TRUE)
    
    # Get population and utility vectors for the right time range
    human_pop_trunc <- human_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(aliveatanytime)
    
    human_wr_u_trunc <- human_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(WR_utility)
    
    human_nc_u_trunc <- human_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(NC_utility)
    
    cat_pop_trunc <- cat_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(aliveatanytime)
    
    cat_wr_u_trunc <- cat_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(WR_utility)
    
    cat_nc_u_trunc <- cat_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(NC_utility)
    
    # Check if we have enough non-NA values for population correlations
    has_valid_pop_data <- !all(is.na(human_pop_trunc)) && !all(is.na(cat_pop_trunc)) && 
      sum(!is.na(human_pop_trunc) & !is.na(cat_pop_trunc)) > 1
    
    # Check for valid WR utility data
    has_valid_wr_utility_data <- !all(is.na(human_wr_u_trunc)) && !all(is.na(cat_wr_u_trunc)) && 
      sum(!is.na(human_wr_u_trunc) & !is.na(cat_wr_u_trunc)) > 1
    
    # Check for valid NC utility data
    has_valid_nc_utility_data <- !all(is.na(human_nc_u_trunc)) && !all(is.na(cat_nc_u_trunc)) && 
      sum(!is.na(human_nc_u_trunc) & !is.na(cat_nc_u_trunc)) > 1
    
    # Calculate WR correlations
    wr_cor_pop <- if(has_valid_pop_data) {
      tryCatch(cor(human_pop_trunc, cat_pop_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    wr_cor_u <- if(has_valid_wr_utility_data) {
      tryCatch(cor(human_wr_u_trunc, cat_wr_u_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    wr_cor_hpop_au <- if(!all(is.na(human_pop_trunc)) && !all(is.na(cat_wr_u_trunc)) && 
                         sum(!is.na(human_pop_trunc) & !is.na(cat_wr_u_trunc)) > 1) {
      tryCatch(cor(human_pop_trunc, cat_wr_u_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    wr_cor_hu_apop <- if(!all(is.na(human_wr_u_trunc)) && !all(is.na(cat_pop_trunc)) && 
                         sum(!is.na(human_wr_u_trunc) & !is.na(cat_pop_trunc)) > 1) {
      tryCatch(cor(human_wr_u_trunc, cat_pop_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    # Calculate WR elasticities
    wr_e_pop <- if(has_valid_pop_data) {
      tryCatch(coef(lm(cat_pop_trunc ~ human_pop_trunc))[2], error = function(e) NA)
    } else NA
    
    wr_e_u <- if(has_valid_wr_utility_data) {
      tryCatch(coef(lm(cat_wr_u_trunc ~ human_wr_u_trunc))[2], error = function(e) NA)
    } else NA
    
    wr_e_hpop_au <- if(!all(is.na(human_pop_trunc)) && !all(is.na(cat_wr_u_trunc)) && 
                       sum(!is.na(human_pop_trunc) & !is.na(cat_wr_u_trunc)) > 1) {
      tryCatch(coef(lm(cat_wr_u_trunc ~ human_pop_trunc))[2], error = function(e) NA)
    } else NA
    
    wr_e_hu_apop <- if(!all(is.na(human_wr_u_trunc)) && !all(is.na(cat_pop_trunc)) && 
                       sum(!is.na(human_wr_u_trunc) & !is.na(cat_pop_trunc)) > 1) {
      tryCatch(coef(lm(cat_pop_trunc ~ human_wr_u_trunc))[2], error = function(e) NA)
    } else NA
    
    # Calculate NC correlations
    nc_cor_pop <- wr_cor_pop  # Same population correlation for both methods
    
    nc_cor_u <- if(has_valid_nc_utility_data) {
      tryCatch(cor(human_nc_u_trunc, cat_nc_u_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    nc_cor_hpop_au <- if(!all(is.na(human_pop_trunc)) && !all(is.na(cat_nc_u_trunc)) && 
                         sum(!is.na(human_pop_trunc) & !is.na(cat_nc_u_trunc)) > 1) {
      tryCatch(cor(human_pop_trunc, cat_nc_u_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    nc_cor_hu_apop <- if(!all(is.na(human_nc_u_trunc)) && !all(is.na(cat_pop_trunc)) && 
                         sum(!is.na(human_nc_u_trunc) & !is.na(cat_pop_trunc)) > 1) {
      tryCatch(cor(human_nc_u_trunc, cat_pop_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    # Calculate NC elasticities
    nc_e_pop <- wr_e_pop  # Same population elasticity for both methods
    
    nc_e_u <- if(has_valid_nc_utility_data) {
      tryCatch(coef(lm(cat_nc_u_trunc ~ human_nc_u_trunc))[2], error = function(e) NA)
    } else NA
    
    nc_e_hpop_au <- if(!all(is.na(human_pop_trunc)) && !all(is.na(cat_nc_u_trunc)) && 
                       sum(!is.na(human_pop_trunc) & !is.na(cat_nc_u_trunc)) > 1) {
      tryCatch(coef(lm(cat_nc_u_trunc ~ human_pop_trunc))[2], error = function(e) NA)
    } else NA
    
    nc_e_hu_apop <- if(!all(is.na(human_nc_u_trunc)) && !all(is.na(cat_pop_trunc)) && 
                       sum(!is.na(human_nc_u_trunc) & !is.na(cat_pop_trunc)) > 1) {
      tryCatch(coef(lm(cat_pop_trunc ~ human_nc_u_trunc))[2], error = function(e) NA)
    } else NA
    
    # Add results to result table
    cor_and_elasticity <- cor_and_elasticity %>% 
      add_row(
        Category = category, 
        WR_cor_pop = wr_cor_pop, 
        WR_cor_u = wr_cor_u, 
        WR_cor_hpop_au = wr_cor_hpop_au, 
        WR_cor_hu_apop = wr_cor_hu_apop,
        WR_e_pop = wr_e_pop, 
        WR_e_u = wr_e_u, 
        WR_e_hpop_au = wr_e_hpop_au, 
        WR_e_hu_apop = wr_e_hu_apop,
        NC_cor_pop = nc_cor_pop, 
        NC_cor_u = nc_cor_u, 
        NC_cor_hpop_au = nc_cor_hpop_au, 
        NC_cor_hu_apop = nc_cor_hu_apop,
        NC_e_pop = nc_e_pop, 
        NC_e_u = nc_e_u, 
        NC_e_hpop_au = nc_e_hpop_au, 
        NC_e_hu_apop = nc_e_hu_apop
      )
  }
  
  return(cor_and_elasticity)
}

#' Calculate factor changes in population over time for each category
#' 
#' @param data The input dataset
#' @return Dataframe with factor changes by category
calculate_factor_changes <- function(data) {
  # Initialize results dataframe
  f_change <- data.frame(
    Category = character(), 
    Factor_change = numeric(), 
    stringsAsFactors = FALSE
  )
  
  # Calculate factor change for each category
  for (category in unique(data$Category)) {
    pop_vec <- data %>% 
      filter(Category == category) %>% 
      select(Year, aliveatanytime)
    
    min_year <- min(pop_vec$Year)
    max_year <- max(pop_vec$Year)
    
    min_year_value <- pop_vec$aliveatanytime[pop_vec$Year == min_year]
    max_year_value <- pop_vec$aliveatanytime[pop_vec$Year == max_year]
    
    factor_change <- max_year_value / min_year_value
    
    f_change <- f_change %>% 
      add_row(Category = category, Factor_change = factor_change)
  }
  
  return(f_change)
}



#' Create standard set of visualizations
#' 
#' @param data The processed dataset
#' @param net_series The net series data
#' @param output_dir Directory for saving visualizations
create_utility_visualizations <- function(data, 
                                          net_series, 
                                          output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  ########## NC UTILITY PLOTS SECOND ##########
  
  # Filter out rows with NA values for NC utility
  filtered_data_nc <- data %>%
    filter(!is.na(NC_utility), !is.na(aliveatanytime))
  
  # NC utility over time - all categories
  p_nc <- ggplot(filtered_data_nc, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - NC Method", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "NC_utility_trends.pdf"), 
         plot = p_nc, width = 10, height = 6)
  
  # No wild terrestrial arthropods
  filtered_nc_n_wta <- filtered_data_nc %>% 
    filter(Category != "Wild terrestrial arthropods")
  
  p_nc_n_wta <- ggplot(filtered_nc_n_wta, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta.pdf"), 
         plot = p_nc_n_wta, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish
  filtered_nc_n_wta_wfi <- filtered_nc_n_wta %>% 
    filter(Category != "Wild fish")
  
  p_nc_n_wta_wfi <- ggplot(filtered_nc_n_wta_wfi, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi.pdf"), 
         plot = p_nc_n_wta_wfi, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no humans
  filtered_nc_n_wta_wfi_hum <- filtered_nc_n_wta_wfi %>% 
    filter(Category != "Humans")
  
  p_nc_n_wta_wfi_hum <- ggplot(filtered_nc_n_wta_wfi_hum, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish, No humans)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_hum.pdf"), 
         plot = p_nc_n_wta_wfi_hum, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no humans, no wild terrestrial mammals
  filtered_nc_n_wta_wfi_hum_wtm <- filtered_nc_n_wta_wfi_hum %>% 
    filter(Category != "Wild terrestrial mammals")
  
  p_nc_n_wta_wfi_hum_wtm <- ggplot(filtered_nc_n_wta_wfi_hum_wtm, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish, No humans, No wt. mammals)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_hum_wtm.pdf"), 
         plot = p_nc_n_wta_wfi_hum_wtm, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no humans, no wild terrestrial mammals, no farmed fish
  filtered_nc_n_wta_wfi_hum_wtm_ffi <- filtered_nc_n_wta_wfi_hum_wtm %>% 
    filter(Category != "Fish")
  
  p_nc_n_wta_wfi_hum_wtm_ffi <- ggplot(filtered_nc_n_wta_wfi_hum_wtm_ffi, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish, No humans, No wt. mammals, No f. fish)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_hum_wtm_ffi.pdf"), 
         plot = p_nc_n_wta_wfi_hum_wtm_ffi, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no humans, no wt. mammals, no f. fish, no chickens
  filtered_nc_n_wta_wfi_hum_wtm_ffi_fch <- filtered_nc_n_wta_wfi_hum_wtm_ffi %>% 
    filter(Category != "Chickens")
  
  p_nc_n_wta_wfi_hum_wtm_ffi_fch <- ggplot(filtered_nc_n_wta_wfi_hum_wtm_ffi_fch, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish, No humans, No wt. mammals, No f. fish, No chickens)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_hum_wtm_ffi_fch.pdf"), 
         plot = p_nc_n_wta_wfi_hum_wtm_ffi_fch, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no humans, no wt. mammals, no f. fish, no chickens, no wild birds
  filtered_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi <- filtered_nc_n_wta_wfi_hum_wtm_ffi_fch %>% 
    filter(Category != "Wild birds")
  
  p_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi <- ggplot(filtered_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish, No humans, No wt. mammals, No f. fish, No chickens, No w. birds)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_hum_wtm_ffi_fch_wbi.pdf"), 
         plot = p_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi, width = 10, height = 6)
  
  ########## NET COMPARISON PLOTS - ALL LIMITED TO 1970-2017 ##########
  
  # Define the time range constraints based on wild birds data (1970-2017)
  min_year_constraint <- 1970
  max_year_constraint <- 2017
  
  ########## ORIGINAL COMPARISON: NC_net_utility_comp.pdf ##########
  
  # Calculate net NC utility without humans
  net_nc_nh_data <- filtered_data_nc %>% 
    filter(Category != "Humans", 
           Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      NC_utility = sum(NC_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "Without Humans")
  
  # Get with-humans data, limited to 1970-2017
  humans_plus_animals <- net_series %>%
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    select(Year, NC_utility) %>%
    mutate(Group = "With Humans")
  
  # Combine for comparison
  combined_nc_series <- bind_rows(
    humans_plus_animals,
    net_nc_nh_data
  )
  
  # Create and save the plot
  p_nc_comp <- ggplot(combined_nc_series, aes(x = Year, y = NC_utility, color = Group)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = "NC Net Utility Comparison (With vs. Without Humans)", 
         y = "Net Utility", 
         x = "Year",
         color = "Dataset") +
    theme_minimal() +
    xlim(min_year_constraint, max_year_constraint)  # Explicitly set x-axis limits
  
  ggsave(file.path(output_dir, "NC_net_utility_comp.pdf"), 
         plot = p_nc_comp, width = 10, height = 6)
  
  ########## NEW COMPARISON: NC_net_utility_comp_n_wta_wfi.pdf ##########
  
  # Get filtered categories (no wild terrestrial arthropods, no wild fish)
  filtered_nc_n_wta_wfi <- filtered_data_nc %>% 
    filter(Category != "Wild terrestrial arthropods",
           Category != "Wild fish")
  
  # Calculate net utility without humans, limited to 1970-2017
  net_nc_n_wta_wfi_nh_data <- filtered_nc_n_wta_wfi %>% 
    filter(Category != "Humans",
           Year >= min_year_constraint,
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      NC_utility = sum(NC_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "Without Humans")
  
  # Calculate net utility with humans, limited to 1970-2017
  net_nc_n_wta_wfi_wh_data <- filtered_nc_n_wta_wfi %>%
    filter(Year >= min_year_constraint,
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      NC_utility = sum(NC_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "With Humans")
  
  # Combine for comparison
  combined_nc_n_wta_wfi_series <- bind_rows(
    net_nc_n_wta_wfi_wh_data,
    net_nc_n_wta_wfi_nh_data
  )
  
  # Create and save the plot
  p_nc_comp_n_wta_wfi <- ggplot(combined_nc_n_wta_wfi_series, aes(x = Year, y = NC_utility, color = Group)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = "NC Net Utility Comparison (No wt. arthropods, No w. fish)", 
         y = "Net Utility", 
         x = "Year",
         color = "Dataset") +
    theme_minimal() +
    xlim(min_year_constraint, max_year_constraint)  # Explicitly set x-axis limits
  
  ggsave(file.path(output_dir, "NC_net_utility_comp_n_wta_wfi.pdf"), 
         plot = p_nc_comp_n_wta_wfi, width = 10, height = 6)
  
  ########## NEW COMPARISON: NC_net_utility_comp_nw.pdf ##########
  
  # Define wild animal categories
  wild_categories <- c("Wild birds", "Wild terrestrial mammals", "Wild fish", "Wild terrestrial arthropods")
  
  # Filter to exclude wild categories
  filtered_nc_nw <- filtered_data_nc %>% 
    filter(!Category %in% wild_categories)
  
  # Calculate net utility without humans, limited to 1970-2017
  net_nc_nw_nh_data <- filtered_nc_nw %>% 
    filter(Category != "Humans",
           Year >= min_year_constraint,
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      NC_utility = sum(NC_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "Without Humans")
  
  # Calculate net utility with humans, limited to 1970-2017
  net_nc_nw_wh_data <- filtered_nc_nw %>%
    filter(Year >= min_year_constraint,
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      NC_utility = sum(NC_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "With Humans")
  
  # Combine for comparison
  combined_nc_nw_series <- bind_rows(
    net_nc_nw_wh_data,
    net_nc_nw_nh_data
  )
  
  # Create and save the plot
  p_nc_comp_nw <- ggplot(combined_nc_nw_series, aes(x = Year, y = NC_utility, color = Group)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = "NC Net Utility Comparison (No Wild Animals)", 
         y = "Net Utility", 
         x = "Year",
         color = "Dataset") +
    theme_minimal() +
    xlim(min_year_constraint, max_year_constraint)  # Explicitly set x-axis limits
  
  ggsave(file.path(output_dir, "NC_net_utility_comp_nw.pdf"), 
         plot = p_nc_comp_nw, width = 10, height = 6)
  
  ########## WR UTILITY PLOTS THIRD ##########
  
  # Filter out rows with NA values for WR utility
  filtered_data_wr <- data %>%
    filter(!is.na(WR_utility), !is.na(aliveatanytime))
  
  # WR utility over time - all categories
  p_wr <- ggplot(filtered_data_wr, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - WR Method", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "WR_utility_trends.pdf"), 
         plot = p_wr, width = 10, height = 6)
  
  # No wild terrestrial arthropods
  filtered_wr_n_wta <- filtered_data_wr %>% 
    filter(Category != "Wild terrestrial arthropods")
  
  p_wr_n_wta <- ggplot(filtered_wr_n_wta, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta.pdf"), 
         plot = p_wr_n_wta, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish
  filtered_wr_n_wta_wfi <- filtered_wr_n_wta %>% 
    filter(Category != "Wild fish")
  
  p_wr_n_wta_wfi <- ggplot(filtered_wr_n_wta_wfi, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi.pdf"), 
         plot = p_wr_n_wta_wfi, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no bees
  filtered_wr_n_wta_wfi_fbe <- filtered_wr_n_wta_wfi %>% 
    filter(Category != "Bees")
  
  p_wr_n_wta_wfi_fbe <- ggplot(filtered_wr_n_wta_wfi_fbe, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe.pdf"), 
         plot = p_wr_n_wta_wfi_fbe, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no bees, no farmed fish
  filtered_wr_n_wta_wfi_fbe_ffi <- filtered_wr_n_wta_wfi_fbe %>% 
    filter(Category != "Fish")
  
  p_wr_n_wta_wfi_fbe_ffi <- ggplot(filtered_wr_n_wta_wfi_fbe_ffi, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees, No f. fish)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe_ffi.pdf"), 
         plot = p_wr_n_wta_wfi_fbe_ffi, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no chickens
  filtered_wr_n_wta_wfi_fbe_ffi_fch <- filtered_wr_n_wta_wfi_fbe_ffi %>% 
    filter(Category != "Chickens")
  
  p_wr_n_wta_wfi_fbe_ffi_fch <- ggplot(filtered_wr_n_wta_wfi_fbe_ffi_fch, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees, No f. fish, No chickens)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe_ffi_fch.pdf"), 
         plot = p_wr_n_wta_wfi_fbe_ffi_fch, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no chickens, no wild terrestrial mammals
  filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm <- filtered_wr_n_wta_wfi_fbe_ffi_fch %>% 
    filter(Category != "Wild terrestrial mammals")
  
  p_wr_n_wta_wfi_fbe_ffi_fch_wtm <- ggplot(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees, No f. fish, No chickens, No wt. mammals)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe_ffi_fch_wtm.pdf"), 
         plot = p_wr_n_wta_wfi_fbe_ffi_fch_wtm, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no chickens, no wt. mammals, no humans
  filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum <- filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm %>% 
    filter(Category != "Humans")
  
  p_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum <- ggplot(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees, No f. fish, No chickens, No wt. mammals, No humans)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe_ffi_fch_wtm_hum.pdf"), 
         plot = p_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no chickens, no wt. mammals, no humans, no wild birds
  filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi <- filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum %>% 
    filter(Category != "Wild birds")
  
  p_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi <- ggplot(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees, No f. fish, No chickens, No wt. mammals, No humans, No w. birds)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi.pdf"), 
         plot = p_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi, width = 10, height = 6)
  
  ########## WR NET COMPARISON PLOTS - ALL LIMITED TO 1970-2017 ##########
  
  # 1. WR_net_utility_comp - Basic with/without humans comparison
  
  # Calculate net WR utility without humans
  net_wr_nh_data <- filtered_data_wr %>% 
    filter(Category != "Humans", 
           Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      WR_utility = sum(WR_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "Without Humans")
  
  # Get with-humans data, limited to 1970-2017
  humans_plus_animals_wr <- net_series %>%
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    select(Year, WR_utility) %>%
    mutate(Group = "With Humans")
  
  # Combine for comparison
  combined_wr_series <- bind_rows(
    humans_plus_animals_wr,
    net_wr_nh_data
  )
  
  # Create and save the plot
  p_wr_comp <- ggplot(combined_wr_series, aes(x = Year, y = WR_utility, color = Group)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = "WR Net Utility Comparison (With vs. Without Humans)", 
         y = "Net Utility", 
         x = "Year",
         color = "Dataset") +
    theme_minimal() +
    xlim(min_year_constraint, max_year_constraint)
  
  ggsave(file.path(output_dir, "WR_net_utility_comp.pdf"), 
         plot = p_wr_comp, width = 10, height = 6)
  
  # 2. WR_net_utility_comp_n_wta_wfi - No wild terrestrial arthropods, no wild fish
  
  # Get filtered categories
  filtered_wr_n_wta_wfi <- filtered_data_wr %>% 
    filter(Category != "Wild terrestrial arthropods",
           Category != "Wild fish")
  
  # Calculate net utility without humans, limited to 1970-2017
  net_wr_n_wta_wfi_nh_data <- filtered_wr_n_wta_wfi %>% 
    filter(Category != "Humans",
           Year >= min_year_constraint,
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      WR_utility = sum(WR_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "Without Humans")
  
  # Calculate net utility with humans, limited to 1970-2017
  net_wr_n_wta_wfi_wh_data <- filtered_wr_n_wta_wfi %>%
    filter(Year >= min_year_constraint,
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      WR_utility = sum(WR_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "With Humans")
  
  # Combine for comparison
  combined_wr_n_wta_wfi_series <- bind_rows(
    net_wr_n_wta_wfi_wh_data,
    net_wr_n_wta_wfi_nh_data
  )
  
  # Create and save the plot
  p_wr_comp_n_wta_wfi <- ggplot(combined_wr_n_wta_wfi_series, aes(x = Year, y = WR_utility, color = Group)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = "WR Net Utility Comparison (No wt. arthropods, No w. fish)", 
         y = "Net Utility", 
         x = "Year",
         color = "Dataset") +
    theme_minimal() +
    xlim(min_year_constraint, max_year_constraint)
  
  ggsave(file.path(output_dir, "WR_net_utility_comp_n_wta_wfi.pdf"), 
         plot = p_wr_comp_n_wta_wfi, width = 10, height = 6)
  
  # 3. WR_net_utility_comp_nw - No wild animals
  
  # Filter to exclude wild categories
  filtered_wr_nw <- filtered_data_wr %>% 
    filter(!Category %in% wild_categories)
  
  # Calculate net utility without humans, limited to 1970-2017
  net_wr_nw_nh_data <- filtered_wr_nw %>% 
    filter(Category != "Humans",
           Year >= min_year_constraint,
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      WR_utility = sum(WR_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "Without Humans")
  
  # Calculate net utility with humans, limited to 1970-2017
  net_wr_nw_wh_data <- filtered_wr_nw %>%
    filter(Year >= min_year_constraint,
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      WR_utility = sum(WR_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "With Humans")
  
  # Combine for comparison
  combined_wr_nw_series <- bind_rows(
    net_wr_nw_wh_data,
    net_wr_nw_nh_data
  )
  
  # Create and save the plot
  p_wr_comp_nw <- ggplot(combined_wr_nw_series, aes(x = Year, y = WR_utility, color = Group)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = "WR Net Utility Comparison (No Wild Animals)", 
         y = "Net Utility", 
         x = "Year",
         color = "Dataset") +
    theme_minimal() +
    xlim(min_year_constraint, max_year_constraint)
  
  ggsave(file.path(output_dir, "WR_net_utility_comp_nw.pdf"), 
         plot = p_wr_comp_nw, width = 10, height = 6)
  
  # 4. WR_net_utility_comp_n_wta_wfi_fbe - No wild terrestrial arthropods, no wild fish, no bees
  
  # Get filtered categories
  filtered_wr_n_wta_wfi_fbe <- filtered_data_wr %>% 
    filter(Category != "Wild terrestrial arthropods",
           Category != "Wild fish",
           Category != "Bees")
  
  # Calculate net utility without humans, limited to 1970-2017
  net_wr_n_wta_wfi_fbe_nh_data <- filtered_wr_n_wta_wfi_fbe %>% 
    filter(Category != "Humans",
           Year >= min_year_constraint,
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      WR_utility = sum(WR_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "Without Humans")
  
  # Calculate net utility with humans, limited to 1970-2017
  net_wr_n_wta_wfi_fbe_wh_data <- filtered_wr_n_wta_wfi_fbe %>%
    filter(Year >= min_year_constraint,
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      WR_utility = sum(WR_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Group = "With Humans")
  
  # Combine for comparison
  combined_wr_n_wta_wfi_fbe_series <- bind_rows(
    net_wr_n_wta_wfi_fbe_wh_data,
    net_wr_n_wta_wfi_fbe_nh_data
  )
  
  # Create and save the plot
  p_wr_comp_n_wta_wfi_fbe <- ggplot(combined_wr_n_wta_wfi_fbe_series, aes(x = Year, y = WR_utility, color = Group)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = "WR Net Utility Comparison (No wt. arthropods, No w. fish, No bees)", 
         y = "Net Utility", 
         x = "Year",
         color = "Dataset") +
    theme_minimal() +
    xlim(min_year_constraint, max_year_constraint)
  
  ggsave(file.path(output_dir, "WR_net_utility_comp_n_wta_wfi_fbe.pdf"), 
         plot = p_wr_comp_n_wta_wfi_fbe, width = 10, height = 6)
}  
  
  
