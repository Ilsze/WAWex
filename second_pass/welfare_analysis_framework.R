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
                                 welfare_potential_method = "WR",
                                 output_dir = "welfare_analysis_results",
                                 create_visualizations = TRUE,
                                 skip_population_plots = FALSE) {
  
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
  
  # 3. Process data based on welfare potential method
  # This is the core of our implementation - selecting the appropriate columns
  cat(paste("Processing with", welfare_potential_method, "welfare potential method...\n"))
  
  # Create utility and population columns based on the selected method
  if(welfare_potential_method == "NC") {
    # Use neuron count columns
    calc_tseries <- ensure_nc_columns(calc_tseries)
    utility_col <- "NC_utility"
    pop_col <- "NC_pop"
    potential_col <- "NC_potential"
  } else if(welfare_potential_method == "WR") {
    # Use welfare range columns
    calc_tseries <- ensure_wr_columns(calc_tseries)
    utility_col <- "WR_utility"
    pop_col <- "WR_pop"
    potential_col <- "WR_potential"
  } else {
    stop("Invalid welfare potential method. Use 'NC' or 'WR'.")
  }
  
  # 4. Calculate net series
  cat("Calculating net series...\n")
  net_series <- calculate_net_series(calc_tseries, 
                                     utility_col = utility_col, 
                                     pop_col = pop_col)
  
  # 5. Calculate net series without humans
  cat("Calculating net series without humans...\n")
  net_series_nh <- calculate_net_series_no_humans(calc_tseries, 
                                                  utility_col = utility_col, 
                                                  pop_col = pop_col)
  
  # 6. Calculate correlations and elasticities
  cat("Calculating correlations and elasticities...\n")
  cor_and_elasticity <- calculate_correlations_elasticities(calc_tseries, 
                                                            utility_col = utility_col, 
                                                            pop_col = pop_col,
                                                            potential_col = potential_col)
  
  # 7. Calculate factor changes
  cat("Calculating factor changes...\n")
  f_change <- calculate_factor_changes(calc_tseries)
  
  # 8. Create visualizations if requested
  if(create_visualizations) {
    cat("Creating visualizations...\n")
    vis_dir <- file.path(output_dir, "visualizations")
    if(!dir.exists(vis_dir)) {
      dir.create(vis_dir, recursive = TRUE)
    }
    
    create_utility_visualizations(calc_tseries, net_series, net_series_nh, 
                                  utility_col = utility_col, 
                                  pop_col = pop_col,
                                  method_name = welfare_potential_method,
                                  output_dir = vis_dir,
                                  skip_population_plots = skip_population_plots)
  }
  
  # 9. Save results
  cat("Saving results...\n")
  method_suffix <- paste0("_", tolower(welfare_level_method), "_", tolower(welfare_potential_method))
  
  write.xlsx(net_series, file.path(output_dir, paste0("net_series", method_suffix, ".xlsx")))
  write.xlsx(net_series_nh, file.path(output_dir, paste0("net_series_nh", method_suffix, ".xlsx")))
  write.xlsx(cor_and_elasticity, file.path(output_dir, paste0("cor_and_elas", method_suffix, ".xlsx")))
  write.xlsx(f_change, file.path(output_dir, paste0("f_change", method_suffix, ".xlsx")))
  
  # 10. Return results
  cat("Analysis complete!\n")
  return(list(
    calc_tseries = calc_tseries,
    net_series = net_series,
    net_series_nh = net_series_nh,
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
     !("NC_pop" %in% colnames(data))) {
    
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
          NC_pop = aliveatanytime * NC_potential,
          NC_utility = aliveatanytime * NC_potential * Welfare_level
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
  if(!("WR_utility" %in% colnames(data)) || 
     !("WR_pop" %in% colnames(data))) {
    
    # If not, calculate them based on available data
    if("WR_potential" %in% colnames(data) && 
       "aliveatanytime" %in% colnames(data) && 
       "Welfare_level" %in% colnames(data)) {
      
      # Calculate WR columns
      data <- data %>%
        mutate(
          WR_pop = aliveatanytime * WR_potential,
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
#' @param utility_col Column name for utility values
#' @param pop_col Column name for population values
#' @return Dataframe with net series by year
calculate_net_series <- function(data, 
                                 utility_col = "WR_utility", 
                                 pop_col = "WR_pop") {
  
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
      net_utility = sum(.data[[utility_col]], na.rm = FALSE),
      net_pop = sum(.data[[pop_col]], na.rm = FALSE)
    )
  
  return(net_series)
}

#' Calculate net series excluding humans
#' 
#' @param data The input dataset
#' @param utility_col Column name for utility values
#' @param pop_col Column name for population values
#' @return Dataframe with net series (no humans) by year
calculate_net_series_no_humans <- function(data, 
                                           utility_col = "WR_utility", 
                                           pop_col = "WR_pop") {
  
  # Filter out humans
  data_nh <- data %>% 
    filter(Category != "Humans")
  
  # Determine the common years across all non-human categories
  year_bounds_nh <- data_nh %>%
    group_by(Category) %>%
    summarize(
      min_y = min(Year, na.rm = TRUE),
      max_y = max(Year, na.rm = TRUE)
    )
  
  common_window_nh <- year_bounds_nh %>%
    summarize(
      start = max(min_y),
      end = min(max_y)
    )
  
  # Filter to only include years in the common window
  data_global_nh <- data_nh %>%
    filter(Year >= common_window_nh$start,
           Year <= common_window_nh$end)
  
  # Compute net series
  net_series_nh <- data_global_nh %>%
    group_by(Year) %>%
    summarize(
      net_utility = sum(.data[[utility_col]], na.rm = FALSE),
      net_pop = sum(.data[[pop_col]], na.rm = FALSE)
    )
  
  return(net_series_nh)
}

#' Calculate correlations and elasticities between human and animal populations/utilities
#' 
#' @param data The input dataset
#' @param utility_col Column name for utility values
#' @param pop_col Column name for population values
#' @param potential_col Column name for welfare potential values
#' @return Dataframe with correlations and elasticities
calculate_correlations_elasticities <- function(data, 
                                                utility_col = "WR_utility", 
                                                pop_col = "WR_pop",
                                                potential_col = "WR_potential") {
  
  # Get non-human categories
  nh_cat <- setdiff(unique(data$Category), "Humans")
  
  # Get human vector with appropriate columns
  human_vec <- data %>% 
    filter(Category == "Humans") %>% 
    select(Year, aliveatanytime, all_of(utility_col))
  
  # Initialize results dataframe
  cor_and_elasticity <- data.frame(
    Category = character(), 
    cor_pop = numeric(), 
    cor_u = numeric(), 
    cor_hpop_au = numeric(), 
    cor_hu_apop = numeric(),
    e_pop = numeric(), 
    e_u = numeric(), 
    e_hpop_au = numeric(), 
    e_hu_apop = numeric(), 
    stringsAsFactors = FALSE
  )
  
  # Calculate correlations and elasticities for each non-human category
  for (category in nh_cat) {
    # Get category vector with appropriate columns
    cat_vec <- data %>% 
      filter(Category == category) %>% 
      select(Year, aliveatanytime, all_of(utility_col))
    
    # Identify min and max year for the correlation
    min_year <- max(min(cat_vec$Year), min(human_vec$Year), na.rm = TRUE)
    max_year <- min(max(cat_vec$Year), max(human_vec$Year), na.rm = TRUE)
    
    # Get population and utility vectors for the right time range
    human_pop_trunc <- human_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(aliveatanytime)
    
    human_u_trunc <- human_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(all_of(utility_col))
    
    cat_pop_trunc <- cat_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(aliveatanytime)
    
    cat_u_trunc <- cat_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(all_of(utility_col))
    
    # Check if we have enough non-NA values to calculate correlations
    has_valid_pop_data <- !all(is.na(human_pop_trunc)) && !all(is.na(cat_pop_trunc)) && 
      sum(!is.na(human_pop_trunc) & !is.na(cat_pop_trunc)) > 1
    
    has_valid_utility_data <- !all(is.na(human_u_trunc)) && !all(is.na(cat_u_trunc)) && 
      sum(!is.na(human_u_trunc) & !is.na(cat_u_trunc)) > 1
    
    # Run correlations with error handling
    cor_pop <- if(has_valid_pop_data) {
      tryCatch(cor(human_pop_trunc, cat_pop_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    cor_u <- if(has_valid_utility_data) {
      tryCatch(cor(human_u_trunc, cat_u_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    cor_hpop_au <- if(!all(is.na(human_pop_trunc)) && !all(is.na(cat_u_trunc)) && 
                      sum(!is.na(human_pop_trunc) & !is.na(cat_u_trunc)) > 1) {
      tryCatch(cor(human_pop_trunc, cat_u_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    cor_hu_apop <- if(!all(is.na(human_u_trunc)) && !all(is.na(cat_pop_trunc)) && 
                      sum(!is.na(human_u_trunc) & !is.na(cat_pop_trunc)) > 1) {
      tryCatch(cor(human_u_trunc, cat_pop_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    # Run regressions for elasticities with error handling
    e_pop <- if(has_valid_pop_data) {
      tryCatch(coef(lm(cat_pop_trunc ~ human_pop_trunc))[2], error = function(e) NA)
    } else NA
    
    e_u <- if(has_valid_utility_data) {
      tryCatch(coef(lm(cat_u_trunc ~ human_u_trunc))[2], error = function(e) NA)
    } else NA
    
    e_hpop_au <- if(!all(is.na(human_pop_trunc)) && !all(is.na(cat_u_trunc)) && 
                    sum(!is.na(human_pop_trunc) & !is.na(cat_u_trunc)) > 1) {
      tryCatch(coef(lm(cat_u_trunc ~ human_pop_trunc))[2], error = function(e) NA)
    } else NA
    
    e_hu_apop <- if(!all(is.na(human_u_trunc)) && !all(is.na(cat_pop_trunc)) && 
                    sum(!is.na(human_u_trunc) & !is.na(cat_pop_trunc)) > 1) {
      tryCatch(coef(lm(cat_pop_trunc ~ human_u_trunc))[2], error = function(e) NA)
    } else NA
    
    # Add results to result table
    cor_and_elasticity <- cor_and_elasticity %>% 
      add_row(
        Category = category, 
        cor_pop = cor_pop, 
        cor_u = cor_u, 
        cor_hpop_au = cor_hpop_au, 
        cor_hu_apop = cor_hu_apop,
        e_pop = e_pop, 
        e_u = e_u, 
        e_hpop_au = e_hpop_au, 
        e_hu_apop = e_hu_apop
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
#' @param net_series_nh The net series data without humans
#' @param utility_col Column name for utility values
#' @param pop_col Column name for population values
#' @param method_name Method name for plot titles
#' @param output_dir Directory for saving visualizations
c#' Create standard set of visualizations
#' 
#' @param data The processed dataset
#' @param net_series The net series data
#' @param net_series_nh The net series data without humans
#' @param utility_col Column name for utility values
#' @param pop_col Column name for population values
#' @param method_name Method name for plot titles
#' @param output_dir Directory for saving visualizations
create_visualizations <- function(data, 
                                  net_series, 
                                  net_series_nh,
                                  utility_col = "WR_utility", 
                                  pop_col = "WR_pop",
                                  method_name = "WR",
                                  output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create directories for different plot types
  pop_dir <- file.path(output_dir, "population_trends")
  if(!dir.exists(pop_dir)) {
    dir.create(pop_dir, recursive = TRUE)
  }
  
  # Filter out rows with NA values
  filtered_data <- data %>%
    filter(!is.na(.data[[utility_col]]), !is.na(aliveatanytime))
  
  # Plot 1: Population over time
  p1 <- ggplot(filtered_data, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = "Population Over Time", 
         y = "Population (alive at any time)", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(pop_dir, paste0("population_trends_", tolower(method_name), ".pdf")), 
         plot = p1, width = 10, height = 6)
  
  # Plot 2: Utility over time
  p2 <- ggplot(filtered_data, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = paste("Utility Over Time -", method_name, "Method"), 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), ".pdf")), 
         plot = p2, width = 10, height = 6)
  
  # Apply different filtering logic based on the method
  if(method_name == "WR") {
    # For WR method: exclude bees, then exclude fish
    
    # Filtered dataset (no bees)
    filtered_nb <- filtered_data %>% 
      filter(Category != "Bees")
    
    # Filtered dataset (no bees, no fish)
    filtered_nbf <- filtered_nb %>% 
      filter(Category != "Fish for Slaughter" & Category != "Fish")
    
    # Plot 3: Population over time (no bees)
    p3 <- ggplot(filtered_nb, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = "Population Over Time (No Bees)", 
           y = "Population (alive at any time)", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(pop_dir, paste0("population_trends_", tolower(method_name), "_nb.pdf")), 
           plot = p3, width = 10, height = 6)
    
    # Plot 4: Utility over time (no bees)
    p4 <- ggplot(filtered_nb, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = paste("Utility Over Time -", method_name, "Method (No Bees)"), 
           y = "Utility", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), "_nb.pdf")), 
           plot = p4, width = 10, height = 6)
    
    # Plot 5: Population over time (no bees, no fish)
    p5 <- ggplot(filtered_nbf, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = "Population Over Time (No Bees, No Fish)", 
           y = "Population (alive at any time)", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(pop_dir, paste0("population_trends_", tolower(method_name), "_nbf.pdf")), 
           plot = p5, width = 10, height = 6)
    
    # Plot 6: Utility over time (no bees, no fish)
    p6 <- ggplot(filtered_nbf, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = paste("Utility Over Time -", method_name, "Method (No Bees, No Fish)"), 
           y = "Utility", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), "_nbf.pdf")), 
           plot = p6, width = 10, height = 6)
    
  } else if(method_name == "NC") {
    # For NC method: exclude humans, then exclude fish, then exclude chickens
    
    # Filtered dataset (no humans)
    filtered_nh <- filtered_data %>% 
      filter(Category != "Humans")
    
    # Filtered dataset (no humans, no fish)
    filtered_nhf <- filtered_nh %>% 
      filter(Category != "Fish for Slaughter" & Category != "Fish")
    
    # Filtered dataset (no humans, no fish, no chickens)
    filtered_nhfc <- filtered_nhf %>% 
      filter(Category != "Chickens")
    
    # Plot 3: Population over time (no humans)
    p3 <- ggplot(filtered_nh, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = "Population Over Time (No Humans)", 
           y = "Population (alive at any time)", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(pop_dir, paste0("population_trends_", tolower(method_name), "_nh.pdf")), 
           plot = p3, width = 10, height = 6)
    
    # Plot 4: Utility over time (no humans)
    p4 <- ggplot(filtered_nh, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = paste("Utility Over Time -", method_name, "Method (No Humans)"), 
           y = "Utility", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), "_nh.pdf")), 
           plot = p4, width = 10, height = 6)
    
    # Plot 5: Population over time (no humans, no fish)
    p5 <- ggplot(filtered_nhf, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = "Population Over Time (No Humans, No Fish)", 
           y = "Population (alive at any time)", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(pop_dir, paste0("population_trends_", tolower(method_name), "_nhf.pdf")), 
           plot = p5, width = 10, height = 6)
    
    # Plot 6: Utility over time (no humans, no fish)
    p6 <- ggplot(filtered_nhf, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = paste("Utility Over Time -", method_name, "Method (No Humans, No Fish)"), 
           y = "Utility", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), "_nhf.pdf")), 
           plot = p6, width = 10, height = 6)
    
    # Plot 7: Population over time (no humans, no fish, no chickens)
    p7 <- ggplot(filtered_nhfc, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = "Population Over Time (No Humans, No Fish, No Chickens)", 
           y = "Population (alive at any time)", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(pop_dir, paste0("population_trends_", tolower(method_name), "_nhfc.pdf")), 
           plot = p7, width = 10, height = 6)
    
    # Plot 8: Utility over time (no humans, no fish, no chickens)
    p8 <- ggplot(filtered_nhfc, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = paste("Utility Over Time -", method_name, "Method (No Humans, No Fish, No Chickens)"), 
           y = "Utility", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), "_nhfc.pdf")), 
           plot = p8, width = 10, height = 6)
  }
  
  # Net utility plots - these remain the same for both methods
  # Plot: Net utility over time
  p_net <- ggplot(net_series, aes(x = Year, y = net_utility)) +
    geom_line(na.rm = FALSE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = paste("Net Utility Over Time -", method_name, "Method"), 
         y = "Net Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, paste0("net_utility_trends_", tolower(method_name), ".pdf")), 
         plot = p_net, width = 10, height = 6)
  
  # Plot: Net utility comparison (with and without humans)
  # Add labels to each dataset
  net_series_labeled <- net_series %>% mutate(Group = "With Humans")
  net_series_nh_labeled <- net_series_nh %>% mutate(Group = "Without Humans")
  
  # Combine the two datasets
  combined_series <- bind_rows(net_series_labeled, net_series_nh_labeled)
  
  p_comp <- ggplot(combined_series, aes(x = Year, y = net_utility, color = Group)) +
    geom_line(na.rm = FALSE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = paste("Net Utility Comparison -", method_name, "Method"), 
         y = "Net Utility", 
         x = "Year",
         color = "Dataset") +
    theme_minimal()
  
  ggsave(file.path(output_dir, paste0("net_utility_comparison_", tolower(method_name), ".pdf")), 
         plot = p_comp, width = 10, height = 6)
}

#' Create standard set of utility visualizations
#' 
#' @param data The processed dataset
#' @param net_series The net series data
#' @param net_series_nh The net series data without humans
#' @param utility_col Column name for utility values
#' @param pop_col Column name for population values
#' @param method_name Method name for plot titles
#' @param output_dir Directory for saving visualizations
#' @param skip_population_plots Whether to skip population trend visualizations
create_utility_visualizations <- function(data, 
                                          net_series, 
                                          net_series_nh,
                                          utility_col = "WR_utility", 
                                          pop_col = "WR_pop",
                                          method_name = "WR",
                                          output_dir = "visualizations",
                                          skip_population_plots = FALSE) {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Filter out rows with NA values
  filtered_data <- data %>%
    filter(!is.na(.data[[utility_col]]), !is.na(aliveatanytime))
  
  # Plot 1: Utility over time (all categories)
  p2 <- ggplot(filtered_data, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
    geom_line() +
    labs(title = paste("Utility Over Time -", method_name, "Method"), 
         y = "Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), ".pdf")), 
         plot = p2, width = 10, height = 6)
  
  # Apply different filtering logic based on the method
  if(method_name == "WR") {
    # For WR method: exclude bees, then exclude fish
    
    # Filtered dataset (no bees)
    filtered_nb <- filtered_data %>% 
      filter(Category != "Bees")
    
    # Filtered dataset (no bees, no fish)
    filtered_nbf <- filtered_nb %>% 
      filter(Category != "Fish for Slaughter" & Category != "Fish")
    
    # Plot: Utility over time (no bees)
    p4 <- ggplot(filtered_nb, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = paste("Utility Over Time -", method_name, "Method (No Bees)"), 
           y = "Utility", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), "_nb.pdf")), 
           plot = p4, width = 10, height = 6)
    
    # Plot: Utility over time (no bees, no fish)
    p6 <- ggplot(filtered_nbf, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = paste("Utility Over Time -", method_name, "Method (No Bees, No Fish)"), 
           y = "Utility", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), "_nbf.pdf")), 
           plot = p6, width = 10, height = 6)
    
  } else if(method_name == "NC") {
    # For NC method: exclude humans, then exclude fish, then exclude chickens
    
    # Filtered dataset (no humans)
    filtered_nh <- filtered_data %>% 
      filter(Category != "Humans")
    
    # Filtered dataset (no humans, no fish)
    filtered_nhf <- filtered_nh %>% 
      filter(Category != "Fish for Slaughter" & Category != "Fish")
    
    # Filtered dataset (no humans, no fish, no chickens)
    filtered_nhfc <- filtered_nhf %>% 
      filter(Category != "Chickens")
    
    # Plot: Utility over time (no humans)
    p4 <- ggplot(filtered_nh, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = paste("Utility Over Time -", method_name, "Method (No Humans)"), 
           y = "Utility", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), "_nh.pdf")), 
           plot = p4, width = 10, height = 6)
    
    # Plot: Utility over time (no humans, no fish)
    p6 <- ggplot(filtered_nhf, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = paste("Utility Over Time -", method_name, "Method (No Humans, No Fish)"), 
           y = "Utility", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), "_nhf.pdf")), 
           plot = p6, width = 10, height = 6)
    
    # Plot: Utility over time (no humans, no fish, no chickens)
    p8 <- ggplot(filtered_nhfc, aes(x = Year, y = .data[[utility_col]], colour = Category, group = interaction(Group, Category))) +
      geom_line() +
      labs(title = paste("Utility Over Time -", method_name, "Method (No Humans, No Fish, No Chickens)"), 
           y = "Utility", 
           x = "Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("utility_trends_", tolower(method_name), "_nhfc.pdf")), 
           plot = p8, width = 10, height = 6)
  }
  
  # Net utility plots - these remain the same for both methods
  # Plot: Net utility over time
  p_net <- ggplot(net_series, aes(x = Year, y = net_utility)) +
    geom_line(na.rm = FALSE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = paste("Net Utility Over Time -", method_name, "Method"), 
         y = "Net Utility", 
         x = "Year") +
    theme_minimal()
  
  ggsave(file.path(output_dir, paste0("net_utility_trends_", tolower(method_name), ".pdf")), 
         plot = p_net, width = 10, height = 6)
  
  # Plot: Net utility comparison (with and without humans)
  # Add labels to each dataset
  net_series_labeled <- net_series %>% mutate(Group = "With Humans")
  net_series_nh_labeled <- net_series_nh %>% mutate(Group = "Without Humans")
  
  # Combine the two datasets
  combined_series <- bind_rows(net_series_labeled, net_series_nh_labeled)
  
  p_comp <- ggplot(combined_series, aes(x = Year, y = net_utility, color = Group)) +
    geom_line(na.rm = FALSE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
    labs(title = paste("Net Utility Comparison -", method_name, "Method"), 
         y = "Net Utility", 
         x = "Year",
         color = "Dataset") +
    theme_minimal()
  
  ggsave(file.path(output_dir, paste0("net_utility_comparison_", tolower(method_name), ".pdf")), 
         plot = p_comp, width = 10, height = 6)
}

# Example usage:
# results <- analyze_welfare_data(
#   data_path = "first_pass/calc_tseries.xlsx",
#   welfare_level_method = "3282",
#   welfare_potential_method = "NC",
#   output_dir = "first_pass/results_3282_NC",
#   create_visualizations = TRUE
# )