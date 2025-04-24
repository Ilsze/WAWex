# Human Welfare Level Calculator
# This script calculates human welfare levels using different methods

library(pacman)
p_load(tidyverse, dplyr, ggplot2, gridExtra, mgcv, nleqslv, png, readr, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)

#' Calculate human welfare levels using different methods
#'
#' @param data_path Path to the World Bank data file
#' @param method Welfare level calculation method ("isoelastic" or "3282")
#' @param output_dir Directory for output files
#' @param visualize Whether to create visualizations
#' @return Dataframe with calculated welfare levels
calculate_human_welfare_levels <- function(data_path,
                                           method = "isoelastic",
                                           output_dir = "human_welfare_results",
                                           visualize = TRUE) {
  
  # Create output directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create visualization directory if needed
  vis_dir <- file.path(output_dir, "visualizations")
  if(visualize && !dir.exists(vis_dir)) {
    dir.create(vis_dir, recursive = TRUE)
  }
  
  # Load World Bank data
  cat("Loading World Bank data...\n")
  wb_data <- read_excel(data_path)
  
  # Calculate welfare levels based on method
  if(method == "isoelastic") {
    # Isoelastic utility method parameters
    cat("Calculating welfare levels using isoelastic method...\n")
    ubar <- -22.1713
    gamma <- 0.674252
    
    # Calculate welfare scores
    human_WL <- wb_data %>% 
      mutate(welfare_level = ubar + (GDP_filled^(1-gamma))/(1-gamma))
    
    # Aggregate to global level
    human_welfare_results <- human_WL %>% 
      select(Year, Country, Population, welfare_level) %>% 
      group_by(Year) %>% 
      summarise(
        Avg_welfare_per_capita = sum(welfare_level * Population, na.rm = TRUE) / sum(Population, na.rm = TRUE),
        Total_population = sum(Population, na.rm = TRUE),
        Total_welfare = sum(welfare_level * Population, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else if(method == "3282") {
    # 32-82 threshold method
    cat("Calculating welfare levels using 32-82 method...\n")
    
    # Calculate threshold as log midpoint between India and Canada GDP
    india_2018_GDP <- wb_data %>%
      filter(Country == "India", Year == 2018) %>%
      pull(GDP_filled)
    
    canada_2018_GDP <- wb_data %>% 
      filter(Country == "Canada", Year == 2018) %>% 
      pull(GDP_filled)
    
    threshold <- exp((log(india_2018_GDP) + log(canada_2018_GDP)) / 2)
    
    # Create welfare data with 32-82 classification
    welfare_data <- wb_data %>%
      mutate(welfare_points = ifelse(GDP_filled <= threshold, 32, 82)) %>%
      mutate(country_welfare = Population * welfare_points)
    
    # Aggregate by year
    human_welfare_results <- welfare_data %>%
      group_by(Year) %>%
      summarize(
        Total_welfare = sum(country_welfare, na.rm = TRUE),
        Total_population = sum(Population, na.rm = TRUE),
        Avg_welfare_per_capita = Total_welfare / Total_population,
        .groups = "drop"
      )
    
  } else {
    stop("Invalid method. Use 'isoelastic' or '3282'.")
  }
  
  # Create visualizations if requested
  if(visualize) {
    cat("Creating visualizations...\n")
    
    # Plot average welfare per capita over time
    p1 <- ggplot(human_welfare_results, aes(x = Year, y = Avg_welfare_per_capita)) +
      geom_line(color = "darkgreen", size = 1) +
      labs(title = paste("Average Global Welfare Per Capita Over Time (", method, " Method)"),
           x = "Year",
           y = "Average Welfare Per Capita") +
      theme_minimal()
    
    ggsave(file.path(vis_dir, paste0("average_human_welfare_", method, ".pdf")),
           plot = p1, width = 10, height = 6)
    
    # Plot total welfare over time
    p2 <- ggplot(human_welfare_results, aes(x = Year, y = Total_welfare)) +
      geom_line(color = "blue", size = 1) +
      labs(title = paste("Aggregate Global Welfare Over Time (", method, " Method)"),
           x = "Year",
           y = "Total Welfare") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
    
    ggsave(file.path(vis_dir, paste0("total_human_welfare_", method, ".pdf")),
           plot = p2, width = 10, height = 6)
    
    # If using 32-82 method, also create GDP distribution visualization
    if(method == "3282") {
      # Get relevant years for visualization
      years_to_plot <- c(1990, 2000, 2010, 2018)
      years_to_plot <- years_to_plot[years_to_plot %in% unique(wb_data$Year)]
      
      for(year in years_to_plot) {
        # Filter data for year
        year_data <- wb_data %>% filter(Year == year)
        
        # Calculate probability mass on each side of threshold
        below_threshold <- sum(year_data$GDP_filled < threshold, na.rm = TRUE)
        above_threshold <- sum(year_data$GDP_filled >= threshold, na.rm = TRUE)
        total_countries <- nrow(year_data)
        
        below_percent <- below_threshold / total_countries * 100
        above_percent <- above_threshold / total_countries * 100
        
        # Calculate histogram for y-scaling
        hist_data <- hist(year_data$GDP_filled, breaks = 30, plot = FALSE)
        max_count <- max(hist_data$counts)
        
        # Create histogram plot
        p_hist <- ggplot(year_data, aes(x = GDP_filled)) +
          geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
          geom_vline(xintercept = threshold, color = "red", linetype = "dashed", size = 1) +
          annotate("text", x = threshold*1.1, y = max_count*0.9, 
                   label = paste("Threshold =", round(threshold, 2)), 
                   color = "red", hjust = 0) +
          # Add probability mass labels
          annotate("label", x = threshold/3, y = max_count*0.8, 
                   label = paste0(round(below_percent, 1), "% of countries\n(", below_threshold, " countries)"), 
                   color = "darkblue", alpha = 0.8) +
          annotate("label", x = threshold*5, y = max_count*0.8, 
                   label = paste0(round(above_percent, 1), "% of countries\n(", above_threshold, " countries)"), 
                   color = "darkblue", alpha = 0.8) +
          scale_x_log10(labels = scales::comma) +
          labs(title = paste("Distribution of GDP per Capita (", year, ")", sep = ""),
               subtitle = paste("Log midpoint threshold:", round(threshold, 2)),
               x = "GDP per Capita (log scale)",
               y = "Count of Countries") +
          theme_minimal()
        
        ggsave(file.path(vis_dir, paste0("distribution_GDP_per_capita_", year, ".pdf")),
               plot = p_hist, width = 10, height = 6)
      }
    }
  }
  
  # Save results to file
  output_file <- file.path(output_dir, paste0("human_wWL_", method, ".xlsx"))
  write.xlsx(human_welfare_results, output_file, rowNames = FALSE)
  cat("Results saved to", output_file, "\n")
  
  # Return results
  return(human_welfare_results)
}

#' Prepare human welfare data for integration with animal welfare analysis
#'
#' @param human_welfare_data Human welfare results from calculate_human_welfare_levels
#' @param output_file Path to save the prepared human data
#' @return Dataframe formatted for integration with calc_tseries
prepare_human_data_for_integration <- function(human_welfare_data, output_file = NULL) {
  
  # Format data for integration
  human_data_formatted <- human_welfare_data %>%
    mutate(
      Category = "Humans",
      Group = "Humans",
      aliveatanytime = Total_population,
      Welfare_level = Avg_welfare_per_capita,
      WR_potential = 1,  # Humans are the reference with potential = 1
      NC_potential = 1   # Humans are the reference with potential = 1
    ) %>%
    select(Year, Category, Group, aliveatanytime, Welfare_level, WR_potential, NC_potential)
  
  # Save to file if specified
  if(!is.null(output_file)) {
    write.xlsx(human_data_formatted, output_file, rowNames = FALSE)
    cat("Prepared human data saved to", output_file, "\n")
  }
  
  return(human_data_formatted)
}

# Example usage:
# # Calculate human welfare using isoelastic method
# human_welfare_isoelastic <- calculate_human_welfare_levels(
#   data_path = "dat/world_bank/world_bank_pop_gdp_clean.xlsx",
#   method = "isoelastic",
#   output_dir = "first_pass/human_welfare_isoelastic",
#   visualize = TRUE
# )
# 
# # Format for integration with animal welfare data
# human_data_for_animals_isoelastic <- prepare_human_data_for_integration(
#   human_welfare_isoelastic, 
#   output_file = "first_pass/human_data_for_animals_isoelastic.xlsx"
# )
# 
# # Calculate human welfare using 32-82 method
# human_welfare_3282 <- calculate_human_welfare_levels(
#   data_path = "dat/world_bank/world_bank_pop_gdp_clean.xlsx",
#   method = "3282",
#   output_dir = "first_pass/human_welfare_3282",
#   visualize = TRUE
# )
# 
# # Format for integration with animal welfare data
# human_data_for_animals_3282 <- prepare_human_data_for_integration(
#   human_welfare_3282, 
#   output_file = "first_pass/human_data_for_animals_3282.xlsx"
# )