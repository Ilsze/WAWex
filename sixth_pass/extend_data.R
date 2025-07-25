#' Visualize original vs extended trends for quality checking
#' Called by prepare_data_for_net_series
#' 
#' @param original_data Original dataset
#' @param extended_data Extended dataset 
#' @param output_dir Directory to save comparison plots
create_trend_extension_plots <- function(original_data, extended_data, output_dir) {
  
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get categories that were extended
  original_ranges <- original_data %>%
    group_by(Category) %>%
    summarize(min_year = min(Year), max_year = max(Year))
  
  extended_ranges <- extended_data %>%
    group_by(Category) %>%
    summarize(min_year = min(Year), max_year = max(Year))
  
  extended_categories <- original_ranges %>%
    left_join(extended_ranges, by = "Category", suffix = c("_orig", "_ext")) %>%
    filter(min_year_orig != min_year_ext | max_year_orig != max_year_ext) %>%
    pull(Category)
  
  # Create comparison plots for each extended category
  for(category in extended_categories) {
    
    orig_data <- original_data %>% filter(Category == category)
    ext_data <- extended_data %>% filter(Category == category)
    
    # Mark original vs extended data
    plot_data <- bind_rows(
      orig_data %>% mutate(Data_Type = "Original"),
      ext_data %>% filter(!Year %in% orig_data$Year) %>% mutate(Data_Type = "Extended")
    )
    
    p <- ggplot(plot_data, aes(x = Year, y = aliveatanytime, color = Data_Type)) +
      geom_line(size = 1) +
      geom_point(size = 0.8) +
      scale_color_manual(values = c("Original" = "black", "Extended" = "red")) +
      labs(title = paste("Trend Extension for", category),
           y = "Population (alive at any time)",
           x = "Year",
           color = "Data Type") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0(gsub("[^A-Za-z0-9]", "_", category), "_extension.pdf")), 
           plot = p, width = 10, height = 6)
  }
  
  cat("Trend extension plots saved to:", output_dir, "\n")
}





#' Extend animal population trends to match target time range for net series calculations
#' 
#' @param data The input dataset with all categories
#' @param output_dir Directory for saving trend extension plots (default: "visualizations")
#' @param target_year_range Year range to extend data to (default: 1960:2023)
#' @param endpoint_years Number of years to use for endpoint trend calculation (default 5)
#' @return Extended dataset suitable for net series calculations
prepare_data_for_net_series <- function(data, 
                                        output_dir = "visualizations",
                                        target_year_range = 1960:2023,
                                        endpoint_years = 5) {
  
  # Create output directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  target_min <- min(target_year_range)
  target_max <- max(target_year_range)
  
  cat("Extending animal trends for net series calculation to cover", target_min, "to", target_max, "\n")
  
  # Store original data for comparison plots
  original_data <- data
  
  #Process each category separately
  extended_data <- data %>%
    group_by(Category) %>%
    group_modify(~ {
      category_data <- .x
      category_name <- .y$Category
      
      # Check minimum data requirements
      if(nrow(category_data) < 3) {
        stop(paste("Not enough data points for", category_name, 
                   "- need at least 3 points, found", nrow(category_data)))
      }
      
      # Get the original year range and sort data
      original_years <- range(category_data$Year)
      category_data <- category_data %>% arrange(Year)
      
      cat("\n", category_name, "original range:", original_years[1], "to", original_years[2])
      
      # Initialize with original data
      extended_data <- category_data
      
      # BACKWARD EXTENSION (if needed)
      if(original_years[1] > target_min) {
        years_before <- target_min:(original_years[1] - 1)
        
        # Use first few years to establish backward trend
        early_data <- category_data %>% 
          head(min(endpoint_years, nrow(category_data)))
        
        if(nrow(early_data) >= 2) {
          # Fit linear trend to early years
          early_model <- lm(aliveatanytime ~ Year, data = early_data)
          
          # Project backward (ensuring non-negative values)
          backward_predictions <- predict(early_model, 
                                          newdata = data.frame(Year = years_before))
          backward_predictions <- pmax(backward_predictions, 0)
          
          # Create backward extension rows by duplicating structure
          backward_rows <- category_data[rep(1, length(years_before)), ]
          backward_rows$Year <- years_before
          backward_rows$aliveatanytime <- backward_predictions
          
          extended_data <- bind_rows(backward_rows, extended_data)
          
          cat(" - extended backward by", length(years_before), "years")
        }
      }
      
      # FORWARD EXTENSION (if needed)
      if(original_years[2] < target_max) {
        years_after <- (original_years[2] + 1):target_max
        
        # Use last few years to establish forward trend
        recent_data <- category_data %>% 
          tail(min(endpoint_years, nrow(category_data)))
        
        if(nrow(recent_data) >= 2) {
          # Calculate coefficient of variation to check stability
          recent_mean <- mean(recent_data$aliveatanytime)
          recent_cv <- sd(recent_data$aliveatanytime) / recent_mean
          
          if(recent_cv < 0.05) {
            # Stable trend: use mean of recent years
            forward_predictions <- rep(recent_mean, length(years_after))
            cat(" - stable trend, using mean")
          } else {
            # Trending: fit linear model to recent years
            recent_model <- lm(aliveatanytime ~ Year, data = recent_data)
            forward_predictions <- predict(recent_model, 
                                           newdata = data.frame(Year = years_after))
            forward_predictions <- pmax(forward_predictions, 0)
            cat(" - trending, using linear extrapolation")
          }
          
          # Create forward extension rows
          last_row_index <- nrow(category_data)
          forward_rows <- category_data[rep(last_row_index, length(years_after)), ]
          forward_rows$Year <- years_after
          forward_rows$aliveatanytime <- forward_predictions
          
          extended_data <- bind_rows(extended_data, forward_rows)
          
          cat(" - extended forward by", length(years_after), "years")
        }
      }
      
      # TRUNCATION: Keep only years within target range
      extended_data <- extended_data %>%
        filter(Year >= target_min & Year <= target_max) %>%
        arrange(Year)
      
      # Report if truncation occurred
      if(original_years[1] < target_min || original_years[2] > target_max) {
        cat(" - truncated to target range")
      }
      
      cat("\n")
      
      return(extended_data) #return if the data wasn't extended, but truncated
    }) %>%
    ungroup()
  
  #If extensions were made, recalculate utility columns
  # Check for required columns first
  required_cols <- c("aliveatanytime", "WR_potential", "NC_potential", 
                     "Welfare_level", "forebrain_neurons")
  missing_cols <- setdiff(required_cols, names(extended_data))
  
  if(length(missing_cols) > 0) {
    stop("Missing required columns for utility calculations: ", 
         paste(missing_cols, collapse = ", "))
  }
  
  # Calculate utility metrics
  # note that only variables coming from aliveatanytime are recalculated. All 
  # variables are actually extended by category_data[rep...] code
  extended_data <- extended_data %>%
    mutate(
      WR_apot = aliveatanytime * WR_potential,
      WR_utility = aliveatanytime * WR_potential * Welfare_level,
      NC_tot = aliveatanytime * forebrain_neurons,
      NC_apot = aliveatanytime * NC_potential,
      NC_utility = aliveatanytime * NC_potential * Welfare_level, 
    )
  
  # Create trend extension plots directory and sanity check
  extension_plots_dir <- file.path(output_dir, "trend_extension_plots")
  if(!dir.exists(extension_plots_dir)) {
    dir.create(extension_plots_dir, recursive = TRUE)
  }
  
  # Create trend extension plots for quality checking
  cat("Creating trend extension plots for quality checking...\n")
  create_trend_extension_plots(data, extended_data, extension_plots_dir)
  
  cat("Data preparation for net series complete. Extended data covers", 
      min(target_year_range), "to", max(target_year_range), "\n")
  cat("Trend extension plots saved to:", extension_plots_dir, "\n")
  
  # Ensure save directory for extended data exists
  if(!dir.exists("fifth_pass/dat/")) {
    dir.create("fifth_pass/dat/", recursive = TRUE)
  }
  
  # Save extended data for reuse in cor and elas calculations
  write.xlsx(extended_data, "fifth_pass/dat/extended_integrated_calc_tseries.xlsx")
}