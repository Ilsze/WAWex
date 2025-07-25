#' Create tree maps for population, nc_tot, and welfare score range
#' n_wta_wfi_fbe
#' 
#' @param data The extended_integrated_calc_tseries dataset
#' @param output_dir Directory for saving tables
#' @return NULL (saves tables to files)
create_treemaps_n_wta_wfi_fbe <- function(data, output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(paste0(output_dir, "/treemaps"))) {
    dir.create(paste0(output_dir, "/treemaps"), recursive = TRUE)
  }

  cat("Creating treemaps...\n")
  
  # Define color palette matching the four-panel plots
  group_colors <- c(
    "Humans" = "#2E86AB",
    "Wild Animals" = "#8B4513", 
    "Farmed Animals" = "#20B2AA"
  )
  
  # Filter data for 2023 and exclude specified categories
  excluded_categories <- c("Wild terrestrial arthropods", "Wild fish", "Bees")
  
  #Prepare data for treemap
  data <- data %>% 
    filter(Year == 2023, 
           !Category %in% excluded_categories) %>% 
    mutate(
      Animal_Category = case_when(
        Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)") ~ paste("Farmed", Category),
        TRUE ~ Category
      ),
      Group_Clean = case_when(
        Category == "Humans" ~ "Humans",
        Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)") ~ "Farmed Animals",
        Group == "Wild Animals" ~ "Wild Animals",
        TRUE ~ "Other"
      )
    )
  
  #Prepare population data
  pop_data <- data %>% 
    filter(!is.na(aliveatanytime)) %>%
    arrange(desc(aliveatanytime)) %>%
    select(Animal_Category, aliveatanytime, Group_Clean)
  
  #Prepare nc_tot data
  pop_data <- data %>% 
    filter(!is.na(NC_tot)) %>%
    arrange(desc(NC_tot)) %>%
    select(Animal_Category, NC_tot, Group_Clean)
  
  #Prepare NC-based welfare score range data
  nc_range_data <- data %>% 
    mutate(NC_range <- NC_potential * )
  
  
}


#this other thing about moving extend_animal_trends to integration.R
#' Extend animal population trends to match human time range using local trend-aware methods
#' This is for the purpose of net series such as NC_utility, WR_utility, and NC_Tot. 
#' Called by prepare_data_for_net_series
#' 
#' @param data The input dataset with all categories
#' @param target_year_range The target year range set by prepare_data_for_net_series
#' @param endpoint_years Number of years to use for endpoint trend calculation (default 5)
#' @return Dataset with extended trends for all categories
extend_animal_trends <- function(data, target_year_range, endpoint_years = 5) {
  
  target_min <- min(target_year_range)
  target_max <- max(target_year_range)
  
  cat("Target extension range:", target_min, "to", target_max, "\n")
  
  # Process each category separately
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
  
  return(extended_data)
}




