#' Calculate wild fish population using Sea Around Us database and reference value
#' 
#' @param reference_count The reference count of wild fish (6.19825e+14 for 2011)
#' @param reference_year The reference year for wild fish count (2011)
#' @param data_path Path to the Sea Around Us database file
#' @param sheet_name Name of the sheet containing time series data
#' @return DataFrame with years and wild fish population estimates
calculate_wild_fish_population <- function(reference_count = 6.19825e+14,
                                           reference_year = 2011,
                                           data_path = "dat/sea_around_us/RAMLDBv4.66.xlsx",
                                           sheet_name = "timeseries_values_views") {
  
  # Check if the data file exists
  if(!file.exists(data_path)) {
    stop("Sea Around Us database file not found: ", data_path)
  }
  
  # Load the Sea Around Us data from the specified sheet
  cat("Loading Sea Around Us database from", data_path, "...\n")
  fish_data <- read_excel(data_path, sheet = sheet_name)
  
  # Rename 'year' column to 'Year' for consistency if needed
  if("year" %in% names(fish_data) && !"Year" %in% names(fish_data)) {
    fish_data <- fish_data %>%
      rename(Year = year)
    cat("Renamed 'year' column to 'Year' for consistency.\n")
  }
  
  # Check if required columns exist
  required_cols <- c("stockid", "stocklong", "Year", "TN")
  if(!all(required_cols %in% names(fish_data))) {
    missing_cols <- setdiff(required_cols, names(fish_data))
    stop("Missing required columns in fish data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Step 1: Remove rows with NA in TN
  fish_data_clean <- fish_data %>%
    filter(!is.na(TN))
  
  cat("Removed", nrow(fish_data) - nrow(fish_data_clean), " out of ", nrow(fish_data), " rows due to NA values in TN. \n")
  
  # Step 2: Calculate year-on-year proportional change for each stockid
  fish_data_changes <- fish_data_clean %>%
    # Group by stockid to calculate changes within each stock
    group_by(stockid) %>%
    # Sort by Year within each group
    arrange(stockid, Year) %>%
    # Calculate the change from previous year
    mutate(
      prev_TN = lag(TN),
      prop_change = (TN - prev_TN) / prev_TN #e.g 10 -> 15 will give 5/10 = 0.5, implying + 50% extra abundance. 15 -> 10 will give -5/15 = -0.33, implying -33% abundance. 
    ) %>%
    # Remove NA values (first year for each stock will have NA change)
    filter(!is.na(prop_change)) %>%
    # Keep only necessary columns
    select(stockid, stocklong, Year, TN, prop_change)
  
  # Step 3: Create aggregated dataset with weighted average of prop_change
  aggregated_changes <- fish_data_changes %>%
    # Group by Year to aggregate across all stocks
    group_by(Year) %>%
    # Calculate weighted average of prop_change, weighted by TN
    summarize(
      prop_change_agg = weighted.mean(prop_change, w = TN, na.rm = TRUE),
      total_TN = sum(TN, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Find min and max years in the aggregated data
  min_year <- min(aggregated_changes$Year)
  max_year <- max(aggregated_changes$Year)
  
  # Create a complete sequence of years
  all_years <- data.frame(
    Year = min_year:max_year
  )
  
  # Join with aggregated changes to ensure all years are included
  complete_changes <- all_years %>%
    left_join(aggregated_changes, by = "Year") %>%
    # Fill NA values with 0 (no change) if any years are missing data
    mutate(
      prop_change_agg = ifelse(is.na(prop_change_agg), 0, prop_change_agg)
    )
  
  # Step 4: Calculate absolute fish counts based on reference year and changes
  # First, check if reference year exists in the data
  if(!(reference_year %in% complete_changes$Year)) {
    stop("Reference year ", reference_year, " not found in processed data.")
  }
  
  # Get the range of years
  years_range <- min_year:max_year
  
  # Initialize population vector with reference count for reference year
  populations <- rep(NA, length(years_range))
  names(populations) <- years_range ##this is not a dataframe, it just helps with access
  populations[as.character(reference_year)] <- reference_count
  
  # Calculate forward from reference year
  if(reference_year < max_year) {
    for(year in (reference_year + 1):max_year) {
      year_idx <- which(complete_changes$Year == year) ##recall that complete_changes is the dataset with Year and prop_change_agg as columns
      prev_year_pop <- populations[as.character(year - 1)] 
      change <- complete_changes$prop_change_agg[year_idx]
      populations[as.character(year)] <- prev_year_pop * (1 + change)
    }
  }
  
  # Calculate backward from reference year
  if(reference_year > min_year) {
    for(year in (reference_year - 1):min_year) {
      year_idx <- which(complete_changes$Year == (year + 1))
      next_year_pop <- populations[as.character(year + 1)]
      change <- complete_changes$prop_change_agg[year_idx]
      # To go backwards, we need to reverse the change formula
      populations[as.character(year)] <- next_year_pop / (1 + change)
    }
  }
  
  # Create final dataset
  fish_populations <- data.frame(
    Year = years_range,
    population = unname(populations)
  )
  
  cat("Wild fish population estimates calculated for years", min_year, "to", max_year, ".\n")
  
  return(fish_populations)
}