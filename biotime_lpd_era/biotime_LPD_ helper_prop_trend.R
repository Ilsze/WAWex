##this file engages with LPD and BioTIME datasets

##function that checks whether any "record" differs in their "study"
# Load required library
check_record_study <- function(data) {
  # Group by the specified columns
  grouped_data <- data %>%
    group_by(ABUNDANCE, YEAR, SAMPLE_DESC, GENUS_SPECIES)
  
  # Check for any groups with more than one unique STUDY_ID
  problematic_groups <- grouped_data %>%
    summarise(unique_study_ids = n_distinct(STUDY_ID)) %>%
    filter(unique_study_ids > 1)
  
  if (nrow(problematic_groups) == 0) {
    cat("No issues found. All combinations of ABUNDANCE, YEAR, SAMPLE_DESC, and GENUS_SPECIES have unique STUDY_ID values.\n")
  } else {
    cat("Found", nrow(problematic_groups), "problematic group(s):\n")
    print(problematic_groups)
    
    # Get details of the problematic rows
    problematic_rows <- grouped_data %>%
      group_by(ABUNDANCE, YEAR, SAMPLE_DESC, GENUS_SPECIES) %>%
      filter(n_distinct(STUDY_ID) > 1)
    
    cat("\nDetailed view of problematic rows:\n")
    View(problematic_rows)
  }
}

# Usage:
# check_unique_combinations(your_dataset)

##function pivots by species, ensures we have all years from min to max, DOES NOT remove rows 
## with 1 or fewer time series data points, and reorders columns
raw_prep <- function(dat) {
  # Identify min and max years from the filtered data
  min_year = min(dat$YEAR)
  max_year = max(dat$YEAR)
  
  #pivot
  dat <- dat %>% 
    group_by(GENUS_SPECIES, YEAR) %>%
    summarise(ABUNDANCE = sum(ABUNDANCE, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      id_cols = GENUS_SPECIES,
      names_from = YEAR,
      values_from = ABUNDANCE,
      names_prefix = "y_"
    )
  
  print(paste0("(", min_year, " to ", max_year, ")"))
  
  # To ensure we have all years from min to max:
  all_years <- seq(min_year, max_year)
  missing_years <- setdiff(all_years, as.numeric(gsub("y_", "", colnames(select(dat, -GENUS_SPECIES)))))
  
  if (length(missing_years) > 0) {
    dat[paste0("y_", missing_years)] <- NA
  }
  
  # Reorder columns
  year_cols <- grep("^y_", colnames(dat), value = TRUE)
  dat <- dat %>%
    select(GENUS_SPECIES, sort(year_cols)) %>% 
    #and ensure all y_columns are numeric
    mutate(across(starts_with("y_"), as.numeric))
  return(dat)
} 

##function combines main dataset with metadata
add_mdat <- function(dat, mdat) {
  merge(dat, mdat, by = "STUDY_ID", all.x = TRUE)
}

##function pivots years into the same row WITHOUT COLLAPSING ROWS, ensures we have 
## all years from min to max for that group, removes rows with 1
## or fewer time series data points, and reorders columns
prop_prep <- function(dat) {
  #average abundance-years for all rows that share the same important variables as listed
  dat <- dat %>% 
    # takes the median of abundance for records that share a method, species, date, depth, grain, and abundance type
    group_by(STUDY_ID, GENUS_SPECIES, YEAR, LATITUDE, LONGITUDE, PLOT, DEPTH, 
             GRAIN_SIZE_TEXT, GRAIN_SQ_KM, AREA_SQ_KM, ABUNDANCE_TYPE) %>%
    #want to summarise by abundance still taking the absolute sum because it 
    # should be the same method, area, year, and species
    summarise(ABUNDANCE = median(ABUNDANCE, na.rm = TRUE), .groups = "drop")
  
  #pivot
  dat_wide <- dat %>% 
    # this pivoting maintains the above groups
    pivot_wider( 
      id_cols = c(STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE, PLOT, DEPTH, 
                  GRAIN_SIZE_TEXT, GRAIN_SQ_KM, AREA_SQ_KM, ABUNDANCE_TYPE),
      names_from = YEAR,
      values_from = ABUNDANCE,
      names_prefix = "y_"
    )
  
  # Identify min and max years from the filtered data
  year_cols <- grep("^y_", colnames(dat_wide), value = TRUE)
  years <- as.numeric(gsub("y_", "", year_cols))
  min_year <- min(years, na.rm = TRUE)
  max_year <- max(years, na.rm = TRUE)
  
  print(paste0("(", min_year, " to ", max_year, ")"))
  
  # To ensure we have all years from min to max:
  all_years <- seq(min_year, max_year)
  missing_years <- setdiff(all_years, years)
  
  if (length(missing_years) > 0) {
    dat_wide[paste0("y_", missing_years)] <- NA
  }
  
  # Reorder columns
  year_cols <- grep("^y_", colnames(dat_wide), value = TRUE)
  dat_wide <- dat_wide %>%
    select(STUDY_ID, GENUS_SPECIES, sort(year_cols), LATITUDE, LONGITUDE, ABUNDANCE_TYPE, 
           GRAIN_SIZE_TEXT, GRAIN_SQ_KM, AREA_SQ_KM, PLOT, DEPTH) %>% 
    #and ensure all y_columns are numeric
    mutate(across(starts_with("y_"), as.numeric))
  return(dat_wide)
} 

prop_prep_ex <- function(dat, min_year, max_year) {
  #average abundance-years for all rows that share the same important variables as listed
  dat <- dat %>% 
    #group_by maintains differentiation of rows over which we might expect different methods/
    # locations to lead to large swings in abundance values that we'd want to only 
    # aggregate after having found proportional change
    group_by(STUDY_ID, GENUS_SPECIES, YEAR, LATITUDE, LONGITUDE, PLOT, DEPTH, 
             GRAIN_SIZE_TEXT, GRAIN_SQ_KM, AREA_SQ_KM, ABUNDANCE_TYPE) %>%
    #want to summarise by abundance still taking the absolute sum because it 
    # should be the same method, area, year, and species
    summarise(ABUNDANCE = median(ABUNDANCE, na.rm = TRUE), .groups = "drop")
  
  #pivot
  dat_wide <- dat %>% 
    # this pivoting maintains the above groups
    pivot_wider( 
      id_cols = c(STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE, PLOT, DEPTH, 
                  GRAIN_SIZE_TEXT, GRAIN_SQ_KM, AREA_SQ_KM, ABUNDANCE_TYPE),
      names_from = YEAR,
      values_from = ABUNDANCE,
      names_prefix = "y_"
    )
  
  # Identify min and max years from the filtered data
  year_cols <- grep("^y_", colnames(dat_wide), value = TRUE)
  years <- as.numeric(gsub("y_", "", year_cols))
  
  print(paste0("(", min_year, " to ", max_year, ")"))
  
  # To ensure we have all years from min to max:
  all_years <- seq(min_year, max_year)
  missing_years <- setdiff(all_years, years)
  
  if (length(missing_years) > 0) {
    dat_wide[paste0("y_", missing_years)] <- NA
  }
  
  # Reorder columns
  year_cols <- grep("^y_", colnames(dat_wide), value = TRUE)
  dat_wide <- dat_wide %>%
    select(STUDY_ID, GENUS_SPECIES, sort(year_cols), LATITUDE, LONGITUDE, ABUNDANCE_TYPE, 
           GRAIN_SIZE_TEXT, GRAIN_SQ_KM, AREA_SQ_KM, PLOT, DEPTH) %>% 
    #and ensure all y_columns are numeric
    mutate(across(starts_with("y_"), as.numeric))
  return(dat_wide)
} 
  

##function that fills missing data observation count version
fill_missing<- function(row, i, group) {
  #get year cols and years as numbers
  year_colnames <- grep("^y_", colnames(row), value = TRUE) #colnames assoc with years
  years <- as.numeric(gsub("y_", "", year_colnames)) #vector of years themselves
  year_cols <- row[year_colnames]
  year_values <- unlist(row[year_colnames])
  
  
  genus_species = row$GENUS_SPECIES
  
  existing_data <- !is.na(year_cols) #true if data exists for that year, false if it doesn't. Data type: Matrix 
  existing_values <- year_cols[existing_data]
  
  print(paste0("Processing row ", i, " :", genus_species))
  
  # Handle cases with 0, 1, uniform non-NA values, or too few non-NA values
  if (sum(existing_data) == 0) {
    stop("Error: no data available for row", i, "_", genus_species)
  } else if (length(unique(existing_values)) == 1) {
    non_na_value <- unique(existing_values)
    year_cols[is.na(year_cols)] <- non_na_value
    row <- cbind(row[c("STUDY_ID", "GENUS_SPECIES")], year_cols, 
                 row[c("LATITUDE", "LONGITUDE", "ABUNDANCE_TYPE", 
                       "GRAIN_SIZE_TEXT", "GRAIN_SQ_KM", "AREA_SQ_KM", "PLOT", 
                       "DEPTH")])      
    #print("uniform non-NA values: median used")
    return(row)
  } else if (length(existing_values) < 5) {
    median <- median(existing_values)
    year_cols[is.na(year_cols)] <- median
    row <- cbind(row[c("STUDY_ID", "GENUS_SPECIES")], year_cols, 
                 row[c("LATITUDE", "LONGITUDE", "ABUNDANCE_TYPE", 
                       "GRAIN_SIZE_TEXT", "GRAIN_SQ_KM", "AREA_SQ_KM", "PLOT", 
                       "DEPTH")])
    #print("too few non-NA values for meaningful trend: median used")
    return(row)
  } else {
    #print("enough non-NA values that aren't uniform")
  }
  
  # Check for linear trend
  correlation <- cor(years[existing_data], existing_values) 
  #print(paste0("correlation: ", correlation))
  threshold <- 0.7 + 0.3 * (1 - sum(existing_data) / length(year_cols))  # Adaptive threshold. 
  #print(paste0("threshold: ", threshold))
  # The second product term is the proportion of missing data. As the proportion
  # of missing data rises, the standard for the strength of the correlation for 
  # it to be counted as a trend also rises. 
  #There's also the consideration that if there are, say 20 non-NA entries in a 
  # row, and a strong positive correlation in the first 10, and a strong 
  # negative correlation in the last ten, then maybe this code wouldn't pick up 
  # on that. Moving window correlation and Segmented regression may be viable 
  # future options. For now, I keep things simple. 
  
  if (abs(correlation) >= threshold) { ##TO DO WHEN NEED CALLS: THE CONTENTS OF
    ## THIS BRACKET HASN'T BEEN CHECKED. SEE THIS CLAUDE CONVO: https://claude.ai/chat/bd141478-bf70-4f32-be70-f5d832725deb
    ## FOR DETAILS
    print(paste("correlation exceeds threshold for ", genus_species, ", row ", i))
    # Linear trend exists
    lm_model <- lm(as.numeric(year_cols) ~ years)
    
    # Function to fill intervening gaps
    fill_intervening <- function(gap_start, gap_end) {
      gap_size <- gap_end - gap_start + 1
      max_to_fill <- floor(gap_size / 2)
      left_data <- sum(existing_data[1:gap_start]) 
      right_data <- sum(existing_data[gap_end:length(year_values)]) 
      # Fill left side
      left_to_fill <- min(max_to_fill, floor(left_data / 2))
      if (left_to_fill > 0) {
        left_predicted <- predict(lm_model, newdata = data.frame(years = years[gap_start:(gap_start + left_to_fill - 1)]))
        year_values[gap_start:(gap_start + left_to_fill - 1)] <- left_predicted
      }
      # Fill right side
      right_to_fill <- min(max_to_fill, floor(right_data / 2))
      if (right_to_fill > 0) {
        right_predicted <- predict(lm_model, newdata = data.frame(years = years[(gap_end - right_to_fill + 1):gap_end]))
        year_values[(gap_end - right_to_fill + 1):gap_end] <- right_predicted
      }
      # Fill middle (if any space left)
      remaining_gap <- (gap_start + left_to_fill):(gap_end - right_to_fill)
      if (length(remaining_gap) > 0) {
        # Get median of up to three values on each side
        left_values <- year_values[max(1, gap_start + left_to_fill - 3):(gap_start + left_to_fill - 1)] #incorrect
        right_values <- year_values[(gap_end - right_to_fill + 1):min(length(year_values), gap_end - right_to_fill + 3)]
        left_median <- if (length(left_values) > 0) median(left_values, na.rm = TRUE) else stop("No left median for interior gap")
        right_median <- if (length(right_values) > 0) median(right_values, na.rm = TRUE) else stop("No right median for interior gap")
        middle_fill <- mean(c(left_median, right_median), na.rm = TRUE)
        year_values[remaining_gap] <- middle_fill
      }
      return(year_values)
    }
    
    # Fill intervening gaps
    ##defines start and end points of gaps in the data. start include NA, end is after NA
    gaps <- which(diff(c(TRUE, existing_data, TRUE)) != 0) 
    ##This loop goes through each gap and fills it using the fill_intervening function. It makes sense, trust me
    
    ##see whether non boundary gaps exist and deal with them by calling fill_intervening
    if (length(gaps) > 2) {
      start_index <- if (gaps[1] == 1) 3 else 1
      end_index <- if (gaps[length(gaps)] == length(year_values) + 1) length(gaps) - 2 else length(gaps)
      if (end_index > start_index) {
        for (j in seq(start_index, end_index, by = 2)) {
          year_values <- fill_intervening(gaps[j], gaps[j+1] - 1)
        }
      }
    }
    
    # Handle boundary gaps
    left_boundary <- which(!is.na(year_values))[1] - 1 #how many leading values are NA?
    right_boundary <- length(year_values) - which(!is.na(year_values))[sum(!is.na(year_values))] #how many trailing values are NA? 
    
    if (left_boundary > 0) {
      # If there's a left boundary gap
      # Determine how much to fill: either the whole gap or half of all non-NA values before the filling of interior gaps, whichever is smaller
      left_to_fill <- min(left_boundary, floor(sum(existing_data) / 2))
      if (left_to_fill > 0) {
        # Use the linear model to predict values for the left boundary
        left_fill <- predict(lm_model, newdata = data.frame(years = years[(left_boundary - left_to_fill + 1):left_boundary]))
        # Fill the right side of the left boundary gap with predicted values
        year_values[(left_boundary - left_to_fill + 1):left_boundary] <- left_fill
        if (left_boundary > left_to_fill) {
          # If there's still gap left, fill the rest with the median of the three nearest non-NA values
          first_non_na <- which(!is.na(year_values))[1]
          right_values <- year_values[first_non_na:min(length(year_values),first_non_na + 2)]
          right_median <- if (length(right_values) > 0) median(right_values, na.rm = TRUE) else stop("no right median in boundary gap")
          year_values[1:(left_boundary - left_to_fill)] <- right_median
          #print("gap remained in left boundary after linear fill: median filled")
          }
      }
    }
    
    # Handle right boundary gap
    if (right_boundary > 0) {
      # If there's a right boundary gap
      # Determine how much to fill: either the whole gap or half of all non-NA values before the filling of interior gaps, whichever is smaller
      right_to_fill <- min(right_boundary, floor(sum(existing_data) / 2))
      if (right_to_fill > 0) {
        # Use the linear model to predict values for the right boundary
        right_fill <- predict(lm_model, newdata = data.frame(years = years[(length(year_values) - right_boundary + 1):(length(year_values) - right_boundary + right_to_fill)]))
        # Fill the left side of the right boundary gap with predicted values
        year_values[(length(year_values) - right_boundary + 1):(length(year_values) - right_boundary + right_to_fill)] <- right_fill
        if (right_boundary > right_to_fill) {
          # If there's still gap left, fill the rest with the median of the three nearest non-NA values
          last_non_na <- which(!is.na(year_values))[sum(!is.na(year_values))]
          left_values <- year_values[max(1, last_non_na - 2):last_non_na]
          left_median <- if (length(left_values) > 0) median(left_values, na.rm = TRUE) else stop("no left median in boundary gap")
          year_values[(length(year_values) - right_boundary + right_to_fill + 1):length(year_values)] <- left_median
          #print("gap remained in right boundary after linear fill: median filled")
          }
      }
    }
    ########## DEALING WITH LINEAR TREND ENDS HERE
  } else {
    # No linear trend, use median
    #print(paste0("correlation doesn't exceed threshold for ", genus_species))
    year_values[is.na(year_values)] <- median(existing_values)
  }
  
  # Ensure no values are below zero
  year_values[year_values < 0] <- 0 ##indeed, all abundance values are positive in the original dataframe
  #update year_cols with new values
  for (col in names(year_values)) {
    year_cols[[col]] <- year_values[col]
  }
  
  #cbind year_cols back to genus_species
  row <- cbind(row[c("STUDY_ID", "GENUS_SPECIES")], year_cols, 
               row[c("LATITUDE", "LONGITUDE", "ABUNDANCE_TYPE", 
                     "GRAIN_SIZE_TEXT", "GRAIN_SQ_KM", "AREA_SQ_KM", "PLOT", 
                     "DEPTH")])
  
  ###commented out because used for demonstrative purposes
  # ##save the plot
  # # Create a data frame for plotting
  # plot_data <- data.frame(
  #   year = years,
  #   value = as.numeric(year_values),
  #   type = ifelse(as.vector(t(existing_data)), "Existing", "Filled")
  # )
  # # Create the plot
  # p <- ggplot(plot_data, aes(x = year, y = value, color = type)) +
  #   geom_point() +
  #   geom_line() +
  #   scale_color_manual(values = c("Existing" = "blue", "Filled" = "red")) +
  #   labs(title = paste("Time Series for", genus_species),
  #        x = "Year",
  #        y = "Value",
  #        color = "Data Type") +
  #   theme_minimal()
  # # Save the plot
  # ggsave(filename = paste0("output/p_disagg_plots/", group, "_row", i, "_", gsub(" ", "_", genus_species), ".png"), 
  #        plot = p, 
  #        width = 10, 
  #        height = 6, 
  #        units = "in")
  
  return(row)
}

##function that produces raw yearly totals for each group # PLOTS 1 OR 2
raw_total <- function(bio_raw) {
  bio_yearly_total <- bio_raw %>%
    select(-GENUS_SPECIES) %>%  # Remove the GENUS_SPECIES column
    summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%  # Sum across all columns, ignoring NAs
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Total") %>%  # Convert to long format
    mutate(Year = as.numeric(gsub("y_", "", Year))) %>%  # Convert Year to numeric
    arrange(Year)  # Sort by Year
  return(bio_yearly_total)
}

##function that produces and saves raw total plots # PLOTS 1 OR 2
plot_raw_total <- function(group_name, bio_total) {
  ggplot(bio_total, aes(x = Year, y = Total)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = paste0(group_name, " Population Over Time"),
         x = "Year",
         y = "Raw Observed Population") +
    scale_x_continuous(breaks = seq(min(bio_total$Year), max(bio_total$Year), by = 5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # If you want to save the plot
  ggsave(paste0("./output/group_raw_observed_totals/", group_name, "_raw_population_trend.png"), width = 10, height = 6, dpi = 300)
}

##function that produces and saves filled total plots #PLOTS 1 OR 2
plot_filled_total <- function(group_name, bio_ftotal) {
  ggplot(bio_ftotal, aes(x = Year, y = Total)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = paste0(group_name, " Population Over Time"),
         x = "Year",
         y = "Observed Population with Missing Data Filled In") +
    scale_x_continuous(breaks = seq(min(bio_ftotal$Year), max(bio_ftotal$Year), by = 5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # If you want to save the plot
  ggsave(paste0("./output/group_filled_observed_totals/", group_name, "_filled_population_trend.png"), width = 10, height = 6, dpi = 300)
}

##function that takes two pfilled and makes columns compatible # PLOTS 1 OR 2
format <- function(filled1, filled2) {
  year_cols1 <- grep("^y_", colnames(filled1), value = TRUE)
  year_cols2 <- grep("^y_", colnames(filled2), value = TRUE)
  years1 <- as.numeric(gsub("y_", "", year_cols1))
  years2 <- as.numeric(gsub("y_", "", year_cols2))
  min_year1 <-  min(years1, na.rm = TRUE)
  min_year2 <-  min(years2, na.rm = TRUE)
  min_year <- min(min_year1, min_year2)
  max_year1 <- max(years1, na.rm = TRUE)
  max_year2 <- max(years2, na.rm = TRUE)
  max_year <- max(max_year1, max_year2)
  all_years <- seq(min_year, max_year)
  missing_years1 <- setdiff(all_years, years1)
  missing_years2 <- setdiff(all_years, years2)
  if (length(missing_years1) > 0) {
    filled1[paste0("y_", missing_years1)] <- NA
    #and reorder
    year_cols1 <- grep("^y_", colnames(filled1), value = TRUE)
    new_order1 <- c("STUDY_ID", "GENUS_SPECIES", sort(year_cols1), 
                   "LATITUDE", "LONGITUDE", "ABUNDANCE_TYPE", "GRAIN_SIZE_TEXT",
                   "GRAIN_SQ_KM", "AREA_SQ_KM", "PLOT", "DEPTH")
    filled1 <- filled1[, new_order1]
  }
  if (length(missing_years2) > 0) {
    filled2[paste0("y_", missing_years2)] <- NA
    #and reorder
    year_cols2 <- grep("^y_", colnames(filled2), value = TRUE)
    new_order2 <- c("STUDY_ID", "GENUS_SPECIES", sort(year_cols2), 
                   "LATITUDE", "LONGITUDE", "ABUNDANCE_TYPE", "GRAIN_SIZE_TEXT",
                   "GRAIN_SQ_KM", "AREA_SQ_KM", "PLOT", "DEPTH")
    filled2 <- filled2[, new_order2]
  }
  return(filled2) #CUSTOMISE FOR EVERYTHING PUT THROUGH
}

################## PROP CHANGE HELPER FUNCTIONS
#function calculates proportional change 
calc_pd <- function(bio_filled) {
  year_cols <- grep("^y_", colnames(bio_filled), value = TRUE)
  min_year <- as.numeric(sub("^y_", "", year_cols[1]))
  bio_pd <- bio_filled
  # First, create all d_y_ columns
  for (col in year_cols) {
    curr_year <- as.numeric(sub("^y_", "", col))
    d_col <- paste0("d_", col)
    bio_pd <- bio_pd %>% 
      mutate(!!d_col := {
        rel_years <- max(min_year, curr_year - 3):max(min_year, curr_year - 1) # establishes the years over which the "preceding median" is taken
        rel_cols <- paste0("y_", rel_years)
        med <- apply(select(., all_of(rel_cols)), 1, median)
        case_when(
          .[[col]] == 0 & med == 0 ~ 1,  # 0/0 case #if both this column and the median of the preceding three are zero, let the prop_change be 1
          med == 0 ~ Inf,  # non-zero/0 case # if this column is not zero but the preceding median is, eventually make d_y the maximum finite value in each row
          TRUE ~ .[[col]] / med  # normal case #otherwise, normal prop_change formula
        )
      })
  }
  # Then, replace Inf values with the maximum finite value in each row
  bio_pd <- bio_pd %>%
    rowwise() %>%
    mutate(across(starts_with("d_y_"), ~ ifelse(is.infinite(.), max(c_across(starts_with("d_y_"))[is.finite(c_across(starts_with("d_y_")))], na.rm = TRUE), .))) %>%
    ungroup()
  return(bio_pd)
}

#function calculates proportional change using local median as opposed to current year value
calc_pd_lm <- function(bio_filled) {
  year_cols <- grep("^y_", colnames(bio_filled), value = TRUE)
  min_year <- as.numeric(sub("^y_", "", year_cols[1]))
  max_year <- as.numeric(sub("^y_", "", year_cols[length(year_cols)]))
  bio_pd <- bio_filled
  # First, create all d_y_ columns
  for (col in year_cols) {
    curr_year <- as.numeric(sub("^y_", "", col))
    d_col <- paste0("d_", col)
    bio_pd <- bio_pd %>% 
      mutate(!!d_col := {
        rel_years <- max(min_year, curr_year - 3):max(min_year, curr_year - 1)
        rel_cols <- paste0("y_", rel_years)
        lag_med <- apply(select(., all_of(rel_cols)), 1, median)
        curr_years <- max(min_year, curr_year - 1):min(max_year, curr_year + 1)
        curr_cols <- paste0("y_", curr_years)
        curr_med <- apply(select(., all_of(curr_cols)), 1, median)
        case_when(
          curr_med == 0 & lag_med == 0 ~ 1,  # 0/0 case
          lag_med == 0 ~ Inf,  # non-zero/0 case
          TRUE ~ curr_med / lag_med  # normal case
        )
      })
  }
  # Then, replace Inf values with the maximum finite value in each row
  bio_pd <- bio_pd %>%
    rowwise() %>%
    mutate(across(starts_with("d_y_"), ~ ifelse(is.infinite(.), max(c_across(starts_with("d_y_"))[is.finite(c_across(starts_with("d_y_")))], na.rm = TRUE), .))) %>%
    ungroup()
  return(bio_pd)
}

# Helper function to calculate geometric mean
geometric_mean <- function(x, na.rm = TRUE) {
  exp(mean(log(x[x > 0]), na.rm = na.rm))
}

#this function aggregates proportional change values at the species level,
prop_agg_species <- function(bio_pd) {
  # Identify the d_y_[year] columns
  year_cols <- grep("^d_y_", names(bio_pd), value = TRUE)
  # Group by GENUS_SPECIES and summarize across d_y_[year] columns using geometric mean
  bio_pd %>%
    group_by(GENUS_SPECIES) %>%
    summarise(across(all_of(year_cols), \(x) geometric_mean(x, na.rm = TRUE))) %>%
    ungroup()
}

##prepare for plot
##function pivots aggregated prop change data to plot
prop_species_to_plot <- function(bio_pda) {
  year_cols <- grep("^d_y_", names(bio_pda), value = TRUE)
  bio_pda_long <- bio_pda %>%
    select(all_of(year_cols)) %>%  # keep nothing but the year columns
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Value") %>%  # Convert to long format
    mutate(Year = as.numeric(gsub("^d_y_", "", Year))) %>%  # Convert Year to numeric
    group_by(Year) %>%  # Group by Year
    summarise(Total = geometric_mean(Value, na.rm = TRUE)) %>%  # Use geometric mean for each year
    arrange(Year)  # Sort by Year
  return(bio_pda_long)
}

##function that produces and saves filled pd plots
plot_prop_species <- function(group_name, bio_pda_long) {
  ggplot(bio_pda_long, aes(x = Year, y = Total)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = paste0(group_name, " Population Change Over Time"),
         x = "Year",
         y = "Population Change with Missing Data Filled In") +
    scale_x_continuous(breaks = seq(min(bio_pda_long$Year), max(bio_pda_long$Year), by = 5)) +
    scale_y_continuous(limits = c(0, 2)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # If you want to save the plot
  ggsave(paste0("./output/group_filled_observed_pchange/", group_name, "_filled_population_trend.png"), width = 10, height = 6, dpi = 300)
}

##function that produces and saves filled pd plots of local median's relative change
plot_prop_species_lm <- function(group_name, bio_pda_long) {
  ggplot(bio_pda_long, aes(x = Year, y = Total)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = paste0(group_name, " Population Change Over Time"),
         x = "Year",
         y = "Population Change Of Local Median with Missing Data Filled In") +
    scale_x_continuous(breaks = seq(min(bio_pda_long$Year), max(bio_pda_long$Year), by = 5)) +
    scale_y_continuous(limits = c(0, 2)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # If you want to save the plot
  ggsave(paste0("./output/group_filled_observed_pchange_lm/", group_name, "_filled_population_trend.png"), width = 10, height = 6, dpi = 300)
}


##make plots cumulative
# Function to transform the time series
accumulate <- function(pda_long) {
  # Find the midpoint Year
  midpoint_index <- ceiling(nrow(pda_long) / 2)
  midpoint_Year <- pda_long$Year[midpoint_index]
  
  # Initialize the new_Total column with 1 at the midpoint
  pda_long$new_Total <- 1
  
  # Calculate cumulative changes to the right of the midpoint
  for (i in (midpoint_index + 1):nrow(pda_long)) {
    pda_long$new_Total[i] <- pda_long$new_Total[i-1] * pda_long$Total[i]
  }
  
  # Calculate cumulative changes to the left of the midpoint
  for (i in (midpoint_index - 1):1) {
    pda_long$new_Total[i] <- pda_long$new_Total[i+1] / pda_long$Total[i+1]
  }
  
  return(pda_long)
}

##function that produces and saves filled pd plots accumulated
plot_prop_species_cu <- function(group_name, bio_pda_long) {
  ggplot(bio_pda_long, aes(x = Year, y = new_Total)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = paste0(group_name, " Normalised Population Trend Over Time"),
         x = "Year",
         y = "Normalised Population Change with Missing Data Filled In") +
    scale_x_continuous(breaks = seq(min(bio_pda_long$Year), max(bio_pda_long$Year), by = 5)) +
    scale_y_continuous(limits = c(0, 2)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # If you want to save the plot
  ggsave(paste0("./output/group_filled_pchange_cu/", group_name, "_filled_population_trend.png"), width = 10, height = 6, dpi = 300)
}

