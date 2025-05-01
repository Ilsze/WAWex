# This script creates a "calc_ts" dataframe for wild animals. 
#Variables: Group, Category, Year, aliveatanytime, WR_potential, NC_potential, 
#           Welfare level, forebrain neurons

library(pacman)
p_load(tidyverse, dplyr, ggplot2, gridExtra, mgcv, nleqslv, png, readr, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)

#' Generate wild animal welfare dataset
#' 
#' @param output_file Optional path to save the generated dataset
#' @param human_forebrain_neurons Number of neurons in human forebrains (for NC_potential calculation)
#' @return Dataframe with wild animal welfare data
generate_wild_animal_dataset <- function(output_file = NULL, 
                                         human_forebrain_neurons = 24526000000) {
  
  # Define the wild animal categories and their parameters
  wild_categories <- data.frame(
    Category = c("Wild birds", "Wild terrestrial mammals", "Wild fish", "Wild terrestrial arthropods"),
    WR_potential = c(0.327, 0.512, 0.087, 0.001),
    Welfare_level = c(-2, -28, -31, -42),
    forebrain_neurons = c(611000000, 999478260.9, 5062418.43292645, 10000)
  )
  
  # Calculate NC_potential based on forebrain neurons relative to humans
  wild_categories <- wild_categories %>%
    mutate(NC_potential = forebrain_neurons / human_forebrain_neurons)
  
  # Define year ranges for each category
  # Wild birds: 1970-2017
  # Others: 1950-2025
  wild_birds_years <- 1970:2017
  other_wild_years <- 1950:2025
  
  # Create dataframes for each category with their respective years
  wild_birds_df <- expand.grid(
    Year = wild_birds_years,
    Category = "Wild birds",
    stringsAsFactors = FALSE
  )
  
  wild_mammals_df <- expand.grid(
    Year = other_wild_years,
    Category = "Wild terrestrial mammals",
    stringsAsFactors = FALSE
  )
  
  wild_fish_df <- expand.grid(
    Year = other_wild_years,
    Category = "Wild fish",
    stringsAsFactors = FALSE
  )
  
  wild_arthropods_df <- expand.grid(
    Year = other_wild_years,
    Category = "Wild terrestrial arthropods",
    stringsAsFactors = FALSE
  )
  
  # Combine all dataframes
  wild_animals_df <- bind_rows(
    wild_birds_df,
    wild_mammals_df,
    wild_fish_df,
    wild_arthropods_df
  )
  
  # Add the Group column
  wild_animals_df$Group <- "Wild Animals"
  
  # Join with the categories dataframe to add the parameter values
  wild_animals_df <- wild_animals_df %>%
    left_join(wild_categories, by = "Category")
  
  # TODO: Add population estimates (aliveatanytime) based on various sources
  # For now, we'll create placeholder values that can be updated later
  # These are completely made-up placeholder values!
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Create population estimates for each category
  # Wild birds: Declining trend from ~400 billion in 1970 to ~300 billion in 2017
  wild_animals_df <- wild_animals_df %>%
    mutate(aliveatanytime = case_when(
      Category == "Wild birds" ~ 4e11 - (Year - 1970) * 2.5e9,
      Category == "Wild terrestrial mammals" ~ 1e11 + (Year - 1950) * 1e8,
      Category == "Wild fish" ~ 2e13 - (Year - 1950) * 1e10,
      Category == "Wild terrestrial arthropods" ~ 1e19 + (Year - 1950) * 1e16
    ))
  
  # Calculate utility metrics
  wild_animals_df <- wild_animals_df %>%
    mutate(
      #WR_pop = aliveatanytime * WR_potential,
      WR_utility = aliveatanytime * WR_potential * Welfare_level,
      #NC_pop = aliveatanytime * NC_potential,
      NC_utility = aliveatanytime * NC_potential * Welfare_level
    )
  
  # Save to file if specified
  if(!is.null(output_file)) {
    write.xlsx(wild_animals_df, output_file, rowNames = FALSE)
    cat("Wild animal dataset saved to", output_file, "\n")
  }
  
  return(wild_animals_df)
}

#' Integrate wild animal data with existing calc_tseries
#' 
#' @param wild_data Wild animal dataset from generate_wild_animal_dataset()
#' @param calc_tseries Existing welfare analysis dataset
#' @param output_file Optional path to save the integrated dataset
#' @return Integrated dataframe with wild and existing animal data
integrate_wild_animal_data <- function(wild_data, calc_tseries, output_file = NULL) {
  
  # Check if required columns exist in both datasets
  required_cols <- c("Year", "Category", "Group", "aliveatanytime", 
                     "WR_potential", "Welfare_level", "forebrain_neurons")
  
  if(!all(required_cols %in% names(wild_data)) || !all(required_cols %in% names(calc_tseries))) {
    missing_wild <- setdiff(required_cols, names(wild_data))
    missing_calc <- setdiff(required_cols, names(calc_tseries))
    
    error_msg <- ""
    if(length(missing_wild) > 0) {
      error_msg <- paste0(error_msg, "Missing columns in wild_data: ", 
                          paste(missing_wild, collapse = ", "), ". ")
    }
    if(length(missing_calc) > 0) {
      error_msg <- paste0(error_msg, "Missing columns in calc_tseries: ", 
                          paste(missing_calc, collapse = ", "), ".")
    }
    
    stop(error_msg)
  }
  
  # Combine the datasets
  integrated_data <- bind_rows(calc_tseries, wild_data)
  
  # Ensure both WR and NC columns exist
  integrated_data <- ensure_wr_columns(integrated_data)
  integrated_data <- ensure_nc_columns(integrated_data)
  
  # Save to file if specified
  if(!is.null(output_file)) {
    write.xlsx(integrated_data, output_file, rowNames = FALSE)
    cat("Integrated dataset saved to", output_file, "\n")
  }
  
  return(integrated_data)
}

#' Ensure the dataset has all required WR columns
#' 
#' @param data The dataset to check and modify
#' @return Updated dataset with all WR columns
ensure_wr_columns <- function(data) {
  # Check if WR columns already exist
  if(!("WR_utility" %in% colnames(data)) 
     ##|| !("WR_pop" %in% colnames(data))
     ) {
    
    # If not, calculate them based on available data
    if("WR_potential" %in% colnames(data) && 
       "aliveatanytime" %in% colnames(data) && 
       "Welfare_level" %in% colnames(data)) {
      
      # Calculate WR columns
      data <- data %>%
        mutate(
          #WR_pop = aliveatanytime * WR_potential,
          WR_utility = aliveatanytime * WR_potential * Welfare_level
        )
    } else {
      stop("Required columns for WR calculations are missing.")
    }
  }
  
  return(data)
}

#' Ensure the dataset has all required NC columns
#' 
#' @param data The dataset to check and modify
#' @return Updated dataset with all NC columns
ensure_nc_columns <- function(data) {
  # Check if NC columns already exist
  if(!("NC_utility" %in% colnames(data)) 
     ##|| !("NC_pop" %in% colnames(data))
     ) {
    
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
      
      # If no human data is present, use the default value
      if(length(human_fneurons) == 0) {
        human_fneurons <- 24526000000
      }
      
      # Calculate NC columns
      data <- data %>%
        mutate(
          NC_potential = forebrain_neurons / human_fneurons,
          #NC_pop = aliveatanytime * NC_potential,
          NC_utility = aliveatanytime * NC_potential * Welfare_level
        )
    } else {
      stop("Required columns for NC calculations are missing.")
    }
  }
  
  return(data)
}

# Example usage:
# Generate wild animal dataset
wild_animals <- generate_wild_animal_dataset(
  output_file = "second_pass/wild_animals_calc_tseries.xlsx"
)
# 
# # Integrate with existing calc_tseries
# # First load existing data
# calc_tseries <- read_excel("first_pass/calc_tseries.xlsx")
# 
# # Then integrate
# integrated_data <- integrate_wild_animal_data(
#   wild_data = wild_animals,
#   calc_tseries = calc_tseries,
#   output_file = "first_pass/integrated_calc_tseries_with_wild.xlsx"
# )