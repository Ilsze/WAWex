# Modular Welfare Analysis Framework
# This script provides a flexible framework for analyzing welfare data
# with different quantification methods for both welfare levels and potentials

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm, 
       patchwork, hrbrthemes, scales)

#' Universal plot saving function that handles all output formats automatically
#' 
#' @param plot_object The ggplot object
#' @param filename Base filename (without extension)
#' @param output_dir Output directory
#' @param presentation_config Global presentation configuration
#' @param pdf_width Width for PDF
#' @param pdf_height Height for PDF
#' @return NULL (saves files)
universal_ggsave <- function(plot_object, filename, output_dir, 
                             presentation_config = NULL,
                             pdf_width = 10, pdf_height = 6) {
  
  # Always save the PDF (original behavior)
  pdf_path <- file.path(output_dir, paste0(filename, ".pdf"))
  ggsave(pdf_path, plot = plot_object, width = pdf_width, height = pdf_height)
  
  # Check if presentation images are enabled
  if(!is.null(presentation_config) && 
     isTRUE(presentation_config$create_presentation_images)) {
    
    # Create presentation directory
    presentation_dir <- file.path(output_dir, "presentation_images")
    if(!dir.exists(presentation_dir)) {
      dir.create(presentation_dir, recursive = TRUE)
    }
    
    # Check if this specific file should get special treatment
    pdf_filename <- paste0(filename, ".pdf")
    
    # Apply filters to determine if this plot should be saved as presentation image
    should_save <- FALSE
    prefix <- ""
    
    # Method 1: Specific filename mapping (your current need)
    if(!is.null(presentation_config$specific_files) && 
       pdf_filename %in% names(presentation_config$specific_files)) {
      should_save <- TRUE
      prefix <- paste0(presentation_config$specific_files[[pdf_filename]], "_")
    }
    
    # Method 2: Pattern matching (for future flexibility)
    if(!is.null(presentation_config$filename_patterns)) {
      for(pattern in presentation_config$filename_patterns) {
        if(grepl(pattern$regex, filename)) {
          should_save <- TRUE
          if(!is.null(pattern$prefix)) {
            prefix <- paste0(pattern$prefix, "_")
          }
          break
        }
      }
    }
    
    # Method 3: Save all (if no specific filters defined)
    if(is.null(presentation_config$specific_files) && 
       is.null(presentation_config$filename_patterns) &&
       isTRUE(presentation_config$save_all)) {
      should_save <- TRUE
    }
    
    # Save presentation image if criteria met
    if(should_save) {
      image_ext <- presentation_config$image_format %||% "png"
      output_filename <- paste0(prefix, filename, ".", image_ext)
      output_path <- file.path(presentation_dir, output_filename)
      
      ggsave(
        filename = output_path,
        plot = plot_object,
        width = presentation_config$image_width %||% 12,
        height = presentation_config$image_height %||% 8,
        dpi = presentation_config$image_dpi %||% 300,
        device = image_ext
      )
      
      cat("Presentation image saved:", output_filename, "\n")
    }
  }
}

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
  
  # 8. Calculate extended correlations and elasticities using extended data
  cat("Calculating extended correlations and elasticities using extended data...\n")
  cor_and_elasticity_extended <- calculate_correlations_elasticities_extended()
  
  # 9. Save results
  cat("Saving results...\n")
  method_suffix <- paste0("_", tolower(welfare_level_method))
  
  write.xlsx(net_series, file.path(output_dir, paste0("net_series", method_suffix, ".xlsx")))
  write.xlsx(cor_and_elasticity, file.path(output_dir, paste0("cor_and_elas", method_suffix, ".xlsx")))
  write.xlsx(cor_and_elasticity_extended, file.path(output_dir, paste0("cor_and_elas_extended", method_suffix, ".xlsx")))
  write.xlsx(f_change, file.path(output_dir, paste0("f_change", method_suffix, ".xlsx")))
  
  # 10. Return results
  cat("Analysis complete!\n")
  return(list(
    calc_tseries = calc_tseries,
    net_series = net_series,
    cor_and_elasticity = cor_and_elasticity,
    cor_and_elasticity_extended = cor_and_elasticity_extended,
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
     !("NC_tot" %in% colnames(data)) ||
     !("NC_apot" %in% colnames(data)) || 
     !("NC_pot_conc" %in% colnames(data)) ||
     !("NC_pot_conv" %in% colnames(data))
     )    {
    
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
          NC_pot_conc = sqrt(NC_potential), 
          NC_pot_conv = NC_potential^2,
          NC_apot = aliveatanytime * NC_potential,
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
  if(!("WR_utility" %in% colnames(data)) ||
     !("WR_apot" %in% colnames(data))) {
    
    # If not, calculate them based on available data
    if("WR_potential" %in% colnames(data) && 
       "aliveatanytime" %in% colnames(data) && 
       "Welfare_level" %in% colnames(data)) {
      
      # Calculate WR columns
      data <- data %>%
        mutate(
          WR_apot = aliveatanytime * WR_potential, 
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

#' Create cor_and_elas for disaggregated groups. 
#' In other words, calculate correlations and elasticities between human and animal populations/utilities
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
    cor_pop = numeric(), 
    e_pop = numeric(),
    WR_cor_u = numeric(), 
    WR_cor_hpop_au = numeric(), 
    WR_cor_hu_apop = numeric(),
    WR_e_u = numeric(), 
    WR_e_hpop_au = numeric(), 
    WR_e_hu_apop = numeric(),
    NC_cor_u = numeric(), 
    NC_cor_hpop_au = numeric(), 
    NC_cor_hu_apop = numeric(),
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
    
    # Calculate pop correlations and elasticities
    cor_pop <- if(has_valid_pop_data) {
      tryCatch(cor(human_pop_trunc, cat_pop_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
  
    e_pop <- if(has_valid_pop_data) {
      tryCatch(coef(lm(cat_pop_trunc ~ human_pop_trunc))[2], error = function(e) NA)
    } else NA
    
    # Calculate WR correlations
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
        cor_pop = cor_pop, 
        e_pop = e_pop, 
        WR_cor_u = wr_cor_u, 
        WR_cor_hpop_au = wr_cor_hpop_au, 
        WR_cor_hu_apop = wr_cor_hu_apop,
        WR_e_u = wr_e_u, 
        WR_e_hpop_au = wr_e_hpop_au, 
        WR_e_hu_apop = wr_e_hu_apop,
        NC_cor_u = nc_cor_u, 
        NC_cor_hpop_au = nc_cor_hpop_au, 
        NC_cor_hu_apop = nc_cor_hu_apop,
        NC_e_u = nc_e_u, 
        NC_e_hpop_au = nc_e_hpop_au, 
        NC_e_hu_apop = nc_e_hu_apop
      )
  }
  
  return(cor_and_elasticity)
}

#' Create cor_and_elas_extended, which is cor_and elas for aggregated groups. 
#' In other words, calculate correlations and elasticities between human and aggregated animal populations/utilities
#' for both WR and NC methods using extended data with four different animal groupings
#' 
#' @return Dataframe with correlations and elasticities for tot (all including humans), tot_non_human (all animals excluding humans), wild (w), and farmed (f) groupings
calculate_correlations_elasticities_extended <- function() {
  
  # Read the extended data
  extended_data <- read_excel("fifth_pass/dat/extended_integrated_calc_tseries.xlsx")
  
  # Define category groups
  excluded_categories <- c("Wild fish", "Wild terrestrial arthropods")
  all_wild_categories <- c("Wild birds", "Wild terrestrial mammals", "Wild fish", "Wild terrestrial arthropods")
  wild_categories_not_excluded <- c("Wild birds", "Wild terrestrial mammals")
  
  # Get all categories for tot (everything INCLUDING humans)
  all_categories_including_humans <- unique(extended_data$Category)
  
  # Get tot_non_human categories (everything except excluded wild categories and humans)
  tot_non_human_categories <- setdiff(unique(extended_data$Category), c("Humans", excluded_categories))
  
  # Get farmed categories (everything that's not wild and not human)
  farmed_categories <- setdiff(unique(extended_data$Category), c("Humans", all_wild_categories))
  
  # Initialize results dataframe with 4 rows and clean column names
  cor_and_elasticity_extended <- data.frame(
    Group = c("tot", "tot_non_human", "w", "f"),
    
    # Population metrics (2 columns)
    cor_pop = numeric(4), 
    e_pop = numeric(4),
    
    # WR utility metrics (6 columns)
    WR_cor_u = numeric(4), 
    WR_cor_hpop_au = numeric(4), 
    WR_cor_hu_apop = numeric(4),
    WR_e_u = numeric(4), 
    WR_e_hpop_au = numeric(4), 
    WR_e_hu_apop = numeric(4),
    
    # NC utility metrics (6 columns)
    NC_cor_u = numeric(4), 
    NC_cor_hpop_au = numeric(4), 
    NC_cor_hu_apop = numeric(4),
    NC_e_u = numeric(4), 
    NC_e_hpop_au = numeric(4), 
    NC_e_hu_apop = numeric(4),
    
    stringsAsFactors = FALSE
  )
  
  # Get human vectors
  human_vec <- extended_data %>% 
    filter(Category == "Humans") %>% 
    select(Year, aliveatanytime, WR_utility, NC_utility)
  
  # Define the four groupings
  groupings <- list(
    tot = all_categories_including_humans,  # NEW: includes humans
    tot_non_human = tot_non_human_categories,  # RENAMED: was "tot_excl"
    w = all_wild_categories,  # CHANGED: now includes all wild (fish + arthropods)
    f = farmed_categories
  )
  
  # Calculate correlations and elasticities for each grouping
  for (i in 1:4) {
    group_name <- c("tot", "tot_non_human", "w", "f")[i]
    categories_in_group <- groupings[[group_name]]
    
    # Aggregate data for this grouping by year
    if(group_name == "tot") {
      # For tot group, aggregate ALL categories (including humans)
      animal_agg <- extended_data %>%
        filter(Category %in% categories_in_group) %>%
        group_by(Year) %>%
        summarize(
          agg_population = sum(aliveatanytime, na.rm = TRUE),
          agg_WR_utility = sum(WR_utility, na.rm = TRUE),
          agg_NC_utility = sum(NC_utility, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      # For other groups, aggregate only non-human categories
      animal_agg <- extended_data %>%
        filter(Category %in% categories_in_group) %>%
        group_by(Year) %>%
        summarize(
          agg_population = sum(aliveatanytime, na.rm = TRUE),
          agg_WR_utility = sum(WR_utility, na.rm = TRUE),
          agg_NC_utility = sum(NC_utility, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    # Find overlapping years between humans and aggregated data
    min_year <- max(min(animal_agg$Year), min(human_vec$Year), na.rm = TRUE)
    max_year <- min(max(animal_agg$Year), max(human_vec$Year), na.rm = TRUE)
    
    # Get human vectors for overlapping period
    human_pop_trunc <- human_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(aliveatanytime)
    
    human_wr_u_trunc <- human_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(WR_utility)
    
    human_nc_u_trunc <- human_vec %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(NC_utility)
    
    # Get aggregated vectors for overlapping period
    animal_pop_trunc <- animal_agg %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(agg_population)
    
    animal_wr_u_trunc <- animal_agg %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(agg_WR_utility)
    
    animal_nc_u_trunc <- animal_agg %>%
      filter(Year >= min_year & Year <= max_year) %>%
      pull(agg_NC_utility)
    
    # Check data validity
    has_valid_pop_data <- !all(is.na(human_pop_trunc)) && !all(is.na(animal_pop_trunc)) && 
      sum(!is.na(human_pop_trunc) & !is.na(animal_pop_trunc)) > 1
    
    has_valid_wr_utility_data <- !all(is.na(human_wr_u_trunc)) && !all(is.na(animal_wr_u_trunc)) && 
      sum(!is.na(human_wr_u_trunc) & !is.na(animal_wr_u_trunc)) > 1
    
    has_valid_nc_utility_data <- !all(is.na(human_nc_u_trunc)) && !all(is.na(animal_nc_u_trunc)) && 
      sum(!is.na(human_nc_u_trunc) & !is.na(animal_nc_u_trunc)) > 1
    
    # Calculate all metrics for this grouping
    
    # Population correlations and elasticities
    cor_pop <- if(has_valid_pop_data) {
      tryCatch(cor(human_pop_trunc, animal_pop_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    e_pop <- if(has_valid_pop_data) {
      tryCatch(coef(lm(animal_pop_trunc ~ human_pop_trunc))[2], error = function(e) NA)
    } else NA
    
    # WR utility correlations
    wr_cor_u <- if(has_valid_wr_utility_data) {
      tryCatch(cor(human_wr_u_trunc, animal_wr_u_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    wr_cor_hpop_au <- if(!all(is.na(human_pop_trunc)) && !all(is.na(animal_wr_u_trunc)) && 
                         sum(!is.na(human_pop_trunc) & !is.na(animal_wr_u_trunc)) > 1) {
      tryCatch(cor(human_pop_trunc, animal_wr_u_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    wr_cor_hu_apop <- if(!all(is.na(human_wr_u_trunc)) && !all(is.na(animal_pop_trunc)) && 
                         sum(!is.na(human_wr_u_trunc) & !is.na(animal_pop_trunc)) > 1) {
      tryCatch(cor(human_wr_u_trunc, animal_pop_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    # WR utility elasticities
    wr_e_u <- if(has_valid_wr_utility_data) {
      tryCatch(coef(lm(animal_wr_u_trunc ~ human_wr_u_trunc))[2], error = function(e) NA)
    } else NA
    
    wr_e_hpop_au <- if(!all(is.na(human_pop_trunc)) && !all(is.na(animal_wr_u_trunc)) && 
                       sum(!is.na(human_pop_trunc) & !is.na(animal_wr_u_trunc)) > 1) {
      tryCatch(coef(lm(animal_wr_u_trunc ~ human_pop_trunc))[2], error = function(e) NA)
    } else NA
    
    wr_e_hu_apop <- if(!all(is.na(human_wr_u_trunc)) && !all(is.na(animal_pop_trunc)) && 
                       sum(!is.na(human_wr_u_trunc) & !is.na(animal_pop_trunc)) > 1) {
      tryCatch(coef(lm(animal_pop_trunc ~ human_wr_u_trunc))[2], error = function(e) NA)
    } else NA
    
    # NC utility correlations
    nc_cor_u <- if(has_valid_nc_utility_data) {
      tryCatch(cor(human_nc_u_trunc, animal_nc_u_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    nc_cor_hpop_au <- if(!all(is.na(human_pop_trunc)) && !all(is.na(animal_nc_u_trunc)) && 
                         sum(!is.na(human_pop_trunc) & !is.na(animal_nc_u_trunc)) > 1) {
      tryCatch(cor(human_pop_trunc, animal_nc_u_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    nc_cor_hu_apop <- if(!all(is.na(human_nc_u_trunc)) && !all(is.na(animal_pop_trunc)) && 
                         sum(!is.na(human_nc_u_trunc) & !is.na(animal_pop_trunc)) > 1) {
      tryCatch(cor(human_nc_u_trunc, animal_pop_trunc, use = "complete.obs"), error = function(e) NA)
    } else NA
    
    # NC utility elasticities
    nc_e_u <- if(has_valid_nc_utility_data) {
      tryCatch(coef(lm(animal_nc_u_trunc ~ human_nc_u_trunc))[2], error = function(e) NA)
    } else NA
    
    nc_e_hpop_au <- if(!all(is.na(human_nc_u_trunc)) && !all(is.na(animal_nc_u_trunc)) && 
                       sum(!is.na(human_nc_u_trunc) & !is.na(animal_nc_u_trunc)) > 1) {
      tryCatch(coef(lm(animal_nc_u_trunc ~ human_pop_trunc))[2], error = function(e) NA)
    } else NA
    
    nc_e_hu_apop <- if(!all(is.na(human_nc_u_trunc)) && !all(is.na(animal_pop_trunc)) && 
                       sum(!is.na(human_nc_u_trunc) & !is.na(animal_pop_trunc)) > 1) {
      tryCatch(coef(lm(animal_pop_trunc ~ human_nc_u_trunc))[2], error = function(e) NA)
    } else NA
    
    # Assign results to the appropriate row
    cor_and_elasticity_extended[i, "cor_pop"] <- cor_pop
    cor_and_elasticity_extended[i, "e_pop"] <- e_pop
    cor_and_elasticity_extended[i, "WR_cor_u"] <- wr_cor_u
    cor_and_elasticity_extended[i, "WR_cor_hpop_au"] <- wr_cor_hpop_au
    cor_and_elasticity_extended[i, "WR_cor_hu_apop"] <- wr_cor_hu_apop
    cor_and_elasticity_extended[i, "WR_e_u"] <- wr_e_u
    cor_and_elasticity_extended[i, "WR_e_hpop_au"] <- wr_e_hpop_au
    cor_and_elasticity_extended[i, "WR_e_hu_apop"] <- wr_e_hu_apop
    cor_and_elasticity_extended[i, "NC_cor_u"] <- nc_cor_u
    cor_and_elasticity_extended[i, "NC_cor_hpop_au"] <- nc_cor_hpop_au
    cor_and_elasticity_extended[i, "NC_cor_hu_apop"] <- nc_cor_hu_apop
    cor_and_elasticity_extended[i, "NC_e_u"] <- nc_e_u
    cor_and_elasticity_extended[i, "NC_e_hpop_au"] <- nc_e_hpop_au
    cor_and_elasticity_extended[i, "NC_e_hu_apop"] <- nc_e_hu_apop
  }
  
  return(cor_and_elasticity_extended)
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





#' Create NC apot (animal potential) trend plots with progressive category exclusions
#' 
#' @param data The processed dataset
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
create_nc_apot_plots <- function(data, output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Filter out rows with NA values for NC apot
  filtered_data_nc_apot <- data %>%
    filter(!is.na(NC_apot), !is.na(aliveatanytime))
  
  # NC apot over time - all categories
  p_nc_apot <- ggplot(filtered_data_nc_apot, aes(x = Year, y = NC_apot, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_data_nc_apot %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_data_nc_apot$Year), 
                                  max(filtered_data_nc_apot$Year) + 30)) +
    labs(title = "NC Potential * Population Over Time (1950-2025)", 
         y = "NC Potential * Population", 
         x = "Year") +
    theme_minimal() +   
    theme(plot.title = element_text(size = 22, face = "bold"),     
          axis.title.x = element_text(size = 24),     
          axis.title.y = element_text(size = 24),      
          axis.text.x = element_text(size = 10),     
          axis.text.y = element_text(size = 10)) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_apot_trends.pdf"), 
         plot = p_nc_apot, width = 10, height = 6)
  
  # No wild terrestrial arthropods
  filtered_nc_apot_n_wta <- filtered_data_nc_apot %>% 
    filter(Category != "Wild terrestrial arthropods")
  
  p_nc_apot_n_wta <- ggplot(filtered_nc_apot_n_wta, aes(x = Year, y = NC_apot, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_apot_n_wta %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_apot_n_wta$Year), 
                                  max(filtered_nc_apot_n_wta$Year) + 30)) +
    labs(title = "NC Potential * Population Over Time (No wt. arthropods)", 
         y = "NC Potential * Population", 
         x = "Year") +
    theme_minimal() +   
    theme(plot.title = element_text(size = 22, face = "bold"),     
          axis.title.x = element_text(size = 24),     
          axis.title.y = element_text(size = 24),      
          axis.text.x = element_text(size = 10),     
          axis.text.y = element_text(size = 10)) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_apot_trends_n_wta.pdf"), 
         plot = p_nc_apot_n_wta, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish
  filtered_nc_apot_n_wta_wfi <- filtered_nc_apot_n_wta %>% 
    filter(Category != "Wild fish")
  
  p_nc_apot_n_wta_wfi <- ggplot(filtered_nc_apot_n_wta_wfi, aes(x = Year, y = NC_apot, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_apot_n_wta_wfi %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_apot_n_wta_wfi$Year), 
                                  max(filtered_nc_apot_n_wta_wfi$Year) + 30)) +
    labs(title = "NC Potential * Population Over Time (No wt. arthropods, No w. fish)", 
         y = "NC Potential * Population", 
         x = "Year") +
    theme_minimal() +   
    theme(plot.title = element_text(size = 22, face = "bold"),     
          axis.title.x = element_text(size = 24),     
          axis.title.y = element_text(size = 24),      
          axis.text.x = element_text(size = 10),     
          axis.text.y = element_text(size = 10)) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_apot_trends_n_wta_wfi.pdf"), 
         plot = p_nc_apot_n_wta_wfi, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no humans
  filtered_nc_apot_n_wta_wfi_hum <- filtered_nc_apot_n_wta_wfi %>% 
    filter(Category != "Humans")
  
  p_nc_apot_n_wta_wfi_hum <- ggplot(filtered_nc_apot_n_wta_wfi_hum, aes(x = Year, y = NC_apot, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_apot_n_wta_wfi_hum %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_apot_n_wta_wfi_hum$Year), 
                                  max(filtered_nc_apot_n_wta_wfi_hum$Year) + 30)) +
    labs(title = "NC Potential * Population Over Time (No wt. arthropods, No w. fish, No humans)", 
         y = "NC Potential * Population", 
         x = "Year") +
    theme_minimal() +   
    theme(plot.title = element_text(size = 22, face = "bold"),     
          axis.title.x = element_text(size = 24),     
          axis.title.y = element_text(size = 24),      
          axis.text.x = element_text(size = 10),     
          axis.text.y = element_text(size = 10)) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_apot_trends_n_wta_wfi_hum.pdf"), 
         plot = p_nc_apot_n_wta_wfi_hum, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no humans, no wild birds
  filtered_nc_apot_n_wta_wfi_hum_wbi <- filtered_nc_apot_n_wta_wfi_hum %>% 
    filter(Category != "Wild birds")
  
  p_nc_apot_n_wta_wfi_hum_wbi <- ggplot(filtered_nc_apot_n_wta_wfi_hum_wbi, aes(x = Year, y = NC_apot, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_apot_n_wta_wfi_hum_wbi %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_apot_n_wta_wfi_hum_wbi$Year), 
                                  max(filtered_nc_apot_n_wta_wfi_hum_wbi$Year) + 30)) +
    labs(title = "NC Potential * Population Over Time (No wt. arthropods, No w. fish, No humans, No w. birds)", 
         y = "NC Potential * Population", 
         x = "Year") +
    theme_minimal() +   
    theme(plot.title = element_text(size = 22, face = "bold"),     
          axis.title.x = element_text(size = 24),     
          axis.title.y = element_text(size = 24),      
          axis.text.x = element_text(size = 10),     
          axis.text.y = element_text(size = 10)) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_apot_trends_n_wta_wfi_hum_wbi.pdf"), 
         plot = p_nc_apot_n_wta_wfi_hum_wbi, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no humans, no wild birds, no wild terrestrial mammals
  filtered_nc_apot_n_wta_wfi_hum_wbi_wtm <- filtered_nc_apot_n_wta_wfi_hum_wbi %>% 
    filter(Category != "Wild terrestrial mammals")
  
  p_nc_apot_n_wta_wfi_hum_wbi_wtm <- ggplot(filtered_nc_apot_n_wta_wfi_hum_wbi_wtm, aes(x = Year, y = NC_apot, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_apot_n_wta_wfi_hum_wbi_wtm %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_apot_n_wta_wfi_hum_wbi_wtm$Year), 
                                  max(filtered_nc_apot_n_wta_wfi_hum_wbi_wtm$Year) + 30)) +
    labs(title = "NC Potential * Population Over Time (No wt. arthropods, No w. fish, No humans, No w. birds, no wt. mammals)", 
         y = "NC Potential * Population", 
         x = "Year") +
    theme_minimal() +   
    theme(plot.title = element_text(size = 22, face = "bold"),     
          axis.title.x = element_text(size = 24),     
          axis.title.y = element_text(size = 24),      
          axis.text.x = element_text(size = 10),     
          axis.text.y = element_text(size = 10)) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_apot_trends_n_wta_wfi_hum_wbi_wtm.pdf"), 
         plot = p_nc_apot_n_wta_wfi_hum_wbi_wtm, width = 10, height = 6)
  
  
  # No wild terrestrial arthropods, no wild fish, no humans, no wild terrestrial mammals, no farmed fish
  filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi <- filtered_nc_apot_n_wta_wfi_hum_wbi_wtm %>% 
    filter(Category != "Fish")
  
  p_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi <- ggplot(filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi, aes(x = Year, y = NC_apot, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi$Year), 
                                  max(filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi$Year) + 30)) +
    labs(title = "NC Potential * Population Over Time (No wt. arthropods, No w. fish, No humans, No w. bitds, No wt. mammals, No f. fish)", 
         y = "NC Potential * Population", 
         x = "Year") +
    theme_minimal() +   
    theme(plot.title = element_text(size = 22, face = "bold"),     
          axis.title.x = element_text(size = 24),     
          axis.title.y = element_text(size = 24),      
          axis.text.x = element_text(size = 10),     
          axis.text.y = element_text(size = 10)) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_apot_trends_n_wta_wfi_hum_wbi_wtm_ffi.pdf"), 
         plot = p_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no humans, no w. birds, no wt. mammals, no f. fish, no chickens
  filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi_fch <- filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi %>% 
    filter(Category != "Chickens")
  
  p_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi_fch <- ggplot(filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi_fch, aes(x = Year, y = NC_apot, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi_fch %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi_fch$Year), 
                                  max(filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi_fch$Year) + 30)) +
    labs(title = "NC Potential * Population Over Time (No wt. arthropods, No w. fish, No humans, No w. birds, No wt. mammals, No f. fish, No chickens)", 
         y = "NC Potential * Population", 
         x = "Year") +
    theme_minimal() +   
    theme(plot.title = element_text(size = 22, face = "bold"),     
          axis.title.x = element_text(size = 24),     
          axis.title.y = element_text(size = 24),      
          axis.text.x = element_text(size = 10),     
          axis.text.y = element_text(size = 10)) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_apot_trends_n_wta_wfi_hum_wbi_wtm_ffi_fch.pdf"), 
         plot = p_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi_fch, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no humans, no w. birds, no wt. mammals, no f. fish, no chickens, no wild birds
  filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi_fch_wbi <- filtered_nc_apot_n_wta_wfi_hum_wbi_wtm_ffi_fch %>% 
    filter(Category != "Wild birds")
  
  cat("NC apot plots saved to:", output_dir, "\n")
}

#' Create NC utility trend plots with progressive category exclusions
#' 
#' @param data The processed dataset
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
create_nc_utility_plots <- function(data, output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Filter out rows with NA values for NC utility
  filtered_data_nc <- data %>%
    filter(!is.na(NC_utility), !is.na(aliveatanytime))
  
  # NC utility over time - all categories
  p_nc <- ggplot(filtered_data_nc, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_data_nc %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_data_nc$Year), 
                                  max(filtered_data_nc$Year) + 30)) +
    labs(title = "Utility Over Time - NC Method (1950-2025)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_utility_trends.pdf"), 
         plot = p_nc, width = 10, height = 6)
  
  # No wild terrestrial arthropods
  filtered_nc_n_wta <- filtered_data_nc %>% 
    filter(Category != "Wild terrestrial arthropods")
  
  p_nc_n_wta <- ggplot(filtered_nc_n_wta, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_n_wta %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_n_wta$Year), 
                                  max(filtered_nc_n_wta$Year) + 30)) +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta.pdf"), 
         plot = p_nc_n_wta, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish
  filtered_nc_n_wta_wfi <- filtered_nc_n_wta %>% 
    filter(Category != "Wild fish")
  
  p_nc_n_wta_wfi <- ggplot(filtered_nc_n_wta_wfi, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_n_wta_wfi %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_n_wta_wfi$Year), 
                                  max(filtered_nc_n_wta_wfi$Year) + 30)) +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi.pdf"), 
         plot = p_nc_n_wta_wfi, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no humans
  filtered_nc_n_wta_wfi_hum <- filtered_nc_n_wta_wfi %>% 
    filter(Category != "Humans")
  
  p_nc_n_wta_wfi_hum <- ggplot(filtered_nc_n_wta_wfi_hum, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_n_wta_wfi_hum %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_n_wta_wfi_hum$Year), 
                                  max(filtered_nc_n_wta_wfi_hum$Year) + 30)) +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish, No humans)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_hum.pdf"), 
         plot = p_nc_n_wta_wfi_hum, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no humans, no wild terrestrial mammals
  filtered_nc_n_wta_wfi_hum_wtm <- filtered_nc_n_wta_wfi_hum %>% 
    filter(Category != "Wild terrestrial mammals")
  
  p_nc_n_wta_wfi_hum_wtm <- ggplot(filtered_nc_n_wta_wfi_hum_wtm, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_n_wta_wfi_hum_wtm %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_n_wta_wfi_hum_wtm$Year), 
                                  max(filtered_nc_n_wta_wfi_hum_wtm$Year) + 30)) +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish, No humans, No wt. mammals)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_hum_wtm.pdf"), 
         plot = p_nc_n_wta_wfi_hum_wtm, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no humans, no wild terrestrial mammals, no farmed fish
  filtered_nc_n_wta_wfi_hum_wtm_ffi <- filtered_nc_n_wta_wfi_hum_wtm %>% 
    filter(Category != "Fish")
  
  p_nc_n_wta_wfi_hum_wtm_ffi <- ggplot(filtered_nc_n_wta_wfi_hum_wtm_ffi, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_n_wta_wfi_hum_wtm_ffi %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_n_wta_wfi_hum_wtm_ffi$Year), 
                                  max(filtered_nc_n_wta_wfi_hum_wtm_ffi$Year) + 30)) +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish, No humans, No wt. mammals, No f. fish)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_hum_wtm_ffi.pdf"), 
         plot = p_nc_n_wta_wfi_hum_wtm_ffi, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no humans, no wt. mammals, no f. fish, no chickens
  filtered_nc_n_wta_wfi_hum_wtm_ffi_fch <- filtered_nc_n_wta_wfi_hum_wtm_ffi %>% 
    filter(Category != "Chickens")
  
  p_nc_n_wta_wfi_hum_wtm_ffi_fch <- ggplot(filtered_nc_n_wta_wfi_hum_wtm_ffi_fch, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_n_wta_wfi_hum_wtm_ffi_fch %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_n_wta_wfi_hum_wtm_ffi_fch$Year), 
                                  max(filtered_nc_n_wta_wfi_hum_wtm_ffi_fch$Year) + 30)) +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish, No humans, No wt. mammals, No f. fish, No chickens)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_hum_wtm_ffi_fch.pdf"), 
         plot = p_nc_n_wta_wfi_hum_wtm_ffi_fch, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no humans, no wt. mammals, no f. fish, no chickens, no wild birds
  filtered_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi <- filtered_nc_n_wta_wfi_hum_wtm_ffi_fch %>% 
    filter(Category != "Wild birds")
  
  p_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi <- ggplot(filtered_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi$Year), 
                                  max(filtered_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi$Year) + 30)) +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish, No humans, No wt. mammals, No f. fish, No chickens, No w. birds)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_hum_wtm_ffi_fch_wbi.pdf"), 
         plot = p_nc_n_wta_wfi_hum_wtm_ffi_fch_wbi, width = 10, height = 6)
  
  cat("NC utility plots saved to:", output_dir, "\n")
}

#' Create WR utility trend plots with progressive category exclusions
#' 
#' @param data The processed dataset
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
create_wr_utility_plots <- function(data, output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Filter out rows with NA values for WR utility
  filtered_data_wr <- data %>%
    filter(!is.na(WR_utility), !is.na(aliveatanytime))
  
  # WR utility over time - all categories
  p_wr <- ggplot(filtered_data_wr, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_data_wr %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_data_wr$Year), 
                                  max(filtered_data_wr$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (1950-2025)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends.pdf"), 
         plot = p_wr, width = 10, height = 6)
  
  # No wild terrestrial arthropods
  filtered_wr_n_wta <- filtered_data_wr %>% 
    filter(Category != "Wild terrestrial arthropods")
  
  p_wr_n_wta <- ggplot(filtered_wr_n_wta, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_wr_n_wta %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_wr_n_wta$Year), 
                                  max(filtered_wr_n_wta$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta.pdf"), 
         plot = p_wr_n_wta, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish
  filtered_wr_n_wta_wfi <- filtered_wr_n_wta %>% 
    filter(Category != "Wild fish")
  
  p_wr_n_wta_wfi <- ggplot(filtered_wr_n_wta_wfi, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_wr_n_wta_wfi %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_wr_n_wta_wfi$Year), 
                                  max(filtered_wr_n_wta_wfi$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi.pdf"), 
         plot = p_wr_n_wta_wfi, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no bees
  filtered_wr_n_wta_wfi_fbe <- filtered_wr_n_wta_wfi %>% 
    filter(Category != "Bees")
  
  p_wr_n_wta_wfi_fbe <- ggplot(filtered_wr_n_wta_wfi_fbe, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_wr_n_wta_wfi_fbe %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_wr_n_wta_wfi_fbe$Year), 
                                  max(filtered_wr_n_wta_wfi_fbe$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe.pdf"), 
         plot = p_wr_n_wta_wfi_fbe, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish, no bees, no farmed fish
  filtered_wr_n_wta_wfi_fbe_ffi <- filtered_wr_n_wta_wfi_fbe %>% 
    filter(Category != "Fish")
  
  p_wr_n_wta_wfi_fbe_ffi <- ggplot(filtered_wr_n_wta_wfi_fbe_ffi, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_wr_n_wta_wfi_fbe_ffi %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_wr_n_wta_wfi_fbe_ffi$Year), 
                                  max(filtered_wr_n_wta_wfi_fbe_ffi$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees, No f. fish)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe_ffi.pdf"), 
         plot = p_wr_n_wta_wfi_fbe_ffi, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no chickens
  filtered_wr_n_wta_wfi_fbe_ffi_fch <- filtered_wr_n_wta_wfi_fbe_ffi %>% 
    filter(Category != "Chickens")
  
  p_wr_n_wta_wfi_fbe_ffi_fch <- ggplot(filtered_wr_n_wta_wfi_fbe_ffi_fch, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_wr_n_wta_wfi_fbe_ffi_fch %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_wr_n_wta_wfi_fbe_ffi_fch$Year), 
                                  max(filtered_wr_n_wta_wfi_fbe_ffi_fch$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees, No f. fish, No chickens)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe_ffi_fch.pdf"), 
         plot = p_wr_n_wta_wfi_fbe_ffi_fch, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no chickens, no wild terrestrial mammals
  filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm <- filtered_wr_n_wta_wfi_fbe_ffi_fch %>% 
    filter(Category != "Wild terrestrial mammals")
  
  p_wr_n_wta_wfi_fbe_ffi_fch_wtm <- ggplot(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm$Year), 
                                  max(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees, No f. fish, No chickens, No wt. mammals)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe_ffi_fch_wtm.pdf"), 
         plot = p_wr_n_wta_wfi_fbe_ffi_fch_wtm, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no chickens, no wt. mammals, no humans
  filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum <- filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm %>% 
    filter(Category != "Humans")
  
  p_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum <- ggplot(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum$Year), 
                                  max(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees, No f. fish, No chickens, No wt. mammals, No humans)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe_ffi_fch_wtm_hum.pdf"), 
         plot = p_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum, width = 10, height = 6)
  
  # No wt. arthropods, no w. fish, no bees, no f. fish, no chickens, no wt. mammals, no humans, no wild birds
  filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi <- filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum %>% 
    filter(Category != "Wild birds")
  
  p_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi <- ggplot(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi$Year), 
                                  max(filtered_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish, No bees, No f. fish, No chickens, No wt. mammals, No humans, No w. birds)", 
         y = "Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi.pdf"), 
         plot = p_wr_n_wta_wfi_fbe_ffi_fch_wtm_hum_wbi, width = 10, height = 6)
  
  cat("WR utility plots saved to:", output_dir, "\n")
}





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

#' Prepare data for net series calculations by extending time ranges. Calls 
#' extend_animal_trends and create_trend_extension_plots
#' 
#' @param data The processed dataset
#' @param output_dir Directory for saving trend extension plots
#' @param target_year_range Year range to extend data to (default: 1960:2019)
#' @return Extended dataset suitable for net series calculations
prepare_data_for_net_series <- function(data, 
                                        output_dir = "visualizations",
                                        target_year_range = 1960:2023) {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extend time range of animal categories for net series calculation
  cat("Extending animal trends for net series calculation...\n")
  extended_data_for_net <- extend_animal_trends(data, target_year_range = target_year_range)
  
  # Create trend extension plots directory and sanity check
  extension_plots_dir <- file.path(output_dir, "trend_extension_plots")
  if(!dir.exists(extension_plots_dir)) {
    dir.create(extension_plots_dir, recursive = TRUE)
  }
  
  # Create trend extension plots for quality checking
  cat("Creating trend extension plots for quality checking...\n")
  create_trend_extension_plots(data, extended_data_for_net, extension_plots_dir)
  
  cat("Data preparation for net series complete. Extended data covers", 
      min(target_year_range), "to", max(target_year_range), "\n")
  cat("Trend extension plots saved to:", extension_plots_dir, "\n")
  
  # Ensure save directory for extended data exists
  if(!dir.exists("fifth_pass/dat/")) {
    dir.create("fifth_pass/dat/", recursive = TRUE)
  }
  
  # Save extended data for reuse in cor and elas calculations
  write.xlsx(extended_data_for_net, "fifth_pass/dat/extended_integrated_calc_tseries.xlsx")
  
  return(extended_data_for_net)
}




#' Create four-panel population comparison plots with stacked areas and simplified legends
#' 
#' @param data The extended_integrated_calc_tseries dataset
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
create_four_panel_population_plots <- function(data, output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Creating four-panel population comparison plots...\n")
  
  # Prepare human population data (single category)
  human_pop <- data %>%
    filter(Category == "Humans") %>%
    select(Year, aliveatanytime) %>%
    filter(!is.na(aliveatanytime), aliveatanytime > 0)
  
  # Prepare farmed animal data - only show Bees and Fish separately, group others
  farmed_pop <- data %>%
    filter(Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)")) %>%
    select(Year, Category, aliveatanytime) %>%
    filter(!is.na(aliveatanytime), !is.na(Category)) %>%
    mutate(
      Year = as.numeric(Year),
      # Simplify categories - only show Bees and Fish, group others
      Category_simplified = case_when(
        Category == "Bees" ~ "Bees",
        Category == "Fish" ~ "Fish",
        TRUE ~ "Other Farmed Animals"
      )
    ) %>%
    group_by(Year, Category_simplified) %>%
    summarise(aliveatanytime = sum(aliveatanytime, na.rm = TRUE), .groups = "drop") %>%
    rename(Category = Category_simplified) %>%
    complete(Year, Category, fill = list(aliveatanytime = 0)) %>%
    group_by(Year) %>%
    filter(sum(aliveatanytime, na.rm = TRUE) > 0) %>%
    ungroup()
  
  # Prepare wild animal data - only show Wild terrestrial arthropods and Wild fish, group others
  wild_pop <- data %>%
    filter(Group == "Wild Animals") %>%
    select(Year, Category, aliveatanytime) %>%
    filter(!is.na(aliveatanytime), !is.na(Category)) %>%
    mutate(
      Year = as.numeric(Year),
      # Simplify categories - only show arthropods and fish, group others
      Category_simplified = case_when(
        Category == "Wild terrestrial arthropods" ~ "Wild terrestrial arthropods",
        TRUE ~ "Other Wild Animals"
      )
    ) %>%
    group_by(Year, Category_simplified) %>%
    summarise(aliveatanytime = sum(aliveatanytime, na.rm = TRUE), .groups = "drop") %>%
    rename(Category = Category_simplified) %>%
    complete(Year, Category, fill = list(aliveatanytime = 0)) %>%
    group_by(Year) %>%
    filter(sum(aliveatanytime, na.rm = TRUE) > 0) %>%
    ungroup()
  
  # Aggregated totals for the comparison panel
  farmed_total <- farmed_pop %>%
    group_by(Year) %>%
    summarise(aliveatanytime = sum(aliveatanytime, na.rm = TRUE), .groups = "drop") %>%
    filter(aliveatanytime > 0)
  
  wild_total <- wild_pop %>%
    group_by(Year) %>%
    summarise(aliveatanytime = sum(aliveatanytime, na.rm = TRUE), .groups = "drop") %>%
    filter(aliveatanytime > 0)
  
  # Set theme
  original_theme <- theme_get()
  theme_set(theme_minimal(base_size = 12) +
              theme(
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 11, color = "grey30"),
                strip.text = element_text(face = "bold"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.ticks = element_line(color = "grey70"),
                axis.line = element_line(color = "grey70")
              ))
  
  # Color palettes
  main_colors <- c("Humans" = "#2E86AB", "Farmed Animals" = "#F24236", "Wild Animals" = "#27AE60")
  
  # Panel 1: Human Population (simple area)
  p1 <- ggplot(human_pop, aes(x = Year, y = aliveatanytime)) +
    geom_area(alpha = 0.7, fill = main_colors["Humans"]) +
    geom_line(color = main_colors["Humans"], size = 1.2) +
    scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B")) +
    labs(title = "Human Population",
         y = "Population (Billions)") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # Panel 2: Farmed Animals (stacked area chart with simplified categories)
  p2 <- ggplot(farmed_pop, aes(x = Year, y = aliveatanytime, fill = Category)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c("Bees" = "#FFD700", "Fish" = "#20B2AA", "Other Farmed Animals" = "#D2691E")) +
    scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B")) +
    labs(title = "Farmed Animal Population",
         y = "Population (Billions)",
         fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 9))
  
  # Panel 3: Wild Animals (stacked area chart with simplified categories)
  p3 <- ggplot(wild_pop, aes(x = Year, y = aliveatanytime, fill = Category)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = c("Wild terrestrial arthropods" = "#8B4513", 
                                 "Other Wild Animals" = "#90EE90")) +
    scale_y_continuous(labels = label_number(scale = 1e-12, suffix = "T")) +
    labs(title = "Wild Animal Population",
         y = "Population (Trillions)",
         fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 9))
  
  # Panel 4: Combined comparison (relative to baseline) with labels - FIXED
  combined_data <- bind_rows(
    human_pop %>% mutate(Group = "Humans"),
    farmed_total %>% mutate(Group = "Farmed Animals"),
    wild_total %>% mutate(Group = "Wild Animals")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals"))) %>%
    # Transform to relative scale (baseline = 1.0)
    group_by(Group) %>%
    arrange(Year) %>%
    mutate(baseline_value = first(aliveatanytime),
           relative_value = aliveatanytime / baseline_value) %>%
    ungroup()
  
  p4 <- ggplot(combined_data, aes(x = Year, y = relative_value,
                                  color = Group, fill = Group)) +
    geom_area(alpha = 0.4, position = "identity") +
    geom_line(size = 1.2) +
    # Add labels at the end of each line
    geom_text(data = combined_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1, suffix = "x")) +
    scale_color_manual(values = c("Humans" = "#2E86AB", "Farmed Animals" = "#FFD700", "Wild Animals" = "#8B4513")) +
    scale_fill_manual(values = c("Humans" = "#2E86AB", "Farmed Animals" = "#FFD700", "Wild Animals" = "#8B4513")) +
    # Extend x-axis to make room for labels
    scale_x_continuous(limits = c(min(combined_data$Year), 
                                  max(combined_data$Year) + 8)) +
    labs(title = "Comparative Population Trends",
         subtitle = "Relative to baseline year reveals growth patterns",
         y = "Population (Relative to Baseline)",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")  # Remove legend since we have direct labels
  
  # Combine with patchwork
  final_plot <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
      title = "Global Population Dynamics: Humans, Farmed Animals, and Wildlife",
      subtitle = "A comparative analysis of population trends across sentient beings (1950-2025)"
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Save plots
  universal_ggsave(final_plot, "four_panel_population_comparison", output_dir,
                   pdf_width = 16, pdf_height = 10)
  
  # Individual panels
  universal_ggsave(p1, "population_humans_only", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p2, "population_farmed_stacked", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p3, "population_wild_stacked", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p4, "population_comparison_log", output_dir,
                   pdf_width = 10, pdf_height = 6)
  
  # Restore theme
  theme_set(original_theme)
  
  cat("Four-panel population comparison plots saved to:", output_dir, "\n")
}


#' Create four-panel NC_total comparison plots with stacked areas
#' 
#' @param data The extended_integrated_calc_tseries dataset
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
create_four_panel_nc_tot_plots <- function(data, output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Creating four-panel NC_tot comparison plots...\n")
  
  # Ensure NC_tot column exists
  data <- ensure_nc_columns(data)
  
  # Prepare human NC_tot data (single category)
  human_nc_tot <- data %>%
    filter(Category == "Humans") %>%
    select(Year, NC_tot) %>%
    filter(!is.na(NC_tot), NC_tot > 0)
  
  # Prepare farmed animal NC_tot data
  farmed_nc_tot <- data %>%
    filter(Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)")) %>%
    select(Year, Category, NC_tot) %>%
    filter(!is.na(NC_tot), !is.na(Category)) %>%
    mutate(
      Year = as.numeric(Year),
      # Simplify categories - show major animals separately
      Category_simplified = case_when(
        Category == "Fish" ~ "Fish",
        Category == "Chickens" ~ "Chickens", 
        Category == "Swine / Pigs" ~ "Swine / Pigs",
        Category == "Goats" ~ "Goats",
        Category == "Cattle" ~ "Cattle",
        Category == "Sheep" ~ "Sheep",
        TRUE ~ "Other Farmed Animals"
      )
    ) %>%
    group_by(Year, Category_simplified) %>%
    summarise(NC_tot = sum(NC_tot, na.rm = TRUE), .groups = "drop") %>%
    rename(Category = Category_simplified)
  
  # Get the most recent year to determine ordering
  most_recent_year <- max(farmed_nc_tot$Year, na.rm = TRUE)
  
  # Calculate ordering based on most recent year's values (largest to smallest)
  category_order <- farmed_nc_tot %>%
    filter(Year == most_recent_year) %>%
    arrange(desc(NC_tot)) %>%
    pull(Category)
  
  # Apply the ordering as a factor
  farmed_nc_tot <- farmed_nc_tot %>%
    mutate(Category = factor(Category, levels = category_order))
  
  # Prepare wild animal NC_tot data
  wild_nc_tot <- data %>%
    filter(Group == "Wild Animals") %>%
    select(Year, Category, NC_tot) %>%
    filter(!is.na(NC_tot), !is.na(Category)) %>%
    mutate(
      Year = as.numeric(Year),
      # Simplify categories - only show arthropods separately
      Category_simplified = case_when(
        Category == "Wild terrestrial arthropods" ~ "Wild terrestrial arthropods",
        TRUE ~ "Other Wild Animals"
      )
    ) %>%
    group_by(Year, Category_simplified) %>%
    summarise(NC_tot = sum(NC_tot, na.rm = TRUE), .groups = "drop") %>%
    rename(Category = Category_simplified)
  
  # Get ordering for wild animals (arthropods should be on top as they're larger)
  wild_most_recent <- max(wild_nc_tot$Year, na.rm = TRUE)
  wild_category_order <- wild_nc_tot %>%
    filter(Year == wild_most_recent) %>%
    arrange(desc(NC_tot)) %>%
    pull(Category)
  
  wild_nc_tot <- wild_nc_tot %>%
    mutate(Category = factor(Category, levels = wild_category_order))
  
  # Aggregated totals for the comparison panel
  farmed_total <- farmed_nc_tot %>%
    group_by(Year) %>%
    summarise(NC_tot = sum(NC_tot, na.rm = TRUE), .groups = "drop") %>%
    filter(NC_tot > 0)
  
  wild_total <- wild_nc_tot %>%
    group_by(Year) %>%
    summarise(NC_tot = sum(NC_tot, na.rm = TRUE), .groups = "drop") %>%
    filter(NC_tot > 0)
  
  # Set theme
  original_theme <- theme_get()
  theme_set(theme_minimal(base_size = 12) +
              theme(
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 11, color = "grey30"),
                strip.text = element_text(face = "bold"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.ticks = element_line(color = "grey70"),
                axis.line = element_line(color = "grey70")
              ))
  
  # Color palettes
  main_colors <- c("Humans" = "#2E86AB", "Farmed Animals" = "#20B2AA", "Wild Animals" = "#8B4513")
  
  # Panel 1: Human Total Neurons (simple area)
  p1 <- ggplot(human_nc_tot, aes(x = Year, y = NC_tot)) +
    geom_area(alpha = 0.7, fill = main_colors["Humans"]) +
    geom_line(color = main_colors["Humans"], size = 1.2) +
    scale_y_continuous(labels = label_number(scale = 1e-12, suffix = "T")) +
    labs(title = "Human Total Neurons",
         y = "Total Neurons (Trillions)") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # Panel 2: Farmed Animals Total Neurons (stacked area chart)
  # Create a named vector for colors based on the actual categories present
  farmed_colors <- c("Chickens" = "#FF0000",        # Red
                     "Pigs" = "#FFC0CB",            # Pink  
                     "Goats" = "#CC7722",           # Ochre
                     "Sheep" = "#32CD32",           # Green
                     "Cattle" = "#8B4513",          # Brown
                     "Fish" = "#20B2AA",            # Teal
                     "Other Farmed Animals" = "#FFD39B") # Orange
  
  # Filter to only colors for categories that exist
  used_farmed_colors <- farmed_colors[names(farmed_colors) %in% unique(farmed_nc_tot$Category)]
  
  p2 <- ggplot(farmed_nc_tot, aes(x = Year, y = NC_tot, fill = Category)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = used_farmed_colors) +
    scale_y_continuous(labels = label_number(scale = 1e-12, suffix = "T")) +
    labs(title = "Farmed Animal Total Neurons",
         y = "Total Neurons (Trillions)",
         fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 8)) +
    guides(fill = guide_legend(nrow = 2))
  
  # Panel 3: Wild Animals Total Neurons (stacked area chart)
  wild_colors <- c("Wild terrestrial arthropods" = "#8B4513", 
                   "Other Wild Animals" = "#90EE90")
  
  used_wild_colors <- wild_colors[names(wild_colors) %in% unique(wild_nc_tot$Category)]
  
  p3 <- ggplot(wild_nc_tot, aes(x = Year, y = NC_tot, fill = Category)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = used_wild_colors) +
    scale_y_continuous(labels = label_number(scale = 1e-15, suffix = "Q")) +
    labs(title = "Wild Animal Total Neurons",
         y = "Total Neurons (Quadrillions)",
         fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 9))
  
  # Panel 4: Combined comparison (relative to baseline) with labels
  combined_data <- bind_rows(
    human_nc_tot %>% mutate(Group = "Humans"),
    farmed_total %>% mutate(Group = "Farmed Animals"),
    wild_total %>% mutate(Group = "Wild Animals")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals"))) %>%
    # Transform to relative scale (baseline = 1.0)
    group_by(Group) %>%
    arrange(Year) %>%
    mutate(baseline_value = first(NC_tot),
           relative_value = NC_tot / baseline_value) %>%
    ungroup()
  
  p4 <- ggplot(combined_data, aes(x = Year, y = relative_value,
                                  color = Group, fill = Group)) +
    geom_area(alpha = 0.4, position = "identity") +
    geom_line(size = 1.2) +
    # Add labels at the end of each line
    geom_text(data = combined_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1, suffix = "x")) +
    scale_color_manual(values = main_colors) +
    scale_fill_manual(values = main_colors) +
    # Extend x-axis to make room for labels
    scale_x_continuous(limits = c(min(combined_data$Year), 
                                  max(combined_data$Year) + 8)) +
    labs(title = "Comparative Total Neuron Trends",
         subtitle = "Relative to baseline year reveals neural capacity distribution",
         y = "Total Neurons (Relative to Baseline)",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")  # Remove legend since we have direct labels
  
  # Combine with patchwork
  final_plot <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
      title = "Global Neural Count: Aggregate Forebrain Neurons Across Sentient Beings",
      subtitle = "A comparative analysis of total neural trends (1950-2023)"
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Save plots
  universal_ggsave(final_plot, "four_panel_nc_tot_comparison", output_dir,
                   pdf_width = 16, pdf_height = 10)
  
  # Individual panels
  universal_ggsave(p1, "nc_tot_humans_only", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p2, "nc_tot_farmed_stacked", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p3, "nc_tot_wild_stacked", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p4, "nc_tot_comparison_log", output_dir,
                   pdf_width = 10, pdf_height = 6)
  
  # Restore theme
  theme_set(original_theme)
  
  cat("Four-panel NC_tot comparison plots saved to:", output_dir, "\n")
}


#' Create four-panel NC_apot (aliveatanytime * NC_potential) comparison plots 
#' with stacked areas
#' 
#' @param data The extended_integrated_calc_tseries dataset
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
create_four_panel_nc_apot_plots <- function(data, output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Creating four-panel NC_apot comparison plots...\n")
  
  # Ensure NC_apot column exists
  data <- ensure_nc_columns(data)
  
  # Prepare human NC_apot data (single category)
  human_nc_apot <- data %>%
    filter(Category == "Humans") %>%
    select(Year, NC_apot) %>%
    filter(!is.na(NC_apot), NC_apot > 0)
  
  # Prepare farmed animal NC_apot data
  farmed_nc_apot <- data %>%
    filter(Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)")) %>%
    select(Year, Category, NC_apot) %>%
    filter(!is.na(NC_apot), !is.na(Category)) %>%
    mutate(
      Year = as.numeric(Year),
      # Simplify categories - show major animals separately
      Category_simplified = case_when(
        Category == "Fish" ~ "Fish",
        Category == "Chickens" ~ "Chickens", 
        Category == "Cattle" ~ "Cattle",
        Category == "Sheep" ~ "Sheep",
        Category == "Pigs" ~ "Pigs",
        Category == "Goats" ~ "Goats",
        TRUE ~ "Other Farmed Animals"
      )
    ) %>%
    group_by(Year, Category_simplified) %>%
    summarise(NC_apot = sum(NC_apot, na.rm = TRUE), .groups = "drop") %>%
    rename(Category = Category_simplified)
  
  # Get the most recent year to determine ordering
  most_recent_year <- max(farmed_nc_apot$Year, na.rm = TRUE)
  
  # Calculate ordering based on most recent year's values (largest to smallest)
  category_order <- farmed_nc_apot %>%
    filter(Year == most_recent_year) %>%
    arrange(desc(NC_apot)) %>%
    pull(Category)
  
  # Apply the ordering as a factor
  farmed_nc_apot <- farmed_nc_apot %>%
    mutate(Category = factor(Category, levels = category_order))
  
  # Prepare wild animal NC_apot data
  wild_nc_apot <- data %>%
    filter(Group == "Wild Animals") %>%
    select(Year, Category, NC_apot) %>%
    filter(!is.na(NC_apot), !is.na(Category)) %>%
    mutate(
      Year = as.numeric(Year),
      # Simplify categories - only show arthropods separately
      Category_simplified = case_when(
        Category == "Wild terrestrial arthropods" ~ "Wild terrestrial arthropods",
        TRUE ~ "Other Wild Animals"
      )
    ) %>%
    group_by(Year, Category_simplified) %>%
    summarise(NC_apot = sum(NC_apot, na.rm = TRUE), .groups = "drop") %>%
    rename(Category = Category_simplified)
  
  # Get ordering for wild animals (arthropods should be on top as they're larger)
  wild_most_recent <- max(wild_nc_apot$Year, na.rm = TRUE)
  wild_category_order <- wild_nc_apot %>%
    filter(Year == wild_most_recent) %>%
    arrange(desc(NC_apot)) %>%
    pull(Category)
  
  wild_nc_apot <- wild_nc_apot %>%
    mutate(Category = factor(Category, levels = wild_category_order))
  
  # Aggregated totals for the comparison panel
  farmed_total <- farmed_nc_apot %>%
    group_by(Year) %>%
    summarise(NC_apot = sum(NC_apot, na.rm = TRUE), .groups = "drop") %>%
    filter(NC_apot > 0)
  
  wild_total <- wild_nc_apot %>%
    group_by(Year) %>%
    summarise(NC_apot = sum(NC_apot, na.rm = TRUE), .groups = "drop") %>%
    filter(NC_apot > 0)
  
  # Set theme
  original_theme <- theme_get()
  theme_set(theme_minimal(base_size = 12) +
              theme(
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 11, color = "grey30"),
                strip.text = element_text(face = "bold"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.ticks = element_line(color = "grey70"),
                axis.line = element_line(color = "grey70")
              ))
  
  # Color palettes
  main_colors <- c("Humans" = "#2E86AB", "Farmed Animals" = "#20B2AA", "Wild Animals" = "#8B4513")
  
  # Panel 1: Human Total Neurons (simple area)
  p1 <- ggplot(human_nc_apot, aes(x = Year, y = NC_apot)) +
    geom_area(alpha = 0.7, fill = main_colors["Humans"]) +
    geom_line(color = main_colors["Humans"], size = 1.2) +
    scale_y_continuous(labels = label_number(scale = 1e-12, suffix = "T")) +
    labs(title = "Human Total Neurons",
         y = "Total Neurons (Trillions)") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # Panel 2: Farmed Animals Total Neurons (stacked area chart)
  # Create a named vector for colors based on the actual categories present
  farmed_colors <- c("Chickens" = "#FF0000",        # Red
                     "Pigs" = "#FFC0CB",            # Pink  
                     "Goats" = "#CC7722",           # Ochre
                     "Sheep" = "#32CD32",           # Green
                     "Cattle" = "#8B4513",          # Brown
                     "Fish" = "#20B2AA",            # Teal
                     "Other Farmed Animals" = "#FFD39B") # Orange
  
  # Filter to only colors for categories that exist
  used_farmed_colors <- farmed_colors[names(farmed_colors) %in% unique(farmed_nc_apot$Category)]
  
  p2 <- ggplot(farmed_nc_apot, aes(x = Year, y = NC_apot, fill = Category)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = used_farmed_colors) +
    scale_y_continuous(labels = label_number(scale = 1e-12, suffix = "T")) +
    labs(title = "Farmed Animal Total Neurons",
         y = "Total Neurons (Trillions)",
         fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 8)) +
    guides(fill = guide_legend(nrow = 2))
  
  # Panel 3: Wild Animals Total Neurons (stacked area chart)
  wild_colors <- c("Wild terrestrial arthropods" = "#8B4513", 
                   "Other Wild Animals" = "#90EE90")
  
  used_wild_colors <- wild_colors[names(wild_colors) %in% unique(wild_nc_apot$Category)]
  
  p3 <- ggplot(wild_nc_apot, aes(x = Year, y = NC_apot, fill = Category)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = used_wild_colors) +
    scale_y_continuous(labels = label_number(scale = 1e-15, suffix = "Q")) +
    labs(title = "Wild Animal Total Neurons",
         y = "Total Neurons (Quadrillions)",
         fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 9))
  
  # Panel 4: Combined comparison (relative to baseline) with labels
  combined_data <- bind_rows(
    human_nc_apot %>% mutate(Group = "Humans"),
    farmed_total %>% mutate(Group = "Farmed Animals"),
    wild_total %>% mutate(Group = "Wild Animals")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals"))) %>%
    # Transform to relative scale (baseline = 1.0)
    group_by(Group) %>%
    arrange(Year) %>%
    mutate(baseline_value = first(NC_apot),
           relative_value = NC_apot / baseline_value) %>%
    ungroup()
  
  p4 <- ggplot(combined_data, aes(x = Year, y = relative_value,
                                  color = Group, fill = Group)) +
    geom_area(alpha = 0.4, position = "identity") +
    geom_line(size = 1.2) +
    # Add labels at the end of each line
    geom_text(data = combined_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1, suffix = "x")) +
    scale_color_manual(values = main_colors) +
    scale_fill_manual(values = main_colors) +
    # Extend x-axis to make room for labels
    scale_x_continuous(limits = c(min(combined_data$Year), 
                                  max(combined_data$Year) + 8)) +
    labs(title = "Comparative Total Relative Neural Capacity Trends",
         subtitle = "Relative to baseline year reveals neural capacity distribution",
         y = "Total Neurons (Relative to Baseline)",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")  # Remove legend since we have direct labels
  
  # Combine with patchwork
  final_plot <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
      title = "Global Neural Capacity: Relative neuron count * Population at any time",
      subtitle = "A comparative analysis of total neural capacity trends (1960-2023)"
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Save plots
  universal_ggsave(final_plot, "four_panel_nc_apot_comparison", output_dir,
                   pdf_width = 16, pdf_height = 10)
  
  # Individual panels
  universal_ggsave(p1, "nc_apot_humans_only", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p2, "nc_apot_farmed_stacked", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p3, "nc_apot_wild_stacked", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p4, "nc_apot_comparison_log", output_dir,
                   pdf_width = 10, pdf_height = 6)
  
  # Restore theme
  theme_set(original_theme)
  
  cat("Four-panel NC_apot comparison plots saved to:", output_dir, "\n")
}


#' Create four-panel human NC_utility with non-human animal score ranges with area 
#' fills, unstacked
#' 
#' @param data extended_integrated_calc_tseries
#' @param output_dir Directory for saving visualizations
#' @return p4 for further use by create_three_panel_nc_func_form_check
create_four_panel_nc_score_range_plots <- function(data, 
                                                output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Creating four-panel nc welfare score range plots...\n")
  
  # Ensure NC_utility column exists
  data <- ensure_nc_columns(data)
  
  # Prepare human NC_utility data
  human_nc_utility <- data %>%
    filter(Category == "Humans") %>%
    select(Year, NC_utility) %>%
    filter(!is.na(NC_utility))
  
  # Prepare farmed animal NC_apot data (aggregated and inverted)
  #Due to CE; change this later when not doing CE
  max_f_welfare_score <- 100
  farmed_NC_urange <- data %>%
    filter(Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)")) %>%
    group_by(Year) %>%
    summarise(NC_apot_total = sum(NC_apot, na.rm = TRUE), .groups = "drop") %>%
    mutate(NC_urange = max_f_welfare_score*NC_apot_total) %>%  #scale up due to range of welfare score
    mutate(NC_urange = -NC_urange) %>%  # Reflect across x-axis
    filter(!is.na(NC_urange))
  
  # Prepare wild animal NC_apot data (aggregated, scaled down, with BOTH positive and negative)
  max_w_welfare_score <- 0.001
  wild_NC_urange_base <- data %>%
    filter(Group == "Wild Animals") %>%
    group_by(Year) %>%
    summarise(NC_apot_total = sum(NC_apot, na.rm = TRUE), .groups = "drop") %>%
    mutate(NC_urange = max_w_welfare_score*NC_apot_total) %>%  #scale up due to range of welfare score
    filter(!is.na(NC_urange))
  
  # Create both positive and negative versions for wild animals
  wild_NC_urange_full <- bind_rows(
    wild_NC_urange_base %>% mutate(value_type = "positive"),
    wild_NC_urange_base %>% mutate(NC_urange = -NC_urange, value_type = "negative")
  )
  
  # Set theme
  original_theme <- theme_get()
  theme_set(theme_minimal(base_size = 12) +
              theme(
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 11, color = "grey30"),
                strip.text = element_text(face = "bold"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.ticks = element_line(color = "grey70"),
                axis.line = element_line(color = "grey70")
              ))
  
  # Color palette with updated farmed animal color
  main_colors <- c("Humans" = "#2E86AB", "Farmed Animals" = "#20B2AA", "Wild Animals" = "#8B4513")
  
  # Panel 1: Human Welfare Score (line only, no fill)
  p1 <- ggplot(human_nc_utility, aes(x = Year, y = NC_utility)) +
    geom_line(color = main_colors["Humans"], size = 1.2) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Human Welfare Score",
         y = "Welfare Score") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # Panel 2: Farmed Animal Welfare Score (inverted with area)
  p2 <- ggplot(farmed_NC_urange, aes(x = Year, y = NC_urange)) +
    geom_area(alpha = 0.7, fill = main_colors["Farmed Animals"]) +
    geom_line(color = main_colors["Farmed Animals"], size = 1.2) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Farmed Animal Welfare - Possible Range",
         y = "Welfare Score") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # Panel 3: Wild Animal Welfare Score (both positive and negative with areas)
  p3 <- ggplot(wild_NC_urange_full, aes(x = Year, y = NC_urange)) +
    geom_area(data = wild_NC_urange_full %>% filter(value_type == "positive"),
              alpha = 0.7, fill = main_colors["Wild Animals"]) +
    geom_area(data = wild_NC_urange_full %>% filter(value_type == "negative"),
              alpha = 0.7, fill = main_colors["Wild Animals"]) +
    geom_line(data = wild_NC_urange_full %>% filter(value_type == "positive"),
              color = main_colors["Wild Animals"], size = 1.2) +
    geom_line(data = wild_NC_urange_full %>% filter(value_type == "negative"),
              color = main_colors["Wild Animals"], size = 1.2) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Wild Animal Welfare - Possible Range",
         subtitle = paste0("Assuming average welfare score has a magnitude of ", max_w_welfare_score, " out of 100"),
         y = "Welfare Score") +
    theme(plot.title = element_text(size = 14, face = "bold")) 
  
  wild_NC_urange_base_neg <- wild_NC_urange_base %>%
    mutate(NC_urange = -NC_urange)
  
  # Panel 4: Combined comparison - need to combine all three with consistent naming
  combined_data <- bind_rows(
    human_nc_utility %>% 
      rename(value = NC_utility) %>%
      mutate(Group = "Humans"),
    farmed_NC_urange %>% 
      rename(value = NC_urange) %>%
      select(Year, value) %>%
      mutate(Group = "Farmed Animals"),
    wild_NC_urange_base %>%  # Use base (positive) for the combined view
      rename(value = NC_urange) %>%
      select(Year, value) %>%
      mutate(Group = "Wild Animals"),
    wild_NC_urange_base_neg %>%  # Use base (negative) for the combined view
      rename(value = NC_urange) %>%
      select(Year, value) %>%
      mutate(Group = "Wild Animals Neg")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals", "Wild Animals Neg")))
  
  # Create separate positive and negative data for proper area fills
  combined_data_split <- combined_data %>%
    mutate(
      positive_value = ifelse(value > 0, value, 0),
      negative_value = ifelse(value < 0, value, 0)
    )
  
  p4 <- ggplot(combined_data_split, aes(x = Year)) +
    # no fill for human values
    # negative line and fills for wild animals
    geom_area(data = combined_data_split %>% filter(Group == "Wild Animals"),
              aes(y = positive_value), 
              alpha = 0.4, fill = main_colors["Wild Animals"]) +
    geom_area(data = combined_data_split %>% filter(Group == "Wild Animals Neg"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Wild Animals"]) +
    # Add area fills for negative values (farmed)
    geom_area(data = combined_data_split %>% filter(Group == "Farmed Animals"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Farmed Animals"]) +
    # Add lines for all groups
    geom_line(data = combined_data,
              aes(y = value, color = Group), 
              size = 1.2) +
    # Add labels at the end of each line
    geom_text(data = combined_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(y = value, label = Group, color = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_color_manual(values = main_colors) +
    # Extend x-axis to make room for labels
    scale_x_continuous(limits = c(min(combined_data$Year), 
                                  max(combined_data$Year) + 8)) +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Introducing Welfare Score",
         subtitle = "Comparative welfare scores across sentient beings",
         y = "Welfare Score",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")  # Remove legend since we have direct labels
  
  # Combine with patchwork
  final_plot <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
      title = "Welfare Score Analysis: Introducing Utility Considerations",
      subtitle = "Exploring welfare scores across humans, farmed animals, and wildlife"
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Save plots
  universal_ggsave(final_plot, "four_panel_nc_welfare_score_range", output_dir,
                   pdf_width = 16, pdf_height = 10)
  
  # Individual panels
  universal_ggsave(p1, "welfare_score_humans_only", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p2, "welfare_score_farmed_inverted", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p3, "welfare_score_wild_scaled_inverted", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p4, "welfare_score_introducing", output_dir,
                   pdf_width = 10, pdf_height = 6)
  
  # Restore theme
  theme_set(original_theme)
  
  cat("Four-panel welfare score range plots saved to:", output_dir, "\n")
  
  return(p4)
}


#' Create three-panel nc_score_range fourth-panel-style plots with concave, 
#' linear, and convex functional forms for nc-based welfare potential
#' 
#' @param data extended_integrated_calc_tseries
#' @param output_dir Directory for saving visualizations
#' @param linear_plot p4 from create_four_panel_nc_score_range_plots - becomes middle panel
#' @return NULL (saves plots to files)
create_three_panel_nc_func_form_check <- function(data, output_dir = "visualizations", linear_plot) {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Creating three-panel neuron count functional form plot...\n")
  
  # Ensure NC_utility column exists
  data <- ensure_nc_columns(data)
  
  max_f_welfare_score <- 100
  max_w_welfare_score <- 0.001
  
  # Prepare human NC_utility data
  human_nc_utility <- data %>%
    filter(Category == "Humans") %>%
    select(Year, NC_utility) %>%
    filter(!is.na(NC_utility))
  
  # Prepare data for Farmed animals (concave + convex) - FIXED: removed negative sign from calculation
  farmed_NC_urange <- data %>%
    filter(Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)")) %>%
    mutate(
      # Apply transformations at row level, then multiply by welfare score
      NC_urange_conc = max_f_welfare_score * aliveatanytime * NC_pot_conc,
      NC_urange_conv = max_f_welfare_score * aliveatanytime * NC_pot_conv,
      NC_urange_linear = max_f_welfare_score * NC_utility  # for reference
    ) %>%
    group_by(Year) %>%
    summarise(
      # Sum the transformed values, then make negative
      NC_urange_conc = -sum(NC_urange_conc, na.rm = TRUE),
      NC_urange_conv = -sum(NC_urange_conv, na.rm = TRUE),
      NC_urange_linear = -sum(NC_urange_linear, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(NC_urange_conc) & !is.na(NC_urange_conv))
  
  # Prepare data for Wild animals (concave + convex)
  wild_NC_urange_base <- data %>%
    filter(Group == "Wild Animals") %>%
    mutate(
      # Apply transformations at row level, then multiply by welfare score
      NC_urange_conc = max_w_welfare_score * aliveatanytime * NC_pot_conc,
      NC_urange_conv = max_w_welfare_score * aliveatanytime * NC_pot_conv,
      NC_urange_linear = max_w_welfare_score * NC_utility  # for reference
    ) %>%
    group_by(Year) %>%
    summarise(
      # Sum the transformed values
      NC_urange_conc = sum(NC_urange_conc, na.rm = TRUE),
      NC_urange_conv = sum(NC_urange_conv, na.rm = TRUE),
      NC_urange_linear = sum(NC_urange_linear, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(NC_urange_conc) & !is.na(NC_urange_conv))
  
  # Create both positive and negative versions for wild animals
  wild_NC_urange_full_conc <- bind_rows(
    wild_NC_urange_base %>% 
      select(Year, NC_urange_conc) %>%
      mutate(value_type = "positive"),
    wild_NC_urange_base %>% 
      select(Year, NC_urange_conc) %>%
      mutate(NC_urange_conc = -NC_urange_conc, value_type = "negative")
  )
  
  wild_NC_urange_full_conv <- bind_rows(
    wild_NC_urange_base %>% 
      select(Year, NC_urange_conv) %>%
      mutate(value_type = "positive"),
    wild_NC_urange_base %>% 
      select(Year, NC_urange_conv) %>%
      mutate(NC_urange_conv = -NC_urange_conv, value_type = "negative")
  )
  
  # Set theme
  original_theme <- theme_get()
  theme_set(theme_minimal(base_size = 12) +
              theme(
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 11, color = "grey30"),
                strip.text = element_text(face = "bold"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.ticks = element_line(color = "grey70"),
                axis.line = element_line(color = "grey70")
              ))
  
  # Color palette - FIXED: Added "Wild Animals Neg" mapping
  main_colors <- c("Humans" = "#2E86AB", 
                   "Farmed Animals" = "#20B2AA", 
                   "Wild Animals" = "#8B4513",
                   "Wild Animals Neg" = "#8B4513")  # Same color for negative
  
  # Panel 1: Combined comparison - CONCAVE
  combined_data_conc <- bind_rows(
    human_nc_utility %>% 
      rename(value = NC_utility) %>%
      mutate(Group = "Humans"),
    farmed_NC_urange %>% 
      rename(value = NC_urange_conc) %>%
      select(Year, value) %>%
      mutate(Group = "Farmed Animals"),
    wild_NC_urange_base %>%
      rename(value = NC_urange_conc) %>%
      select(Year, value) %>%
      mutate(Group = "Wild Animals"),
    wild_NC_urange_base %>%
      rename(value = NC_urange_conc) %>%
      select(Year, value) %>%
      mutate(value = -value, Group = "Wild Animals Neg")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals", "Wild Animals Neg")))
  
  # Create separate positive and negative data for proper area fills
  combined_data_split_conc <- combined_data_conc %>%
    mutate(
      positive_value = ifelse(value > 0, value, 0),
      negative_value = ifelse(value < 0, value, 0)
    )
  
  p1 <- ggplot(combined_data_split_conc, aes(x = Year)) +
    # Area fills for positive values (wild animals)
    geom_area(data = combined_data_split_conc %>% filter(Group == "Wild Animals"),
              aes(y = positive_value), 
              alpha = 0.4, fill = main_colors["Wild Animals"]) +
    # Area fills for negative values (wild animals negative and farmed)
    geom_area(data = combined_data_split_conc %>% filter(Group == "Wild Animals Neg"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Wild Animals Neg"]) +
    geom_area(data = combined_data_split_conc %>% filter(Group == "Farmed Animals"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Farmed Animals"]) +
    # Add lines for all groups
    geom_line(data = combined_data_conc,
              aes(y = value, color = Group), 
              size = 1.2) +
    # Add labels at the end of each line
    geom_text(data = combined_data_conc %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(y = value, label = Group, color = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_color_manual(values = main_colors) +
    scale_x_continuous(limits = c(min(combined_data_conc$Year), 
                                  max(combined_data_conc$Year) + 8)) +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Concave: sqrt(neuron count ratio)",
         subtitle = "Diminishing returns to scale",
         y = "Welfare Score",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")
  
  # Panel 2: Linear (from input)
  p2 <- linear_plot + 
    labs(title = "Linear: neuron count ratio",
         subtitle = "Proportional returns to scale")
  
  # Panel 3: Combined comparison - CONVEX
  combined_data_conv <- bind_rows(
    human_nc_utility %>% 
      rename(value = NC_utility) %>%
      mutate(Group = "Humans"),
    farmed_NC_urange %>% 
      rename(value = NC_urange_conv) %>%
      select(Year, value) %>%
      mutate(Group = "Farmed Animals"),
    wild_NC_urange_base %>%
      rename(value = NC_urange_conv) %>%
      select(Year, value) %>%
      mutate(Group = "Wild Animals"),
    wild_NC_urange_base %>%
      rename(value = NC_urange_conv) %>%
      select(Year, value) %>%
      mutate(value = -value, Group = "Wild Animals Neg")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals", "Wild Animals Neg")))
  
  # Create separate positive and negative data for proper area fills
  combined_data_split_conv <- combined_data_conv %>%
    mutate(
      positive_value = ifelse(value > 0, value, 0),
      negative_value = ifelse(value < 0, value, 0)
    )
  
  p3 <- ggplot(combined_data_split_conv, aes(x = Year)) +
    # Area fills for positive values (wild animals)
    geom_area(data = combined_data_split_conv %>% filter(Group == "Wild Animals"),
              aes(y = positive_value), 
              alpha = 0.4, fill = main_colors["Wild Animals"]) +
    # Area fills for negative values (wild animals negative and farmed)
    geom_area(data = combined_data_split_conv %>% filter(Group == "Wild Animals Neg"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Wild Animals Neg"]) +
    geom_area(data = combined_data_split_conv %>% filter(Group == "Farmed Animals"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Farmed Animals"]) +
    # Add lines for all groups
    geom_line(data = combined_data_conv,
              aes(y = value, color = Group), 
              size = 1.2) +
    # Add labels at the end of each line
    geom_text(data = combined_data_conv %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(y = value, label = Group, color = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_color_manual(values = main_colors) +
    scale_x_continuous(limits = c(min(combined_data_conv$Year), 
                                  max(combined_data_conv$Year) + 8)) +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Convex: (neuron count ratio)²",
         subtitle = "Increasing returns to scale",
         y = "Welfare Score",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")
  
  # Combine three plots with patchwork
  final_plot <- (p1 | p2 | p3) +
    plot_annotation(
      title = "Welfare Score Analysis: Different Transformations of Neuron Count Ratios",
      subtitle = "Exploring how welfare potential might scale with relative neuron counts"
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "none")
  
  # Save plots
  universal_ggsave(final_plot, "three_panel_nc_func_form_check", output_dir,
                   pdf_width = 20, pdf_height = 7)
  
  # Individual panels
  universal_ggsave(p1, "welfare_score_range_conc_4th_style", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p3, "welfare_score_range_conv_4th_style", output_dir,
                   pdf_width = 8, pdf_height = 6)
  
  # Restore theme
  theme_set(original_theme)
  
  cat("Three-panel welfare score range plots about neuron transformations saved to:", output_dir, "\n")
}


#' Create four-panel human WR_utility with non-human animal score ranges with area 
#' fills, unstacked
#' 
#' @param data extended_integrated_calc_tseries
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
create_four_panel_wr_score_range_plots <- function(data, output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Creating four-panel wr welfare score range plots...\n")
  
  # Ensure WR_utility column exists
  data <- ensure_wr_columns(data)

  max_f_welfare_score <- 100
  max_w_welfare_score <- 0.001
  
  # Prepare human WR_utility data
  human_wr_utility <- data %>%
    filter(Category == "Humans") %>%
    select(Year, WR_utility) %>%
    filter(!is.na(WR_utility))
  
  # Prepare farmed animal WR_apot data (aggregated and inverted)
  max_f_welfare_score <- 100 # due to CE
  farmed_WR_urange <- data %>%
    filter(Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)")) %>%
    group_by(Year) %>%
    summarise(WR_apot_total = sum(WR_apot, na.rm = TRUE), .groups = "drop") %>%
    mutate(WR_urange = max_f_welfare_score*WR_apot_total) %>%  #scale up due to range of welfare score
    mutate(WR_urange = -WR_urange) %>%  # Reflect across x-axis
    filter(!is.na(WR_urange))
  
  # Prepare wild animal WR_apot data (aggregated, scaled down, with BOTH positive and negative)
  max_w_welfare_score <- 0.001
  wild_WR_urange_base <- data %>%
    filter(Group == "Wild Animals") %>%
    group_by(Year) %>%
    summarise(WR_apot_total = sum(WR_apot, na.rm = TRUE), .groups = "drop") %>%
    mutate(WR_urange = max_w_welfare_score*WR_apot_total) %>%  #scale up due to range of welfare score
    filter(!is.na(WR_urange))
  
  # Create both positive and negative versions for wild animals
  wild_WR_urange_full <- bind_rows(
    wild_WR_urange_base %>% mutate(value_type = "positive"),
    wild_WR_urange_base %>% mutate(WR_urange = -WR_urange, value_type = "negative")
  )
  
  # Set theme
  original_theme <- theme_get()
  theme_set(theme_minimal(base_size = 12) +
              theme(
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 11, color = "grey30"),
                strip.text = element_text(face = "bold"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.ticks = element_line(color = "grey70"),
                axis.line = element_line(color = "grey70")
              ))
  
  # Color palette with updated farmed animal color
  main_colors <- c("Humans" = "#2E86AB", "Farmed Animals" = "#20B2AA", "Wild Animals" = "#8B4513")
  
  # Panel 1: Human Welfare Score (line only, no fill)
  p1 <- ggplot(human_wr_utility, aes(x = Year, y = WR_utility)) +
    geom_line(color = main_colors["Humans"], size = 1.2) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Human Welfare Score",
         y = "Welfare Score") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # Panel 2: Farmed Animal Welfare Score (inverted with area)
  p2 <- ggplot(farmed_WR_urange, aes(x = Year, y = WR_urange)) +
    geom_area(alpha = 0.7, fill = main_colors["Farmed Animals"]) +
    geom_line(color = main_colors["Farmed Animals"], size = 1.2) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Farmed Animal Welfare - Possible Range",
         y = "Welfare Score") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # Panel 3: Wild Animal Welfare Score (both positive and negative with areas)
  p3 <- ggplot(wild_WR_urange_full, aes(x = Year, y = WR_urange)) +
    geom_area(data = wild_WR_urange_full %>% filter(value_type == "positive"),
              alpha = 0.7, fill = main_colors["Wild Animals"]) +
    geom_area(data = wild_WR_urange_full %>% filter(value_type == "negative"),
              alpha = 0.7, fill = main_colors["Wild Animals"]) +
    geom_line(data = wild_WR_urange_full %>% filter(value_type == "positive"),
              color = main_colors["Wild Animals"], size = 1.2) +
    geom_line(data = wild_WR_urange_full %>% filter(value_type == "negative"),
              color = main_colors["Wild Animals"], size = 1.2) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Wild Animal Welfare - Possible Range",
         subtitle = paste0("Assuming average welfare score has a magnitude of ", max_w_welfare_score, " out of 100"),
         y = "Welfare Score") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  wild_WR_urange_base_neg <- wild_WR_urange_base %>%
    mutate(WR_urange = -WR_urange)
  
  # Panel 4: Combined comparison - need to combine all three with consistent naming
  combined_data <- bind_rows(
    human_wr_utility %>% 
      rename(value = WR_utility) %>%
      mutate(Group = "Humans"),
    farmed_WR_urange %>% 
      rename(value = WR_urange) %>%
      select(Year, value) %>%
      mutate(Group = "Farmed Animals"),
    wild_WR_urange_base %>%  # Use base (positive) for the combined view
      rename(value = WR_urange) %>%
      select(Year, value) %>%
      mutate(Group = "Wild Animals"),
    wild_WR_urange_base_neg %>%  # Use base (negative) for the combined view
      rename(value = WR_urange) %>%
      select(Year, value) %>%
      mutate(Group = "Wild Animals Neg")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals", "Wild Animals Neg")))
  
  # Create separate positive and negative data for proper area fills
  combined_data_split <- combined_data %>%
    mutate(
      positive_value = ifelse(value > 0, value, 0),
      negative_value = ifelse(value < 0, value, 0)
    )
  
  p4 <- ggplot(combined_data_split, aes(x = Year)) +
    # no fill for human values
    # negative line and fills for wild animals
    geom_area(data = combined_data_split %>% filter(Group == "Wild Animals"),
              aes(y = positive_value), 
              alpha = 0.4, fill = main_colors["Wild Animals"]) +
    geom_area(data = combined_data_split %>% filter(Group == "Wild Animals Neg"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Wild Animals"]) +
    # Add area fills for negative values (farmed)
    geom_area(data = combined_data_split %>% filter(Group == "Farmed Animals"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Farmed Animals"]) +
    # Add lines for all groups
    geom_line(data = combined_data,
              aes(y = value, color = Group), 
              size = 1.2) +
    # Add labels at the end of each line
    geom_text(data = combined_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(y = value, label = Group, color = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_color_manual(values = main_colors) +
    # Extend x-axis to make room for labels
    scale_x_continuous(limits = c(min(combined_data$Year), 
                                  max(combined_data$Year) + 8)) +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Introducing Welfare Score",
         subtitle = "Comparative welfare scores across sentient beings",
         y = "Welfare Score",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")  # Remove legend siwre we have direct labels
  
  # Combine with patchwork
  final_plot <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
      title = "Welfare Score Analysis: Introducing Utility Considerations",
      subtitle = "Exploring welfare scores across humans, farmed animals, and wildlife"
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Save plots
  universal_ggsave(final_plot, "four_panel_wr_welfare_score_range", output_dir,
                   pdf_width = 16, pdf_height = 10)
  
  # Individual panels
  universal_ggsave(p1, "welfare_score_humans_only", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p2, "welfare_score_farmed_inverted", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p3, "welfare_score_wild_scaled_inverted", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p4, "welfare_score_introducing", output_dir,
                   pdf_width = 10, pdf_height = 6)
  
  # Restore theme
  theme_set(original_theme)
  
  cat("Four-panel welfare score range plots saved to:", output_dir, "\n")
  }


#' Create four-panel welfare score plots with CE welfare score estimates
#' 
#' @param data extended_integrated_calc_tseries
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
create_four_panel_score_plots <- function(data, output_dir = "visualisations") {
  
  cat("Creating four-panel welfare score plots...\n")
  
  # Set theme
  original_theme <- theme_get()
  theme_set(theme_minimal(base_size = 12) +
              theme(
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 11, color = "grey30"),
                strip.text = element_text(face = "bold"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.ticks = element_line(color = "grey70"),
                axis.line = element_line(color = "grey70")
              ))
  
  # Color palette with updated farmed animal color
  main_colors <- c("Humans" = "#2E86AB", "Farmed Animals" = "#20B2AA", "Wild Animals" = "#8B4513")
  
  #Prepare farmed animal data - only show farmed fish, farmed chickens, cattle, 
  #swine, and bees separately. group others
  farmed_nc_utility <- data %>% 
    filter(Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)")) %>% 
    select(Year, Category, NC_utility) %>% 
    mutate(
      Year = as.numeric(Year), 
      #Simplify categories
      Category_simplified = case_when(
        Category == "Fish" ~ "Fish", 
        Category == "Chickens" ~ "Chickens", 
        Category == "Cattle" ~ "Cattle",
        Category == "Pigs" ~ "Pigs", 
        Category == "Sheep" ~ "Sheep", 
        TRUE ~ "Other Farmed Animals"
      )
    ) %>% 
    group_by(Year, Category_simplified) %>% 
    summarise(NC_utilty = sum(NC_utility, na.rm = TRUE), .groups = "drop") %>% 
    rename(Category = Category_simplified)
  
  #get the most recent year to determine ordering
  most_recent_year <- max(farmed_nc_utility$Year, na.rm = TRUE)
  
  #calculate ordering based on most recent year's values (largest to smallest)
  category_order <- farmed_nc_utility %>%
    filter(Year == most_recent_year) %>%
    arrange(desc(NC_utility)) %>%
    pull(Category)
  
  #Apply the ordering as a factor
  farmed_nc_utility <- farmed_nc_utility %>% 
    mutate(Category = factor(Category, levels = category_order))
  
  #Prepare wild animal NC_utility data
  wild_nc_utility <- data %>%
    filter(Group == "Wild Animals") %>%
    select(Year, Category, NC_utility) %>%
    filter(!is.na(NC_utility), !is.na(Category)) %>%
    mutate(
      Year = as.numeric(Year),
      # Simplify categories - only show arthropods separately
      Category_simplified = case_when(
        Category == "Wild terrestrial arthropods" ~ "Wild terrestrial arthropods",
        TRUE ~ "Other Wild Animals"
      )
    ) %>%
    group_by(Year, Category_simplified) %>%
    summarise(NC_tot = sum(NC_tot, na.rm = TRUE), .groups = "drop") %>%
    rename(Category = Category_simplified)
  
  
  
  
  
  
  
  
  
  # Panel 1: Human Welfare Score (CE)
  p1 <- ggplot(human_nc_utility, aes(x = Year, y = NC_utility)) +
    geom_line(color = main_colors["Humans"], size = 1.2) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Human Welfare Score",
         y = "Welfare Score") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # Panel 2: Farmed Animal Welfare Score (CE), disaggregated, stacked, simplified categories
  
  
  
  
  # Panel 3: 
  
  
  }


#' Create NC net utility comparison plots with different category exclusions
#' 
#' @param extended_data_for_net Extended dataset covering full time range
#' @param output_dir Directory for saving visualizations
#' @param min_year_constraint Start year for analysis (default: 1960)
#' @param max_year_constraint End year for analysis (default: 2019)
#' @return NULL (saves plots to files)
create_nc_net_utility_comparisons <- function(extended_data_for_net, 
                                              output_dir = "visualizations",
                                              min_year_constraint = 1960,
                                              max_year_constraint = 2019) {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Define wild animal categories
  wild_categories <- c("Wild birds", "Wild terrestrial mammals", "Wild fish", "Wild terrestrial arthropods")
  
  # Filter extended data for NC utility calculations
  extended_nc_data <- extended_data_for_net %>%
    filter(!is.na(NC_utility), !is.na(aliveatanytime))
  
  # Calculate net NC utility with and without humans (all categories included)
  net_nc_comparison_data <- extended_nc_data %>% 
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      with_humans = sum(NC_utility, na.rm = TRUE),
      without_humans = sum(NC_utility[Category != "Humans"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(with_humans, without_humans), 
                 names_to = "Group", 
                 values_to = "NC_utility") %>%
    mutate(Group = case_when(
      Group == "with_humans" ~ "With Humans",
      Group == "without_humans" ~ "Without Humans"
    ))
  
  # Create and save the plot - all categories
  p_nc_comp <- ggplot(net_nc_comparison_data, aes(x = Year, y = NC_utility, color = Group)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = net_nc_comparison_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)),
              aes(label = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min_year_constraint, max_year_constraint + 6)) +
    labs(title = paste0("NC Net Utility Comparison (With vs. Without Humans, ", min_year_constraint, "-", max_year_constraint, ")"), 
         y = "Net Utility", 
         x = "Year") +
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_net_utility_comp.pdf"), 
         plot = p_nc_comp, width = 10, height = 6)
  
  # NC net utility - no wild terrestrial arthropods, no wild fish
  extended_nc_n_wta_wfi <- extended_data_for_net %>%
    filter(!is.na(NC_utility), !is.na(aliveatanytime),
           Category != "Wild terrestrial arthropods",
           Category != "Wild fish")
  
  net_nc_n_wta_wfi_data <- extended_nc_n_wta_wfi %>% 
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      human_utility = sum(NC_utility[Category == "Humans"], na.rm = TRUE),
      non_human_utility = sum(NC_utility[Category != "Humans"], na.rm = TRUE),
      total = sum(NC_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(human_utility, non_human_utility, total), 
                 names_to = "Group", 
                 values_to = "NC_utility") %>%
    mutate(Group = case_when(
      Group == "human_utility" ~ "Human Utility",
      Group == "non_human_utility" ~ "Non-Human Utility",
      Group == "total" ~ "Total"
    ))
  
  p_nc_comp_n_wta_wfi <- ggplot(net_nc_n_wta_wfi_data, aes(x = Year, y = NC_utility, color = Group)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = net_nc_n_wta_wfi_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)),
              aes(label = Group), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min_year_constraint, max_year_constraint + 10)) +
    labs(title = paste0("NC Net Utility Comparison (No wt. arthropods, No w. fish, ", min_year_constraint, "-", max_year_constraint, ")"), 
         y = "Net Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_net_utility_comp_n_wta_wfi.pdf"), 
         plot = p_nc_comp_n_wta_wfi, width = 10, height = 6)
  
  # NC net utility - no wild animals
  extended_nc_nw <- extended_data_for_net %>%
    filter(!is.na(NC_utility), !is.na(aliveatanytime),
           !Category %in% wild_categories)
  
  net_nc_nw_data <- extended_nc_nw %>% 
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      with_humans = sum(NC_utility, na.rm = TRUE),
      without_humans = sum(NC_utility[Category != "Humans"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(with_humans, without_humans), 
                 names_to = "Group", 
                 values_to = "NC_utility") %>%
    mutate(Group = case_when(
      Group == "with_humans" ~ "With Humans",
      Group == "without_humans" ~ "Without Humans"
    ))
  
  p_nc_comp_nw <- ggplot(net_nc_nw_data, aes(x = Year, y = NC_utility, color = Group)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = net_nc_nw_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)),
              aes(label = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min_year_constraint, max_year_constraint + 6)) +
    labs(title = paste0("NC Net Utility Comparison (No Wild Animals, ", min_year_constraint, "-", max_year_constraint, ")"), 
         y = "Net Utility", 
         x = "Year") +
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_net_utility_comp_nw.pdf"), 
         plot = p_nc_comp_nw, width = 10, height = 6)
  
  cat("NC net utility comparison plots saved to:", output_dir, "\n")
}


#' Create WR net utility comparison plots with different category exclusions
#' 
#' @param extended_data_for_net Extended dataset covering full time range
#' @param output_dir Directory for saving visualizations
#' @param min_year_constraint Start year for analysis (default: 1960)
#' @param max_year_constraint End year for analysis (default: 2019)
#' @return NULL (saves plots to files)
create_wr_net_utility_comparisons <- function(extended_data_for_net, 
                                              output_dir = "visualizations",
                                              min_year_constraint = 1960,
                                              max_year_constraint = 2019) {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Define wild animal categories
  wild_categories <- c("Wild birds", "Wild terrestrial mammals", "Wild fish", "Wild terrestrial arthropods")
  
  # Filter extended data for WR utility calculations
  extended_wr_data <- extended_data_for_net %>%
    filter(!is.na(WR_utility), !is.na(aliveatanytime))
  
  # Calculate net WR utility with and without humans (all categories included)
  net_wr_comparison_data <- extended_wr_data %>% 
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      with_humans = sum(WR_utility, na.rm = TRUE),
      without_humans = sum(WR_utility[Category != "Humans"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(with_humans, without_humans), 
                 names_to = "Group", 
                 values_to = "WR_utility") %>%
    mutate(Group = case_when(
      Group == "with_humans" ~ "With Humans",
      Group == "without_humans" ~ "Without Humans"
    ))
  
  # Create and save the plot - all categories
  p_wr_comp <- ggplot(net_wr_comparison_data, aes(x = Year, y = WR_utility, color = Group)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = net_wr_comparison_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)),
              aes(label = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min_year_constraint, max_year_constraint + 6)) +
    labs(title = paste0("WR Net Utility Comparison (With vs. Without Humans, ", min_year_constraint, "-", max_year_constraint, ")"), 
         y = "Net Utility", 
         x = "Year") +
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_net_utility_comp.pdf"), 
         plot = p_wr_comp, width = 10, height = 6)
  
  # WR net utility - no wild terrestrial arthropods, no wild fish
  extended_wr_n_wta_wfi <- extended_data_for_net %>%
    filter(!is.na(WR_utility), !is.na(aliveatanytime),
           Category != "Wild terrestrial arthropods",
           Category != "Wild fish")
  
  net_wr_n_wta_wfi_data <- extended_wr_n_wta_wfi %>% 
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      human_utility = sum(WR_utility[Category == "Humans"], na.rm = TRUE),
      non_human_utility = sum(WR_utility[Category != "Humans"], na.rm = TRUE),
      total = sum(WR_utility, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(human_utility, non_human_utility, total), 
                 names_to = "Group", 
                 values_to = "WR_utility") %>%
    mutate(Group = case_when(
      Group == "human_utility" ~ "Human Utility",
      Group == "non_human_utility" ~ "Non-Human Utility",
      Group == "total" ~ "Total"
    ))
  
  p_wr_comp_n_wta_wfi <- ggplot(net_wr_n_wta_wfi_data, aes(x = Year, y = WR_utility, color = Group)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = net_wr_n_wta_wfi_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)),
              aes(label = Group), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min_year_constraint, max_year_constraint + 10)) +
    labs(title = paste0("WR Net Utility Comparison (No wt. arthropods, No w. fish, ", min_year_constraint, "-", max_year_constraint, ")"), 
         y = "Net Utility", 
         x = "Year") +
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_net_utility_comp_n_wta_wfi.pdf"), 
         plot = p_wr_comp_n_wta_wfi, width = 10, height = 6)
  
  # WR net utility - no wild terrestrial arthropods, no wild fish, no bees
  extended_wr_n_wta_wfi_fbe <- extended_data_for_net %>%
    filter(!is.na(WR_utility), !is.na(aliveatanytime),
           Category != "Wild terrestrial arthropods",
           Category != "Wild fish",
           Category != "Bees")
  
  net_wr_n_wta_wfi_fbe_data <- extended_wr_n_wta_wfi_fbe %>% 
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      with_humans = sum(WR_utility, na.rm = TRUE),
      without_humans = sum(WR_utility[Category != "Humans"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(with_humans, without_humans), 
                 names_to = "Group", 
                 values_to = "WR_utility") %>%
    mutate(Group = case_when(
      Group == "with_humans" ~ "With Humans",
      Group == "without_humans" ~ "Without Humans"
    ))
  
  p_wr_comp_n_wta_wfi_fbe <- ggplot(net_wr_n_wta_wfi_fbe_data, aes(x = Year, y = WR_utility, color = Group)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = net_wr_n_wta_wfi_fbe_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)),
              aes(label = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min_year_constraint, max_year_constraint + 6)) +
    labs(title = paste0("WR Net Utility Comparison (No wt. arthropods, No w. fish, No bees, ", min_year_constraint, "-", max_year_constraint, ")"), 
         y = "Net Utility", 
         x = "Year") +
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_net_utility_comp_n_wta_wfi_fbe.pdf"), 
         plot = p_wr_comp_n_wta_wfi_fbe, width = 10, height = 6)
  
  # WR net utility - no wild animals
  extended_wr_nw <- extended_data_for_net %>%
    filter(!is.na(WR_utility), !is.na(aliveatanytime),
           !Category %in% wild_categories)
  
  net_wr_nw_data <- extended_wr_nw %>% 
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      with_humans = sum(WR_utility, na.rm = TRUE),
      without_humans = sum(WR_utility[Category != "Humans"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(with_humans, without_humans), 
                 names_to = "Group", 
                 values_to = "WR_utility") %>%
    mutate(Group = case_when(
      Group == "with_humans" ~ "With Humans",
      Group == "without_humans" ~ "Without Humans"
    ))
  
  p_wr_comp_nw <- ggplot(net_wr_nw_data, aes(x = Year, y = WR_utility, color = Group)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = net_wr_nw_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)),
              aes(label = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min_year_constraint, max_year_constraint + 6)) +
    labs(title = paste0("WR Net Utility Comparison (No Wild Animals, ", min_year_constraint, "-", max_year_constraint, ")"), 
         y = "Net Utility", 
         x = "Year") +
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_net_utility_comp_nw.pdf"), 
         plot = p_wr_comp_nw, width = 10, height = 6)
  
  cat("WR net utility comparison plots saved to:", output_dir, "\n")
}

#' Create NC net total neurons comparison plots
#' 
#' @param extended_data_for_net Extended dataset covering full time range
#' @param output_dir Directory for saving visualizations
#' @param min_year_constraint Start year for analysis (default: 1960)
#' @param max_year_constraint End year for analysis (default: 2019)
#' @return NULL (saves plots to files)
create_nc_net_tot_series <- function(extended_data_for_net, 
                                     output_dir = "visualizations",
                                     min_year_constraint = 1960,
                                     max_year_constraint = 2019) {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Filter extended data for NC_tot calculations
  extended_nc_tot_data <- extended_data_for_net %>%
    filter(!is.na(NC_tot), !is.na(aliveatanytime))
  
  # Calculate net NC_tot with and without humans (all categories included)
  net_nc_tot_comparison_data <- extended_nc_tot_data %>% 
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      with_humans = sum(NC_tot, na.rm = TRUE),
      without_humans = sum(NC_tot[Category != "Humans"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(with_humans, without_humans), 
                 names_to = "Group", 
                 values_to = "NC_tot") %>%
    mutate(Group = case_when(
      Group == "with_humans" ~ "With Humans",
      Group == "without_humans" ~ "Without Humans"
    ))
  
  # Create and save the plot - all categories
  p_nc_tot_comp <- ggplot(net_nc_tot_comparison_data, aes(x = Year, y = NC_tot, color = Group)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = net_nc_tot_comparison_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)),
              aes(label = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min_year_constraint, max_year_constraint + 6)) +
    labs(title = paste0("Net Total Neurons Comparison (With vs. Without Humans, ", min_year_constraint, "-", max_year_constraint, ")"), 
         y = "Net Total Neurons", 
         x = "Year") +
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_net_tot_trends.pdf"), 
         plot = p_nc_tot_comp, width = 10, height = 6)
  
  # No wild terrestrial arthropods, no wild fish - NET SERIES with extended data
  extended_nc_tot_n_wta_wfi <- extended_data_for_net %>%
    filter(!is.na(NC_tot), !is.na(aliveatanytime),
           Category != "Wild terrestrial arthropods",
           Category != "Wild fish")
  
  # Calculate net NC_tot with and without humans for filtered categories
  net_nc_tot_n_wta_wfi_data <- extended_nc_tot_n_wta_wfi %>% 
    filter(Year >= min_year_constraint, 
           Year <= max_year_constraint) %>%
    group_by(Year) %>%
    summarize(
      with_humans = sum(NC_tot, na.rm = TRUE),
      without_humans = sum(NC_tot[Category != "Humans"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(with_humans, without_humans), 
                 names_to = "Group", 
                 values_to = "NC_tot") %>%
    mutate(Group = case_when(
      Group == "with_humans" ~ "With Humans",
      Group == "without_humans" ~ "Without Humans"
    ))
  
  # Create and save the net series plot - filtered categories
  p_nc_tot_n_wta_wfi_net <- ggplot(net_nc_tot_n_wta_wfi_data, aes(x = Year, y = NC_tot, color = Group)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = net_nc_tot_n_wta_wfi_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)),
              aes(label = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min_year_constraint, max_year_constraint + 6)) +
    labs(title = paste0("Net Total Neurons Comparison (No wt. arthropods, No w. fish, ", min_year_constraint, "-", max_year_constraint, ")"), 
         y = "Net Total Neurons", 
         x = "Year") +
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_net_tot_trends_n_wta_wfi.pdf"), 
         plot = p_nc_tot_n_wta_wfi_net, width = 10, height = 6)
  
  cat("NC net total neurons series plots saved to:", output_dir, "\n")
}

#' Create disaggregated plots with total lines for NC_tot, NC_utility, and WR_utility
#' 
#' @param data The processed dataset
#' @param output_dir Directory for saving visualizations
#' @param min_total_year Start year for total line calculation (default: 1990)
#' @param max_total_year End year for total line calculation (default: 2017)
#' @return NULL (saves plots to files)
create_disaggregated_plots_with_totals <- function(data, 
                                                   output_dir = "visualizations",
                                                   min_total_year = 1990,
                                                   max_total_year = 2017) {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Filter out rows with NA values for each metric
  filtered_data_nc_tot <- data %>%
    filter(!is.na(NC_tot), !is.na(aliveatanytime))
  
  filtered_data_nc <- data %>%
    filter(!is.na(NC_utility), !is.na(aliveatanytime))
  
  filtered_data_wr <- data %>%
    filter(!is.na(WR_utility), !is.na(aliveatanytime))
  
  # NC_tot over time - all categories (basic plot without total line)
  p_nc_tot <- ggplot(filtered_data_nc_tot, aes(x = Year, y = NC_tot, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    geom_text(data = filtered_data_nc_tot %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    scale_x_continuous(limits = c(min(filtered_data_nc_tot$Year), 
                                  max(filtered_data_nc_tot$Year) + 30)) +
    labs(title = "Total Neurons Over Time by Category (1950-2025)", 
         y = "Total Neurons", 
         x = "Year") +
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_tot_trends.pdf"), 
         plot = p_nc_tot, width = 10, height = 6)
  
  # NC_tot: No wild terrestrial arthropods, no wild fish - individual categories with total line
  filtered_nc_tot_n_wta_wfi <- filtered_data_nc_tot %>% 
    filter(Category != "Wild terrestrial arthropods",
           Category != "Wild fish")
  
  # Calculate total across all displayed categories for specified time range
  # time constraint is due to the total line needing to come from only available data
  total_nc_tot <- filtered_nc_tot_n_wta_wfi %>%
    filter(Year >= min_total_year, Year <= max_total_year) %>%
    group_by(Year) %>%
    summarize(NC_tot = sum(NC_tot, na.rm = TRUE), .groups = "drop") %>%
    mutate(Category = "Total", Group = "Total")
  
  # Combine individual categories with total
  plot_data_with_total <- bind_rows(filtered_nc_tot_n_wta_wfi, total_nc_tot)
  
  p_nc_tot_n_wta_wfi <- ggplot(plot_data_with_total, aes(x = Year, y = NC_tot, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    # Add labels at the end of each line
    geom_text(data = plot_data_with_total %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    # Extend x-axis to make room for labels
    scale_x_continuous(limits = c(min(plot_data_with_total$Year), 
                                  max(plot_data_with_total$Year) + 30)) +
    labs(title = "Total Neurons Over Time (No wt. arthropods, No w. fish)", 
         y = "Total Neurons", 
         x = "Year") +
    # Remove the legend since we have direct labels
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_tot_trends_n_wta_wfi.pdf"), 
         plot = p_nc_tot_n_wta_wfi, width = 10, height = 6)
  
  # NC utility - No wild terrestrial arthropods, no wild fish - individual categories with total line
  filtered_nc_utility_n_wta_wfi <- filtered_data_nc %>% 
    filter(Category != "Wild terrestrial arthropods",
           Category != "Wild fish")
  
  # Calculate total across all displayed categories for specified time range
  # time constraint is due to the total line needing to come from only available data
  total_nc_utility <- filtered_nc_utility_n_wta_wfi %>%
    filter(Year >= min_total_year, Year <= max_total_year) %>%
    group_by(Year) %>%
    summarize(NC_utility = sum(NC_utility, na.rm = TRUE), .groups = "drop") %>%
    mutate(Category = "Total", Group = "Total")
  
  # Combine individual categories with total
  plot_data_nc_utility_with_total <- bind_rows(filtered_nc_utility_n_wta_wfi, total_nc_utility)
  
  p_nc_utility_n_wta_wfi <- ggplot(plot_data_nc_utility_with_total, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    # Add labels at the end of each line
    geom_text(data = plot_data_nc_utility_with_total %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    # Extend x-axis to make room for labels
    scale_x_continuous(limits = c(min(plot_data_nc_utility_with_total$Year), 
                                  max(plot_data_nc_utility_with_total$Year) + 30)) +
    labs(title = "Utility Over Time - NC Method (No wt. arthropods, No w. fish)", 
         y = "NC Utility", 
         x = "Year") +
    # Remove the legend since we have direct labels
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "NC_utility_trends_n_wta_wfi_with_total.pdf"), 
         plot = p_nc_utility_n_wta_wfi, width = 10, height = 6)
  
  # WR utility - No wild terrestrial arthropods, no wild fish - individual categories with total line
  filtered_wr_utility_n_wta_wfi <- filtered_data_wr %>% 
    filter(Category != "Wild terrestrial arthropods",
           Category != "Wild fish")
  
  # Calculate total across all displayed categories for specified time range
  # time constraint is due to the total line needing to come from only available data
  total_wr_utility <- filtered_wr_utility_n_wta_wfi %>%
    filter(Year >= min_total_year, Year <= max_total_year) %>%
    group_by(Year) %>%
    summarize(WR_utility = sum(WR_utility, na.rm = TRUE), .groups = "drop") %>%
    mutate(Category = "Total", Group = "Total")
  
  # Combine individual categories with total
  plot_data_wr_utility_with_total <- bind_rows(filtered_wr_utility_n_wta_wfi, total_wr_utility)
  
  p_wr_utility_n_wta_wfi <- ggplot(plot_data_wr_utility_with_total, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    # Add labels at the end of each line
    geom_text(data = plot_data_wr_utility_with_total %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    # Extend x-axis to make room for labels
    scale_x_continuous(limits = c(min(plot_data_wr_utility_with_total$Year), 
                                  max(plot_data_wr_utility_with_total$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish)", 
         y = "WR Utility", 
         x = "Year") +
    # Remove the legend since we have direct labels
     theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_with_total.pdf"), 
         plot = p_wr_utility_n_wta_wfi, width = 10, height = 6)
  
  # Calculate total without bees across all displayed categories for specified time range
  total_wr_utility_no_bees <- filtered_wr_utility_n_wta_wfi %>%
    filter(Year >= min_total_year, Year <= max_total_year,
           Category != "Bees") %>%
    group_by(Year) %>%
    summarize(WR_utility = sum(WR_utility, na.rm = TRUE), .groups = "drop") %>%
    mutate(Category = "Total without bees", Group = "Total")
  
  # Combine individual categories with both totals
  plot_data_wr_utility_with_totals <- bind_rows(filtered_wr_utility_n_wta_wfi, total_wr_utility, total_wr_utility_no_bees)
  
  p_wr_utility_n_wta_wfi_totals <- ggplot(plot_data_wr_utility_with_totals, aes(x = Year, y = WR_utility, colour = Category, group = interaction(Group, Category))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 1) +
    # Add labels at the end of each line
    geom_text(data = plot_data_wr_utility_with_totals %>% 
                group_by(Category, Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(label = Category), 
              hjust = -0.1, 
              size = 6, 
              check_overlap = TRUE) +
    # Extend x-axis to make room for labels
    scale_x_continuous(limits = c(min(plot_data_wr_utility_with_totals$Year), 
                                  max(plot_data_wr_utility_with_totals$Year) + 30)) +
    labs(title = "Utility Over Time - WR Method (No wt. arthropods, No w. fish)", 
         y = "WR Utility", 
         x = "Year") +
    # Remove the legend since we have direct labels
    theme_minimal() +   theme(     plot.title = element_text(size = 22, face = "bold"),     axis.title.x = element_text(size = 24),     axis.title.y = element_text(size = 24),      axis.text.x = element_text(size = 10),     axis.text.y = element_text(size = 10)   ) +
    theme(legend.position = "none")
  
  ggsave(file.path(output_dir, "WR_utility_trends_n_wta_wfi_with_totals.pdf"), 
         plot = p_wr_utility_n_wta_wfi_totals, width = 10, height = 6)
  
  cat("Disaggregated plots with totals saved to:", output_dir, "\n")
}

#' Create time series with total lines for human population and disaggregated n_wta_wfi plots
#' 
#' @param data The processed dataset
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
create_mixed_series <- function(data, output_dir = "visualisations") {
  
}



