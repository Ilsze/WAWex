#Install necessary packages
# install.packages(c("tidyverse", "dplyr", "readr", "ggplot2", "gridExtra", 
#                    "png", "mgcv", "tidyselect", "stringr", "readxl", 
#                    "openxlsx", "foreign", "broom", "knitr", "data.table", "dlm"))

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm, 
       patchwork, hrbrthemes, scales)

# Source the integration script (which sources the other scripts)
source("fifth_pass/integration.R")
source("fifth_pass/welfare_analysis_framework.R")
source("fifth_pass/create_utility_visualizations.R")

# ==============================================================================
# FLEXIBLE PRESENTATION CONFIGURATION
# ==============================================================================

presentation_config <- list(
  create_presentation_images = TRUE,
  image_format = "png",
  image_width = 12,      # Keep reasonable size
  image_height = 8,      # Keep reasonable size
  image_dpi = 300,       # Keep high DPI for crisp text
  
  # OPTION 1: Specific files (your current need)
  specific_files = list(
    "NC_net_tot_trends.pdf" = "A1",
    "NC_tot_trends.pdf" = "A2", 
    "NC_tot_trends_n_wta_wfi.pdf" = "A3",
    "NC_net_tot_trends_n_wta_wfi.pdf" = "A4",
    "NC_net_utility_comp.pdf" = "B1",
    "NC_utility_trends.pdf" = "B2",
    "NC_utility_trends_n_wta_wfi_with_total.pdf" = "B3",
    "NC_net_utility_comp_n_wta_wfi.pdf" = "B4",
    "NC_net_utility_comp_nw.pdf" = "B5",
    "WR_net_utility_comp.pdf" = "C1",
    "WR_utility_trends.pdf" = "C2",
    "WR_utility_trends_n_wta_wfi_with_totals.pdf" = "C3",
    "WR_net_utility_comp_n_wta_wfi.pdf" = "C4",
    "WR_net_utility_comp_n_wta_wfi_fbe.pdf" = "C5",
    "WR_net_utility_comp_nw.pdf" = "C6",
    "WR_utility_trends_n_wta_wfi_fbe.pdf" = "D1",
    "WR_utility_trends_n_wta_wfi_fbe_ffi_fch_wtm_hum.pdf" = "D2"
  )
  
  # OPTION 2: Pattern-based (for future flexibility)
  # filename_patterns = list(
  #   list(regex = "^NC_.*utility.*", prefix = "NC_UTIL"),
  #   list(regex = "^WR_.*utility.*", prefix = "WR_UTIL"),
  #   list(regex = ".*_trends$", prefix = "TREND")
  # )
  
  # OPTION 3: Save all plots as presentation images
  # save_all = TRUE
)

# ==============================================================================
# GGSAVE OVERRIDE
# ==============================================================================

# Override the default ggsave function globally
if(!is.null(presentation_config) && presentation_config$create_presentation_images) {
  
  # Create new ggsave function that automatically handles presentation images
  ggsave <- function(filename, plot = last_plot(), device = NULL, path = NULL, 
                     scale = 1, width = NA, height = NA, units = c("in", "cm", "mm", "px"),
                     dpi = 300, limitsize = TRUE, bg = NULL, ...) {
    
    # Call the ORIGINAL ggplot2 ggsave using namespace qualification to avoid recursion
    result <- ggplot2:::ggsave(filename = filename, plot = plot, device = device, 
                               path = path, scale = scale, width = width, height = height, 
                               units = units, dpi = dpi, limitsize = limitsize, bg = bg, ...)
    
    # Extract directory and filename info
    if(!is.null(path)) {
      output_dir <- path
    } else {
      output_dir <- dirname(filename)
      if(output_dir == ".") output_dir <- getwd()
    }
    
    base_filename <- tools::file_path_sans_ext(basename(filename))
    pdf_filename <- paste0(base_filename, ".pdf")
    
    # Check if this file should get presentation image treatment
    should_save <- FALSE
    prefix <- ""
    
    # Method 1: Specific filename mapping
    if(!is.null(presentation_config$specific_files) && 
       pdf_filename %in% names(presentation_config$specific_files)) {
      should_save <- TRUE
      prefix <- paste0(presentation_config$specific_files[[pdf_filename]], "_")
    }
    
    # Method 2: Pattern matching
    if(!is.null(presentation_config$filename_patterns)) {
      for(pattern in presentation_config$filename_patterns) {
        if(grepl(pattern$regex, base_filename)) {
          should_save <- TRUE
          if(!is.null(pattern$prefix)) {
            prefix <- paste0(pattern$prefix, "_")
          }
          break
        }
      }
    }
    
    # Method 3: Save all
    if(is.null(presentation_config$specific_files) && 
       is.null(presentation_config$filename_patterns) &&
       isTRUE(presentation_config$save_all)) {
      should_save <- TRUE
    }
    
    # Create presentation image if needed
    if(should_save) {
      # Create presentation directory
      presentation_dir <- file.path(output_dir, "presentation_images")
      if(!dir.exists(presentation_dir)) {
        dir.create(presentation_dir, recursive = TRUE)
      }
      
      # Save presentation image using the ORIGINAL ggplot2 ggsave (no recursion!)
      image_ext <- presentation_config$image_format %||% "png"
      output_filename <- paste0(prefix, base_filename, ".", image_ext)
      output_path <- file.path(presentation_dir, output_filename)
      
      ggplot2:::ggsave(
        filename = output_path,
        plot = plot,
        width = presentation_config$image_width %||% 12,
        height = presentation_config$image_height %||% 8,
        dpi = presentation_config$image_dpi %||% 300,
        device = image_ext
      )
      
      cat("Presentation image saved:", output_filename, "\n")
    }
    
    return(result)
  }
}

# ==============================================================================
# RUN ANALYSIS 
# ==============================================================================

all_results <- run_all_welfare_method_combinations(
  human_data_path = "dat/world_bank/world_bank_pop_gdp_clean.xlsx",
  farmed_animal_data_path = "fifth_pass/calc_tseries.xlsx",
  wild_animal_data_path = "fifth_pass/wild_calc_tseries.xlsx",
  output_base_dir = "fifth_pass/welfare_results",
  create_visualizations = TRUE
)

# Restore original ggsave if needed
if(exists("original_ggsave")) {
  ggsave <- original_ggsave
  rm(original_ggsave)
}





# ==============================================================================
# FINDING MY HEAD 
# ==============================================================================
# human_dat = read_xlsx("dat/world_bank/world_bank_pop_gdp_clean.xlsx")
# farmed_dat = read_xlsx("fifth_pass/calc_tseries.xlsx")
# wild_dat = read_xlsx("fifth_pass/wild_calc_tseries.xlsx")
# integrated_dat = read_xlsx("fifth_pass/welfare_results/3282/integrated_calc_tseries.xlsx")
# extended_integrated_dat <- read_xlsx("fifth_pass/dat/extended_integrated_calc_tseries.xlsx")


