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
  
  #Prepare welfare score range data
  ws_range_data <- data %>% 
    mutate(WS_range <- NC_potential * )
  
  
}






