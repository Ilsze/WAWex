#this file views and manipulates datras fish data, and also shetlandfishermen fish data:https://www.shetlandfishermen.com/site/assets/files/2370/sfa_briefing_note_sea_fish.pdf
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, stringr) 


#-----------------------------------------------
# 1. DATA LOADING AND INITIAL EXPLORATION
#-----------------------------------------------
fish65 <- read_csv("dat/datras/CPUE per length per haul per hour_2025-05-05 14_54_30.csv")
fish6667 <- read_csv("dat/datras/CPUE per length per haul per hour_2025-05-05 14_55_03.csv")
fish6871 <- read_csv("dat/datras/CPUE per length per haul per hour_2025-05-05 14_55_55.csv")
fish7279 <- read_csv("dat/datras/CPUE per length per haul per hour_2025-05-05 14_56_45.csv")
fish8095 <- read_csv("dat/datras/CPUE per length per haul per hour_2025-05-05 14_58_15.csv")
fish9609 <- read_csv("dat/datras/CPUE per length per haul per hour_2025-05-05 15_08_19.csv")
fish1025 <- read_csv("dat/datras/CPUE per length per haul per hour_2025-05-05 15_10_00.csv")

# Combine all data files
fish_dat <- rbind(fish65, fish6667, fish6871, fish7279, fish8095, fish9609, fish1025) %>% 
  # Remove constant variables (those with only 1 unique value)
  select(-Survey, -Quarter, -Sex, -SweptArea_km2, -CPUE_number_per_km2, 
         -DateofCalculation, -DateTime, -AphiaID) %>% #these three are determined by other variables, 
  # Also check for and remove any columns with all NA values
  select_if(~!all(is.na(.)))

write_csv(fish_dat, "dat/datras/CPUE_clean.csv")


#-----------------------------------------------
# 2. ANALYZE SAMPLING CONSISTENCY OVER TIME
#-----------------------------------------------

# Check which variables show systematic changes over time
sampling_changes <- fish_dat %>%
  # Group by year to see how sampling changed
  group_by(Year) %>%
  summarize(
    n_species = n_distinct(Species),
    n_areas = n_distinct(Area),
    n_gears = n_distinct(Gear),
    n_depths = n_distinct(Depth),
    n_countries = n_distinct(Country),
    n_daynight_both = n_distinct(DayNight),
    n_samples = n(), # Total number of records per year
    # Calculate average depth to see if depth range changed
    avg_depth = mean(Depth, na.rm = TRUE),
    min_depth = min(Depth, na.rm = TRUE),
    max_depth = max(Depth, na.rm = TRUE),
    # Look at day/night proportion
    prop_day = mean(DayNight == "D", na.rm = TRUE),
    .groups = "drop"
  )

# Save this analysis
write_csv(sampling_changes, "dat/datras/sampling_changes_by_year.csv")

# Create depth bins based on quartiles for better grouping
depth_breaks <- quantile(fish_dat$Depth, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
depth_breaks[1] <- 0  # Ensure starts at 0
depth_labels <- paste0("Depth_", 1:4)

#-----------------------------------------------
# 3. PREPARE DATA FOR LPI ANALYSIS
#-----------------------------------------------

# Step 1: First, explicitly sum across length classes for each sampling event
length_aggregated <- fish_dat %>%
  # Create depth bins
  mutate(
    Depth_bin = cut(Depth, breaks = depth_breaks, labels = depth_labels, include.lowest = TRUE)
  ) %>%
  # Group by all variables EXCEPT LngtClass that define a unique sampling event
  group_by(Species, Year, Country, Area, Gear, DayNight, Depth_bin, Ship, HaulNo, HaulDur, ShootLat, ShootLong, SubArea) %>%
  # Sum CPUE across all length classes within each sampling event
  summarize(
    total_cpue = sum(CPUE_number_per_hour, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Create standardized sampling units by averaging over lower-level variables
lpi_prep <- length_aggregated %>%
  # Group by our core high-level variables (averaging over SubArea and other sampling details)
  group_by(Species, Year, Country, Area, Gear, DayNight, Depth_bin) %>%
  summarize(
    # Average across sampling events within each standardized unit
    mean_cpue = mean(total_cpue, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  # Keep only observations with positive CPUE
  filter(mean_cpue > 0)

# Step 3: Check temporal coverage for each standardized unit
unit_coverage <- lpi_prep %>%
  group_by(Species, Country, Area, Gear, DayNight, Depth_bin) %>%
  summarize(
    n_years = n_distinct(Year),
    first_year = min(Year),
    last_year = max(Year),
    year_span = last_year - first_year,
    total_samples = sum(n_samples),
    .groups = "drop"
  )

# Create year-population presence matrix (same as before)
presence_matrix <- lpi_prep %>%
  unite("pop_id", c(Species, Country, Area, Gear, DayNight, Depth_bin), sep = "_", remove = FALSE) %>%
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = pop_id,
    names_from = Year,
    values_from = present,
    values_fill = 0
  )

# Get all years
all_years <- colnames(presence_matrix)[!colnames(presence_matrix) %in% "pop_id"]
all_years <- as.numeric(all_years)

# Set minimum required time span (adjust as needed)
min_years_required <- 15

# Try different numbers of years (starting from minimum required)
results <- data.frame(
  n_years = integer(),
  years_selected = character(),
  n_populations = integer()
)

# Try keeping the top N years by population coverage
for(n_years in min_years_required:length(all_years)) {
  # Get population counts for each year
  year_pop_counts <- colSums(presence_matrix[, as.character(all_years)])
  
  # Select top n_years by population count
  top_years <- names(sort(year_pop_counts, decreasing = TRUE)[1:n_years])
  
  # Count populations with data for all selected years
  complete_pops <- presence_matrix %>%
    mutate(coverage = rowSums(across(all_of(top_years)))) %>%
    filter(coverage == n_years) %>%
    nrow()
  
  # Store result
  results <- rbind(results, data.frame(
    n_years = n_years,
    years_selected = paste(top_years, collapse = ","),
    n_populations = complete_pops
  ))
}

# Find the option with maximum populations
best_option <- results %>%
  filter(n_populations == max(n_populations))

# If there are ties, choose the one with fewer years
if(nrow(best_option) > 1) {
  best_option <- best_option %>%
    filter(n_years == min(n_years))
}

cat("Best option:", best_option$n_years, "years with", best_option$n_populations, "populations\n")

# Get selected years as a vector
selected_years <- as.numeric(strsplit(best_option$years_selected, ",")[[1]])

# Get populations with complete data
final_pops <- presence_matrix %>%
  mutate(coverage = rowSums(across(all_of(as.character(selected_years))))) %>%
  filter(coverage == length(selected_years)) %>%
  pull(pop_id)

# Create balanced dataset
balanced_lpi <- lpi_prep %>%
  unite("pop_id", c(Species, Country, Area, Gear, DayNight, Depth_bin), sep = "_", remove = FALSE) %>%
  filter(
    pop_id %in% final_pops,
    Year %in% selected_years
  ) %>%
  mutate(
    Binomial = gsub(" ", "_", Species),
    ID = gsub(" ", "_", pop_id),
    year = Year,
    popvalue = mean_cpue
  ) %>%
  select(Binomial, ID, year, popvalue)

# Create a boxplot of popvalue by year to see the distribution over time
ggplot(balanced_lpi, aes(x = factor(year), y = popvalue)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_y_log10() +
  labs(
    title = "Distribution of CPUE Values by Year",
    subtitle = "Log scale, all populations with ≥30 years of data",
    x = "Year",
    y = "CPUE (number per hour) - Log10 Scale"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7)
  )
ggsave("dat/datras/popvalue_by_year.pdf", width = 12, height = 6)
ggsave("dat/datras/popvalue_by_year.png", width = 12, height = 6, dpi = 300)

########## OLD BUT FUNCTIONAL ALTERNATIVE: USED TO RUN FROM LINE 104 ONWARDS
# # Focus on the most reliable long-term trends
# # Keep only units with at least 30 years of data
# good_units <- unit_coverage %>%
#   filter(n_years >= 30)
# 
# # Print summary of coverage
# cat("From", nrow(unit_coverage), "total units,", nrow(good_units), 
#     "have long-term temporal coverage (≥30 years)\n")
# 
# # Optional: Save the list of units that meet this criterion
# write.csv(good_units, "dat/datras/reliable_units_30yr.csv", row.names = FALSE)
# 
# # Filter to keep only the good units
# lpi_formatted <- lpi_prep %>%
#   semi_join(good_units, by = c("Species", "Country", "Area", "Gear", "DayNight", "Depth_bin")) %>%
#   # Rename for LPI format and fix formatting issues
#   mutate(
#     # Create population ID - replace spaces with underscores
#     Species_fixed = gsub(" ", "_", Species),  # Replace spaces with underscores in species names
#     ID = paste(Species_fixed, Country, Area, Gear, DayNight, Depth_bin, sep = "_"),
#     # Replace any remaining spaces in ID with underscores
#     ID = gsub(" ", "_", ID),
#     # Rename columns for LPI format
#     Binomial = Species_fixed,  # Use the underscore version of species name
#     year = Year,
#     popvalue = mean_cpue
#   ) %>%
#   # Select only columns needed for LPI
#   select(Binomial, ID, year, popvalue)
# 
# # Save formatted data
# write_csv(lpi_formatted, "dat/datras/lpi_formatted_data.csv")
# 
# # Create the tab-delimited version for LPI
# write.table(lpi_formatted, "dat/datras/lpi_formatted_data.txt", 
#             sep = "\t", row.names = FALSE, quote = FALSE)
# 
# # Create the infile for LPI
# infile <- data.frame(
#   FileName = "dat/datras/lpi_formatted_data.txt",
#   Group = 1,
#   Weighting = 1
# )
# 
# write.table(infile, "dat/datras/lpi_infile.txt", 
#             sep = "\t", row.names = FALSE, quote = FALSE)
# 
# # Print summary statistics
# cat("LPI dataset created with", nrow(lpi_formatted), "records\n")
# cat("Covering", length(unique(lpi_formatted$Binomial)), "species\n")
# cat("With", length(unique(lpi_formatted$ID)), "populations\n")
# cat("Over years", min(lpi_formatted$year), "to", max(lpi_formatted$year), "\n")
# 
# #-----------------------------------------------
# # 4. RUN LPI ANALYSIS
# #-----------------------------------------------
# 
# # Source the rlpi package
# # Make sure the rlpi package is installed - if not, uncomment and run:
# # library(devtools)
# # install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)
# 
# library(rlpi)
# 
# # Run the LPI analysis with the correct parameter names
# fish_lpi <- LPIMain(
#   infile = "dat/datras/lpi_infile.txt",
#   VERBOSE = FALSE,            # Set to TRUE for detailed output
#   SHOW_PROGRESS = TRUE,       # Shows progress during calculation (note the all-caps)
#   BOOT_STRAP_SIZE = 1000,     # Number of bootstrap replicates (note the different parameter name)
#   PLOT_MAX = 2025             # Maximum year for plotting
# )
# 
# # Remove any NA rows (typically at the end of the time series)
# fish_lpi_clean <- fish_lpi[complete.cases(fish_lpi), ]
# 
# # Save the LPI results
# write.csv(fish_lpi_clean, "dat/datras/fish_lpi_results.csv", row.names = FALSE)