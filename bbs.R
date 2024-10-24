#this file views and manipulates bbs bird data, to be combined with population abundance data
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, stringr) 

#read in al the datasets in bbs
# Set the path to your folder containing datasets
file_path <- "./data/bbs"
# Get a list of all files in the folder
file_list <- list.files(path = file_path, full.names = TRUE)
# Create an empty list to store the datasets
dat_list <- list()
# Loop through each file and read it into the list
for (dat in file_list) {
  # Get the file name without extension
  dat_name <- tools::file_path_sans_ext(basename(dat))
  # Read the file (adjust the read function based on your file type)
  dat_list[[dat_name]] <- read.csv(dat)  # For CSV files
  # For other file types, use appropriate functions like read.table, readRDS, etc.
}
# Now 'dat_list' is a list containing all your datasets
# Access individual datasets using their names, e.g., dat_list$NDakota

## Add a column to each dataset with the natural language of the state name
# Loop through each dataset in the list
for (dataset_name in names(dat_list)) {
  # Add a new column 'State' with the dataset name
  dat_list[[dataset_name]]$State <- dataset_name
}

#bind datasets together
raw <- bind_rows(dat_list)
names(raw)

#check extant
extinct_species <- raw %>%
  # Filter for the year 2023
  filter(Year == 2023) %>%
  # Group by species
  group_by(AOU) %>%
  # Sum the SpeciesTotal for each species
  summarise(total_count = sum(SpeciesTotal, na.rm = TRUE)) %>%
  # Filter for species with zero total count
  filter(total_count == 0)

# Check if there are any extinct species
if (nrow(extinct_species) > 0) {
  print("There are potentially extinct species in the 2023 data:")
  print(extinct_species)
} else {
  print("No potentially extinct species found in the 2023 data.")
}

# Optional: Get the count of potentially extinct species
extinct_species_count <- nrow(extinct_species)
print(paste("Number of potentially extinct species:", extinct_species_count))