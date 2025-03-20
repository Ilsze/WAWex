## This file cleans fao data about aquaculture tonnages between 1950 and 2022.
# "VALUE" is in tonnes of live weight equivalent. Species codes are translated.
#Weighted proportional change trends for the major group pisces are determined. 

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)

#load aquaculture data
fao_aqua_dat <- read_csv("dat/fao/Aquaculture_Quantity.csv")
#load aquaculture codes
fao_aqua_codes <- read.csv("dat/fao/CL_FI_SPECIES_GROUPS.csv")

#match fao_aqua_codes' Major_groups with fao_aqua_dat
# Select relevant columns from fao_aqua_codes
fao_aqua_codes_subset <- fao_aqua_codes[, c("X3A_Code", "Name_En", "Scientific_Name", "Major_Group")]
# Merge datasets
fao_aqua <- merge(fao_aqua_dat, fao_aqua_codes_subset, 
                     by.x = "SPECIES.ALPHA_3_CODE", by.y = "X3A_Code", 
                     all.x = TRUE) %>% 
#filter only PISCES
  filter(Major_Group == "PISCES")

#check period range by species
# Get min and max year for each species
# Note that this is just a rough idea. More technically, one should separate 
# country code, area code, and environment code.
species_year_range <- fao_aqua %>%
  group_by(SPECIES.ALPHA_3_CODE) %>%
  summarise(min_year = min(PERIOD, na.rm = TRUE),
            max_year = max(PERIOD, na.rm = TRUE))
## finding: median min year is 1991, median max year is 2022

##################### PROP CHANGE DATA #######################################
#proportional change year-on-year at the species-country-area-environment level
fao_aqua_prop_change <- fao_aqua %>%
  arrange(SPECIES.ALPHA_3_CODE, COUNTRY.UN_CODE, AREA.CODE, ENVIRONMENT.ALPHA_2_CODE, PERIOD) %>% #the non-PERIOD variables don't really matter here, but looks better visually
  group_by(SPECIES.ALPHA_3_CODE, COUNTRY.UN_CODE, AREA.CODE, ENVIRONMENT.ALPHA_2_CODE) %>%
  mutate(ton_prop_change = 1 + (VALUE - lag(VALUE)) / lag(VALUE)) %>%
  ungroup()
# Always good practice to ungroup after mutate

# Compute time-averaged VALUE (tlw) per species-country-area-environment
avg_value <- fao_aqua %>%
  group_by(SPECIES.ALPHA_3_CODE, COUNTRY.UN_CODE, AREA.CODE, ENVIRONMENT.ALPHA_2_CODE) %>%
  summarise(avg_VALUE = mean(VALUE, na.rm = TRUE), .groups = "drop")

#proportional change year-on-year at the major group (pisces) level
fao_aqua_agg_prop_change <- fao_aqua_prop_change %>% 
  #merge average tlw values into fao_aqua_prop_change dataset
  left_join(avg_value, by = c("SPECIES.ALPHA_3_CODE", "COUNTRY.UN_CODE", "AREA.CODE", "ENVIRONMENT.ALPHA_2_CODE")) %>%
  group_by(PERIOD) %>% 
  #Weight rows according to average tonnage at the species-country-area-environment level [UTH]
  summarise(
    avg_prop_change = exp( #exp, sum and log, necessary for geometric mean. placement of avg_VALUE is for "weighted geometric mean"
      sum(
        log(ton_prop_change[is.finite(log(ton_prop_change))]) * avg_VALUE[is.finite(log(ton_prop_change))], #is.finite ensures na, NaN, and Inf values are excluded
        na.rm = TRUE
      ) / sum(avg_VALUE[is.finite(log(ton_prop_change))], na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>% 
  ungroup()

#save fao aqua clean
# write.xlsx(fao_aqua, "dat/fao/fao_aquaculture_clean.xlsx") 
#Commented out bc missing head-tail values weren't filled in

#save aquaculture prop_change
write.xlsx(fao_aqua_agg_prop_change, "dat/fao/fao_aquaculture_prop_change.xlsx")



#################################### SCRATCH ###################################
#issue: do I need to fill in missing species data? Generate missing data summary
# Create a full sequence of years for each species and find missing years
missing_years <- fao_aqua %>%
  select(SPECIES.ALPHA_3_CODE, PERIOD) %>%
  distinct() %>%  # Remove duplicates
  group_by(SPECIES.ALPHA_3_CODE) %>%
  complete(PERIOD = full_seq(PERIOD, 1)) %>%  # Create full year range
  filter(is.na(SPECIES.ALPHA_3_CODE)) %>%  # Identify missing years
  select(SPECIES.ALPHA_3_CODE, PERIOD)  # Keep only species and missing years
## No intermediary values missing