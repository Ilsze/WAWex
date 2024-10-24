#this file merges EVS and WVS into IVS
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, foreign, broom, knitr, data.table) 

#read integrated data
ivs <- read.spss("data/int_val_surv/Integrated_values_surveys_1981-2022.sav", to.data.frame = TRUE)
# #read dictionary
# dict <- read_excel("data/int_val_surv/ZA7503_Common_EVS_WVS_Dictionary_IVS.xlsx") #parsing issues

#US year life satisfaction and happiness data only. 
# S003 is location, S020 is Year, A170 is life satisfaction (1-10), A008 is happiness. 
# For happiness, reports are made on a categorical scale Not at all happy, 
## Not very happy, Quite Happy, and Very happy, converted to a scale (1-4). 
# First keep only important cols
swb_us <- select(ivs, S003, S020, A170, A008) %>%
  filter(S003 == unique(ivs$S003)[68]) %>%
  mutate(
    A170 = case_when(
      A170 == "Satisfied" ~ 8,
      A170 == "Dissatisfied" ~ 3,
      TRUE ~ as.numeric(levels(A170)[A170])
    ),
    A008 = case_when(
      A008 == "Not at all happy" ~ 1,
      A008 == "Not very happy" ~ 2,
      A008 == "Quite happy" ~ 3,
      A008 == "Very happy" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(A008)) %>% 
  group_by(S020) %>%  # Group by year (S020)
  summarise(
    mean_life_satisfaction = mean(A170, na.rm = TRUE),
    median_life_satisfaction = median(A170, na.rm = TRUE),
    mean_happiness = mean(A008, na.rm = TRUE),
    median_happiness = median(A008, na.rm = TRUE),
    n = n()  # Count of observations per year
  ) %>%
  ungroup()


# Ensure S020 is numeric
swb_us <- swb_us %>%
  mutate(S020 = as.numeric(as.character(S020)))

# Ensure S020 is numeric
swb_us <- swb_us %>%
  mutate(S020 = as.numeric(as.character(S020)))

#save dataset so don't have to load ivs from scratch each time
write_rds(swb_us, "./data/int_val_surv/swb_us.rds")
