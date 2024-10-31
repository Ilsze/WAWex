## This file cleans world bank GDP data

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, foreign, broom, knitr, data.table) 

GDP_long <- GDP_dat %>%
  # Filter for just US and Canada
  filter(`Country Name` %in% c("United States", "Canada")) %>%
  # Select only the columns we need
  select(`Country Name`, matches("^\\d{4}$")) %>%  # This keeps country name and year columns
  # Pivot longer
  pivot_longer(
    cols = matches("^\\d{4}$"),  # All columns that are years
    names_to = "Time",           # New column for years
    values_to = "GDP"            # New column for GDP values
  )

##add GDP of both countries
GDP_total <- GDP_long %>%
  group_by(Time) %>%
  summarize(GDP = sum(GDP, na.rm = TRUE)) %>%
  arrange(Time)

##save the dataset
write_rds(GDP_total, "dat/world_bank/GDP.rds")
