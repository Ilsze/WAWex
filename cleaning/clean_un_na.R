## This file cleans un data so that only North American data exists between 1960 and 2023
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, foreign, broom, knitr, data.table) 

############# POPULATION DATA CLEANING #########################################
un_dat <- read_csv("dat/un/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv")

## BELOW CODE IS COPIED FROM OLD swb_na_bird
#filter for north american region/subregion/country/area
## Bermuda is LocID 60. Greenland is LocID 304. Saint Pierre and Miquelon is 666. Each are excluded due to bird data probs not there
## Canada is locID 124
## USA is LocID 840
un_f <- un_dat %>%
  select("LocID", "Location", "PopTotal", "Time") %>%
  filter(LocID == 840 | LocID == 124) %>%
  #at this point, still differentiated by age group
  group_by(Time, Location) %>%
  summarise(PopTot = sum(PopTotal), .groups = "drop") %>%
  ungroup() %>% 
  #multiply population by 1000 to eliminate need for rescaling later
  mutate(PopTot = PopTot * 1000)

#rename values for consistency
un_f <- un_f %>%
  rename(Country = Location) %>%
  mutate(Country = ifelse(Country == "United States of America", "United States", Country))

#keep 1960 to 2023
un_6023 <- filter(un_f, Time >= 1960)


#save to avoid having to load un_dat again
write_rds(un_6023, "./dat/un/un_6023.rds")