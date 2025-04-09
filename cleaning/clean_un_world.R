## This file cleans un data so that aggregated world data exists
#BE WARNED. THERE'S DOUBLE COUNTING WITHOUT DATA CLEANING BY COUNTRY
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)  

############# POPULATION DATA CLEANING: WORLD AGGREGATE ################
#only has Location = "World" data
un_world_dat <- read_csv("dat/un/unpopulation_dataportal_20250211133511.csv")

#select only the Time and Value columns
un_w5025 <- un_world_dat %>%
  select("Time", "Value") #Location is 'World' and Loc is '900' btw
#note that units are correct from the outset, unlike in the below treated dataset
  
#save to excel
write.xlsx(un_w5025, "./dat/un/un_w5025.xlsx")

###################  POPULATION DATA CLEANING: BY COUNTRY ###########
#this data is by single age, annually for 1950-2023 in thousands
un_raw <- read_xlsx("dat/un/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", skip = 16) 

un_ctry <- un_raw %>% 
  filter(Type == "Country/Area")

#prepare to sum across age groups
age_cols <- as.character(0:99) %>% c("100+")

#filter for Type = Country/Area
un_dat <- un_ctry %>% 
  #rename col name
  rename(Country = `Region, subregion, country or area *`,
         Code = `ISO3 Alpha-code`) %>%  # rename for sanity
  #ensure age values are numeric
  mutate(across(all_of(age_cols), ~ as.numeric(.))) %>%
  #combine age
  group_by(Country, Code, Year) %>%
  summarise(Total = sum(across(all_of(age_cols)), na.rm = TRUE), .groups = "drop") %>% 
  ungroup() %>% 
  #rescale numbers
  mutate(Total = Total * 1000)

#save to file
write.xlsx(un_dat, "dat/un/un_c5023.xlsx")
  

################# TO GET 2020 POPULATIONS OF INSECT-EATING COUNTRIES ##########
un_dat <- read_csv("dat/un/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv") 

#list insect-eating countries the way they appear in the un data
ins_eat_countries <- c(
  "Lesotho", "Burundi", "Cameroon", "Democratic Republic of the Congo",
  "Central African Republic", "South Sudan", "Zambia", "Burkina Faso",
  "Angola", "Botswana", "Niger", "Zimbabwe", "Malawi", "South Africa",
  "India", "China", "Indonesia", "Lao People's Democratic Republic",
  "Thailand", "Japan", "Brazil", "Venezuela (Bolivarian Republic of)",
  "Ecuador", "Colombia", "Argentina", "Papua New Guinea",
  "Malaysia", "Australia", "Mexico"
)
#insect-eating locations that don't appear in the data: Kalimantan, sinai desert; I leave out

#now make insect-eating country population. two columns: Location and PopTot 
un_insect_eating_pop <- un_dat %>% 
  #select countries 
  filter(Location %in% ins_eat_countries, Time == 2020) %>% 
  #sum over age group for sanity check 
  group_by(Location) %>% 
  summarise(PopTot = sum(PopTotal, na.rm = TRUE)) %>% 
  ungroup() %>% 
  #convert thousands population unit
  mutate(PopTot = PopTot*1000)

#write to file
write.xlsx(un_insect_eating_pop, "dat/un/un_insect_eating_pop.xlsx")
