#this file takes clean human population data, fao livestock data, fao aquaculture data, 
#basic insect backcast data, fish trend data, wild animal group trend data
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, rlang)

#read in human pop data
un_w5025 <- read_excel("dat/un/un_w5025.xlsx")
#create pop_change dataset
un_w5025_pd <- compute_prop_change(un_w5025, Time, Value)
##geometric mean of proportional change
gmean_un_w5025_pd <- exp(mean(log(un_w5025_pd$prop_change), na.rm = TRUE)) #pretty accurate!
#gmean is 1.016. This is the average growth rate per year

#read in livestock fao data
#Note that FAO data is 'stocks', which indicates the number of units of the species 
#present in the country at the time of enumeration.
## This data is also granular via Item and Area
fao_t6123 <- read_excel("dat/fao/fao_terrestrial_livestock_clean.xlsx")
#compute number of each animal group today
fao_t23_PopTotal <- fao_t6123 %>%
  filter(Year == 2023) %>% 
  group_by(Item) %>%
  summarise(PopTot = sum(Value, na.rm = TRUE)) %>% 
  ungroup()
#compute prop change
fao_t6123_pd <- fao_t6123 %>%
  group_by(Item) %>%
  group_modify(~ compute_prop_change(.x, Year, Value, Area)) %>% 
  ungroup()
#returns a single datagrame with the proportional change results for every species



## UTH WHY DID WE WANT PROP CHANGE AGAIN? FLAT POPULATION IS THE POINT!

################# VIEWING PURPOSES ###########
#human time series 
ggplot(un_w5025_pd, aes(x = Time, y = prop_change)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Annual Proportional Change in Human Population",
    x = "Year",
    y = "Average Proportional Change"
  ) +
  theme_minimal()

#FAO terrestrial livestock time series

############ MUCH TOO VERBOSE #############
#have to separate fao_t6123 by Item because compute_prop_change doesn't deal with
#species
fao_t6123_ass <- filter(fao_t6123, Item == "Asses")
fao_t6123_cam <- filter(fao_t6123, Item == "Camels")
fao_t6123_cat <- filter(fao_t6123, Item == "Cattle")
fao_t6123_chi <- filter(fao_t6123, Item == "Chickens")
fao_t6123_goa <- filter(fao_t6123, Item == "Goats")
fao_t6123_hor <- filter(fao_t6123, Item == "Horses")
fao_t6123_mul <- filter(fao_t6123, Item == "Mules and hinnies")
fao_t6123_she <- filter(fao_t6123, Item == "Sheep")
fao_t6123_bee <- filter(fao_t6123, Item == "Bees")
fao_t6123_buf <- filter(fao_t6123, Item == "Buffalo")
fao_t6123_duc <- filter(fao_t6123, Item == "Ducks")
fao_t6123_gee <- filter(fao_t6123, Item == "Geese")
fao_t6123_swi <- filter(fao_t6123, Item == "Swine / pigs")
fao_t6123_tur <- filter(fao_t6123, Item == "Turkeys")
fao_t6123_rab <- filter(fao_t6123, Item == "Rabbits and hares")
fao_t6123_oca <- filter(fao_t6123, Item == "Other camelids")
fao_t6123_oro <- filter(fao_t6123, Item == "Other rodents")
fao_t6123_obi <- filter(fao_t6123, Item == "Other birds")
#compute prop change at the Item level, aggregating over areas
fao_t6123_ass_pd <- compute_prop_change(fao_t6123_ass, Year, Value, Area)
fao_t6123_cam_pd <- compute_prop_change(fao_t6123_cam, Year, Value, Area) 
fao_t6123_cat_pd <- compute_prop_change(fao_t6123_cat, Year, Value, Area)
fao_t6123_chi_pd <- compute_prop_change(fao_t6123_chi, Year, Value, Area)
fao_t6123_goa_pd <- compute_prop_change(fao_t6123_goa, Year, Value, Area) 
fao_t6123_hor_pd <- compute_prop_change(fao_t6123_hor, Year, Value, Area)
fao_t6123_mul_pd <- compute_prop_change(fao_t6123_mul, Year, Value, Area)
fao_t6123_she_pd <- compute_prop_change(fao_t6123_she, Year, Value, Area)
fao_t6123_bee_pd <- compute_prop_change(fao_t6123_bee, Year, Value, Area)
fao_t6123_buf_pd <- compute_prop_change(fao_t6123_buf, Year, Value, Area) 
fao_t6123_duc_pd <- compute_prop_change(fao_t6123_duc, Year, Value, Area) 
fao_t6123_gee_pd <- compute_prop_change(fao_t6123_gee, Year, Value, Area)
fao_t6123_swi_pd <- compute_prop_change(fao_t6123_swi, Year, Value, Area) 
fao_t6123_tur_pd <- compute_prop_change(fao_t6123_tur, Year, Value, Area) 
fao_t6123_rab_pd <- compute_prop_change(fao_t6123_rab, Year, Value, Area)
fao_t6123_oca_pd <- compute_prop_change(fao_t6123_oca, Year, Value, Area)
fao_t6123_oro_pd <- compute_prop_change(fao_t6123_oro, Year, Value, Area) 
fao_t6123_obi_pd <- compute_prop_change(fao_t6123_obi, Year, Value, Area) 

unique(fao_t6123$Item)



