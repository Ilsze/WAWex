#This file conducts simple analysis of the time series from the calculations gsheet
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)  

#read in time series from google sheet -> excel
calc_tseries <- read_excel("first_pass/calc_tseries.xlsx") #sheet = 3 sometimes needed

#input NC potentially manually from sheet
human_fneurons <- 24560000000
calc_tseries <- calc_tseries %>% 
  mutate(forebrain_neurons = case_when(
    Category == "Humans" ~ human_fneurons,
    Category == "Fish" ~ 43853820,
    Category == "Asses" ~ 1.20E+08,
    Category == "Bees" ~ 170000,
    Category == "Buffalo" ~ 794523333,
    Category == "Camels" ~ 907460000,
    Category == "Cattle" ~ 790000000,
    Category == "Chickens" ~ 86000000,
    Category == "Ducks" ~ 112255000,
    Category == "Geese" ~ 203533500,
    Category == "Goats" ~ 504000000,
    Category == "Horses" ~ 120000000,
    Category == "Mules and hinnies" ~ 1.20E+08,
    Category == "Other birds" ~ 20523000,
    Category == "Other camelids" ~ 907460000,
    Category == "Other rodents" ~ 31500000,
    Category == "Swine / pigs" ~ 554000000,
    Category == "Rabbits and hares" ~ 101000000,
    Category == "Sheep" ~ 504000000,
    Category == "Turkeys" ~ 86000000 )) %>% 
  mutate(NC_potential = forebrain_neurons/human_fneurons, 
         NC_pot_conc = sqrt(NC_potential), 
         NC_pot_conv = NC_potential^2,
         NC_apot = aliveatanytime*NC_potential,
         NC_utility = aliveatanytime*NC_potential*Welfare_level, 
         WR_apot = aliveatanytime*WR_potential,
         WR_utility = aliveatanytime*WR_potential*Welfare_level
         
         )

#save to file
write.xlsx(calc_tseries, "first_pass/calc_tseries.xlsx")