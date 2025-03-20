## This file cleans rosenberg data so that only year, median abundance estimate, 
## and median estimate of the change exists
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)  

overall_bird_n <- read_csv("./AdamCSmithCWS-Estimating_Change_in_NorthAmerican_Birds-a78d595/overall avifauna trajectories N and loss.csv") %>% 
  select("year", "N_med")

#calculate proportion lost
N_med_ref <- overall_bird_n$N_med[[1]] #number of birds at earliest year in US
rosenberg_na7017 <- overall_bird_n %>% 
  mutate(prop_change = N_med / N_med_ref)

#save to file
write_csv(rosenberg_na7017, "dat/rosenberg/rosenberg_na7017.csv")
