library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, stringr, readxl) 

rosenberg <- read_excel("./data/rosenberg/rosenberg.xlsx") 
definitions <- read_excel("./data/rosenberg/rosenberg.xlsx", sheet = "Definitions")

#abandoned. See folder AdamCSmithCWS-Estimating_Change_in_NorthAmerican_Birds-a78d595 for supposedly reproducible code