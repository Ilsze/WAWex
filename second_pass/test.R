library(pacman)
p_load(tidyverse, dplyr, ggplot2, gridExtra, mgcv, nleqslv, png, readr, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)

wb_data <- read_excel("dat/world_bank/world_bank_pop_gdp_clean.xlsx")
unique(wb_data$Country)
