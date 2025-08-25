## This file cleans callaghan data so that only Scientific name, lower Ci, upper
## Ci, and abundance estimate remain
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)  

############# POPULATION DATA CLEANING #########################################
callaghan_dat <- read_xlsx("dat/callaghan/pnas.2023170118.sd01.xlsx")

#select only the Scientific name, Lower CI, Upper CI, and Abundance estimate columns
callaghan_results <- callaghan_dat %>%
  select("Scientific name", "95% Lower CI", "Abundance estimate", "95% Upper CI") 

#also calculate the sum 
callaghan_total <- sum(callaghan_results$`Abundance estimate`)

#save to excel
write.xlsx(callaghan_results, "./dat/callaghan/callaghan_ss.xlsx")

# #callaghan precise abundance
# dat <- read.xlsx("dat/callaghan/callaghan_ss.xlsx")
# It's 50516171834 individual birds