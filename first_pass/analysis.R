#This file conducts simple analysis of the time series from the calculations gsheet
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)  

######################## CORRELATIONS AND REGRESSIONS with 1 ###################
#read in time series
calc_tseries <- read_excel("first_pass/calc_tseries.xlsx")
#get nonhuman categories:
nh_cat <- unique(calc_tseries$Category)[-1] #removes first entry, Humans

#get human vector
human_pop_vec <- calc_tseries %>% 
  filter(Category == "Humans") %>% 
  select("Year", "aliveatanytime")

#create results df
cor_and_elasticity <- data.frame(Category = character(), Correlation = numeric(), 
                                 Elasticity = numeric(), stringsAsFactors = FALSE)

#populate correlation df
for (category in nh_cat) {
  cat_pop_vec <- calc_tseries %>% 
    filter(Category == category) %>% 
    select("Year", "aliveatanytime")
  
  #identify min and max year for the correlation
  min_year <- max(min(cat_pop_vec$Year), min(human_pop_vec$Year))
  max_year <- min(max(cat_pop_vec$Year), max(human_pop_vec$Year))
  
  #get population vectors for the right time range
  human_pop_vec_trunc <- human_pop_vec %>%
    filter(Year >= min_year & Year <= max_year) %>%
    pull(aliveatanytime) 
  # note that pull is necessary rather than human_pop_vec$aliveatanytime in 
  # order to get vector rather than dataframe
  cat_pop_vec_trunc <- cat_pop_vec %>%
    filter(Year >= min_year & Year <= max_year) %>%
    pull(aliveatanytime)
  
  #run correlation
  correlation_result <- cor(human_pop_vec_trunc, cat_pop_vec_trunc, use = "complete.obs")
  
  # run regression: estimate how many more/fewer animals per additional human
  model <- lm(cat_pop_vec_trunc ~ human_pop_vec_trunc)
  elasticity <- coef(model)[2]  # slope coefficient  
  
  #run regression
  #add result to result table
  cor_and_elasticity <- cor_and_elasticity %>% 
    add_row(Category = category, Correlation = correlation_result, 
            Elasticity = elasticity)
}

write.xlsx(cor_and_elasticity, "first_pass/cor_and_elas.xlsx")


########################## STRAIGHT FACTOR CHANGE ############################## 
#initialise vector with factor change
f_change <- data.frame(Category = character(), Factor_change = numeric(), stringsAsFactors = FALSE)

for (category in unique(calc_tseries$Category)) {
  pop_vec <- calc_tseries %>% 
    filter(Category == category) %>% 
    select(Year, aliveatanytime)
  
  min_year <- min(pop_vec$Year)
  max_year <- max(pop_vec$Year)
  
  min_year_value <- pop_vec$aliveatanytime[pop_vec$Year == min_year]
  max_year_value <- pop_vec$aliveatanytime[pop_vec$Year == max_year]
  
  factor_change <- max_year_value / min_year_value
  
  f_change <- f_change %>% 
    add_row(Category = category, Factor_change = factor_change)
}

write.xlsx(f_change, "first_pass/f_change.xlsx")


################### REGGRESION RESULTS ##########################

