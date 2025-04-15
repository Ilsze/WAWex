#This file conducts simple analysis of the time series from the calculations gsheet
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)  

#read in time series
calc_tseries <- read_excel("first_pass/calc_tseries.xlsx") %>% 
  mutate(utility = aliveatanytime*Welfare_potential*Welfare_level)
#save utility to file
write.xlsx(calc_tseries, "first_pass/calc_tseries.xlsx")

#create net utility 
# Step 1: Get min and max years for each Category
year_bounds <- calc_tseries %>%
  group_by(Category) %>%
  summarise(min_year = min(Year, na.rm = TRUE),
            max_year = max(Year, na.rm = TRUE))

# Step 2: Find the overlapping year range
global_min_year <- max(year_bounds$min_year)  # latest starting year
global_max_year <- min(year_bounds$max_year)  # earliest ending year

# Step 3: Filter to that overlapping year range
calc_tseries_trimmed <- calc_tseries %>%
  filter(Year >= global_min_year, Year <= global_max_year)

# Step 4: Calculate net utility
net_utility <- calc_tseries_trimmed %>%
  group_by(Year) %>%
  summarise(net_utility = sum(utility))  # defaults to na.rm = FALSE

# Step 5: Save result
write.xlsx(net_utility, "first_pass/net_utility.xlsx")

################ CORRELATIONS AND ELASTICITIES ###################

#get nonhuman categories: a vector of character strings
nh_cat <- unique(calc_tseries$Category)[-1] #removes first entry, Humans

#get human vector
human_pop_vec <- calc_tseries %>% 
  filter(Category == "Humans") %>% 
  select("Year", "aliveatanytime", "Welfare_potential", "Welfare_level", "utility")

#create results df
cor_and_elasticity <- data.frame(Category = character(), cor_pop = numeric(), 
                                 cor_u = numeric(), cor_hpop_au = numeric(), 
                                 cor_hu_apop = numeric(),
                                 e_pop = numeric(), e_u = numeric(), 
                                 e_hpop_au = numeric(), e_hu_apop = numeric(), 
                                 stringsAsFactors = FALSE)

#populate correlation df
for (category in nh_cat) {
  cat_pop_vec <- calc_tseries %>% 
    filter(Category == category) %>% 
    select("Year", "aliveatanytime", "Welfare_potential", "Welfare_level", "utility")
  
  #identify min and max year for the correlation
  min_year <- max(min(cat_pop_vec$Year), min(human_pop_vec$Year), na.rm = TRUE)
  max_year <- min(max(cat_pop_vec$Year), max(human_pop_vec$Year), na.rm = TRUE)
  
  #get population and utility vectors for the right time range
  human_pop_vec_trunc <- human_pop_vec %>%
    filter(Year >= min_year & Year <= max_year) %>%
    pull(aliveatanytime)
  human_u_vec_trunc <- human_pop_vec %>%
    filter(Year >= min_year & Year <= max_year) %>%
    pull(utility)
  cat_pop_vec_trunc <- cat_pop_vec %>%
    filter(Year >= min_year & Year <= max_year) %>%
    pull(aliveatanytime)
  cat_u_vec_trunc <- cat_pop_vec %>%
    filter(Year >= min_year & Year <= max_year) %>%
    pull(utility)
  # note that pull is necessary rather than human_pop_vec$aliveatanytime in 
  # order to get vector rather than dataframe
  
  #run correlations
  # cor_hpop_au: correlation between human population and animal utility
  # e_hpop_au: elasticity of animal utility with respect to human population
  cor_pop <- cor(human_pop_vec_trunc, cat_pop_vec_trunc, use = "complete.obs")
  cor_u <- cor(human_u_vec_trunc, cat_u_vec_trunc, use = "complete.obs")
  cor_hpop_au <- cor(human_pop_vec_trunc, cat_u_vec_trunc, use = "complete.obs")
  cor_hu_apop <- cor(human_u_vec_trunc, cat_pop_vec_trunc, use = "complete.obs")
  
  # run regression: estimate how many more/fewer animals per additional human
  model_pop <- lm(cat_pop_vec_trunc ~ human_pop_vec_trunc)
  elasticity_pop <- coef(model_pop)[2]  # slope coefficient  
  # run regression: estimate how animal *utility* changes with human *utility*
  model_u <- lm(cat_u_vec_trunc ~ human_u_vec_trunc)
  elasticity_u <- coef(model_u)[2] 
  # run regression: estimate how animal *utility* changes with human *population*
  model_hpop_au <- lm(cat_u_vec_trunc ~ human_pop_vec_trunc)
  elasticity_hpop_au <- coef(model_hpop_au)[2]
  # run regression: estimate how animal *population* changes with human *utility*
  model_hu_apop <- lm(cat_pop_vec_trunc ~ human_u_vec_trunc)
  elasticity_hu_apop <- coef(model_hu_apop)[2]
  
  #add results to result table - UTH
  cor_and_elasticity <- cor_and_elasticity %>% 
    add_row(Category = category, cor_pop = cor_pop, 
            cor_u = cor_u, cor_hpop_au = cor_hpop_au, 
            cor_hu_apop = cor_hu_apop,
            e_pop = elasticity_pop, e_u = elasticity_u, 
            e_hpop_au = elasticity_hpop_au, e_hu_apop = elasticity_hu_apop)

}

#spreadsheet has category, correlation, and elasticity variables only
write.xlsx(cor_and_elasticity, "first_pass/cor_and_elas.xlsx")


########################## STRAIGHT FACTOR CHANGE ############################## 
##produces "factor change in aliveatanytime over the available time periods"
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





