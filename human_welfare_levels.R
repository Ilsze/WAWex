#This file estimates human welfare levels first globally, then at a disaggregated level. 
library(pacman)
p_load(tidyverse, dplyr, ggplot2, gridExtra, mgcv, nleqslv, png, readr, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)

#load data
wb_6023 <- read_excel("dat/world_bank/world_bank_pop_gdp_clean.xlsx")

######## calibrate isoelastic utility function. #####################
india_2018_GDP <- wb_6023 %>%
  filter(Country == "India", Year == 2018) %>%
  pull(GDP_filled)
# 6714.966

canada_2018_GDP <- wb_6023 %>% 
  filter(Country == "Canada", Year == 2018) %>% 
  pull(GDP_filled)
# 49982.6

burundi_2018_GDP <- wb_6023 %>%
  filter(Country == "Burundi", Year == 2018) %>%
  pull(GDP_filled)
# 753.4555

#results after using wolfram alpha on india and canada suggest #UTH recalibrating
ubar <- -22.1713
gamma <- 0.674252
#for a sanity check, this gives us u(753.4555) = 4 welfare score for Burundi, which 
#is actually a lot lower than what I might have thought, around 18. But it's ok.

######################## WELFARE WEIGHTED RESULTS ##############################
#calculate weflare scores for all
human_WL <- wb_6023 %>% 
  mutate(welfare_level = ubar + (GDP_filled^(1-gamma))/(1-gamma))

#aggregate to global level
human_wWL <- human_WL %>% 
  select(Year, Country, Population, welfare_level) %>% 
  group_by(Year) %>% 
  summarise(
    Total_welfare = sum(welfare_level * Population, na.rm = TRUE) / sum(Population, na.rm = TRUE),
    .groups = "drop"
  )

#save to file
write.xlsx(human_wWL, "first_pass/human_wWL.xlsx")


############### TESTS AND CHECKS ###############
#sanity check india and canada welfare levels given calibration
india_2018_wl <- human_WL %>%
  filter(Country == "India", Year == 2018) %>%
  pull(welfare_level)
# 32

canada_2018_wl <- human_WL %>% 
  filter(Country == "Canada", Year == 2018) %>% 
  pull(welfare_level)
# 82