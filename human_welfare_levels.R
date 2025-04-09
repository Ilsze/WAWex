#This file estimates human welfare levels first globally, then at a disaggregated level. 
library(pacman)
p_load(tidyverse, dplyr, ggplot2, gridExtra, mgcv, nleqslv, png, readr, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)

#load human population per country per year 
un_c5023 <- read_excel("dat/un/un_c5023.xlsx")

#load world GDP per country per year
wb_w6023 <- read_excel("dat/world_bank/GDP_world_clean.xlsx")

######## calibrate isoelastic utility function. #####################
india_2018_GDP <- wb_w6023 %>%
  filter(Country_Name == "India", Year == 2018) %>%
  pull(GDP_filled)

canada_2018_GDP <- wb_w6023 %>% 
  filter(Country_Name == "Canada", Year == 2018) %>% 
  pull(GDP_filled)

india_2018_pop <- un_c5023 %>% 
  filter(Country == "India", Year == 2018) %>% 
  pull(Total)

canada_2018_pop <- un_c5023 %>% 
  filter(Country == "Canada", Year == 2018) %>% 
  pull(Total)

burundi_2018_GDP <- wb_w6023 %>%
  filter(Country_Name == "Burundi", Year == 2018) %>%
  pull(GDP_filled)

burundi_2018_pop <- un_c5023 %>% 
  filter(Country == "Burundi", Year == 2018) %>% 
  pull(Total)

#get GDP per capita
iGDPpc_2018 <- india_2018_GDP/india_2018_pop
cGDPpc_2018 <- canada_2018_GDP/canada_2018_pop
bGDPpc_2018 <- burundi_2018_GDP/burundi_2018_pop @#245


#results after using wolfram alpha on india and canada suggest
ubar <- -0.0832451
gamma <- 0.70255
#for a sanity check, this gives us u(245) = 17 welfare score for Burundi, which 
#is startlingly close to the quick analysis of 18/100 that I actually estimated.


