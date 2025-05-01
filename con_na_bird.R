# This file plots north american bird utility against human growth, population,
# and utility. Utility is derived from consumption-based measures.
#con stands for "consumption", representing the consumption approach
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, foreign, broom, knitr, data.table) 

############## BIRD CONSUMPTION AS HUNTER-GATHERER FOOD CONSUMPTION ############
swb_bird_na <- readRDS("~/Documents/GitHub/WAWex/dat/swb_bird_na.rds")
HFCE <- readRDS("dat/world_bank/HFCE.rds")
pop_6023 <- readRDS("dat/un/un_6023.rds")
HG_eat_constant <- -1.82 #the hunter gatherer food-derived utility for baseline wild animal welfare
ubar <- -2.47 #Chad's VSL-derived ubar

#Keep only the relevant rows
bird_na <- select(swb_bird_na, Time, N_med, Loss_med, Loss_med_c) 


#merge population and HFCE data
con <- merge(HFCE, pop_6023, c("Country", "Time")) %>% 
  #create utility variable. assumes equality. note HFCE is per capita
  mutate(UTot = PopTot*(ubar + log(HFCE))) %>% 
  #sum US UTot with Canada UTot
  group_by(Time) %>% 
  summarise(UTot = sum(UTot), 
            PopTot = sum(PopTot), #recall PopTot refers to total number of individuals in the country/ies that year
            HFCEAvg = sum(HFCE * PopTot) / PopTot
            ) %>% 
  ungroup()

#keep only 1970 onwards in con for base and ease of eyeballing
con <- filter(con, Time >= 1970)

#prepare base variables
UTot_base = con$UTot[[1]]
PopTot_base = con$PopTot[[1]]

#mutate with consumption analysis variables
con_bird_na <- bird_na %>% 
  mutate(UTot_bird = N_med * HG_eat_constant) %>% 
  #add human con data
  merge(con, "Time") %>% 
        #In all the years from 1970 to the present year, how many extra human util-years relative to if population and utility per person hadn't changed since 1970?
  mutate(U_c = sapply(1:n(), function(i) sum(UTot[1:i])) - row_number()*UTot_base,
         #In all the years from 1970 to the present year, how many extra humans relative to if population stayed stable since 1970?
         Gain_PopTot_c = sapply(1:n(), function(i) sum(PopTot[1:i])) - row_number()*PopTot_base, ##SANITY CHECK UTH DOESN'T LOOK RIGHT
         #In all the years from 1970 to the present year, how many extra negative bird util-years were averted compared to if bird populations hadn't changed since 1970?
         Averted_bird_U_c = -Loss_med_c * HG_eat_constant, #note the negative sign! means values are positive, pointing to the negative utils averted
         #In all the years from 1970 to the present year, how many bird-years averted for every human-year gained relative to if population had stayed stable since 1970? 
         bird_years_averted_per_human_year = ifelse(Gain_PopTot_c == 0, NA, Loss_med_c/Gain_PopTot_c),
         #In all the years from 1970 to the present year, how many bird-years averted for every human util-years gained compared to stability from 1970?
         bird_years_averted_per_human_util_year = ifelse(U_c == 0, NA, Loss_med_c/U_c),
         #In all the years from 1970 to the present year, how many bird-util-years averted for every human-year gained relative to stability from 1970? 
         negative_bird_util_years_averted_per_human_year = ifelse(Gain_PopTot_c == 0, NA, Averted_bird_U_c/Gain_PopTot_c),
         #In all the years from 1970 to the present year, how many bird-util-years averted for every human-util-year gained relative to stability from 1970? 
         negative_bird_util_years_averted_per_human_util_year = ifelse(U_c == 0, NA, Averted_bird_U_c/U_c))


