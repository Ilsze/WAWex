#This file backcasts Abraham's global farmed insect estimates using trend 
#evidence from various sources. 
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, minpack.lm)  
#Two-time-point data
ttp_dat <- read_excel("dat/rp/RP and general search for farmed insect backcast.xlsx", 
                      sheet = "Historical estimates") #note the sheet name is misleading
#Abraham's one-time-point global data
abraham_dat <- read_excel("dat/rp/RP and general search for farmed insect backcast.xlsx", 
                          sheet = "Abraham")

########### BACKCAST FOR TIME SERIES ###########################################
# load Sagar's trends
load("dat/rp/sagar_bsf_model.Rdata") #object is called "log_bsf"
load("dat/rp/sagar_mw_model.Rdata") #object is called "log_mw"

#load aquaculture data
fao_aqua_pd <- read_csv("dat/fao/fao_aquaculture_prop_change") #diff name is normal

#load fao terrestrial livestock data
fao_terrestrial_livestock_clean <- read_excel("dat/fao/fao_terrestrial_livestock_clean")
## Note that "Swine / pigs" are already aggregated, but Poultry (Ducks, Geese, Chickens) are disaggregated

#calculate weight on insect consumption by non-western humans. unit is MMT [uth redo with this unit]
#load un world pop data
#select only 2020 time and only insect eating countries
un_insect_eating_pop <- read_xlsx("dat/un/un_insect_eating_pop.xlsx")
insects_eaten_per_capita <- 0.00010714
total_insect_eaters <- sum(un_insect_eating_pop$PopTot) 
total_insects_eaten <- total_insect_eaters * insects_eaten_per_capita #448860 tonnes per year
#finding: humans might eat around 0.448860 MMT of farmed insects per year

#google sheet produces the following weights for insect backcasting trends: 
pet_food_weight <- 1.225814e-01
layer_feed_weight <- 1.481163e-01
broiler_feed_weight <- 3.340638e-01
swine_feed_weight <- 2.793362e-01
#aquaculture...
#weight on recreational fishing looks too low. don't look into this. too small
#too specific. Use OOM estimates from here: https://rethinkpriorities.org/research-area/the-scale-of-direct-human-impact-on-invertebrates/#Which_and_how_many_invertebrates_are_used_or_killed_by_humans_
#then use human population or gdp growth for the trends in all the small contributors, and for the 
#large contributors, I assume feed and shit, track down the actual trends [UTH]


#TODO: create variable that represents general trend of farmed insects for 
## food and feed for extrapoaltion to non mw and bsf variables



#Missing farmed species
## Abraham Rowe wrote about insects for food and feed. Estimate 86B insects as 
## global pop: https://www.getguesstimate.com/models/16202


##Also a midpoint estimate of 14 billion female cochineals are farmed per year -- but 
## most cochineals are harvested from the wild -- a midpoint estimate of 89 billion



######################### FINAL TOUCHES #########################
#Recall that china must be removed from fao_terrestrial_livestock_clean to avoid double counting



########### Scratch
#see if measure is unique
unique(fao_aqua_dat$MEASURE) #yes it is.
# all values are in the unit indicated by "MEASURE", which is tonnes of live weight
#The only measure variable is Q_tlw, which stands for Quantity in tonnes of live
#weight equivalent. Live weight equivalent refers to the total weight of an 
#animal hen it's alive, before any processing such as gutting deboning, drying, etc

