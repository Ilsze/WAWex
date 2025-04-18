#this file loads swb data from the US, taken from IVS and appends it to NA 
# population data. It then compares population trends from Rosenberg

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, foreign, broom, knitr, data.table) 

#load US SWB data
swb_us <- read_rds("./dat/int_val_surv/swb_us.rds")

# If you want to save the plots, you can use ggsave:
# ggsave("life_satisfaction_plot.png", plot_satisfaction, width = 10, height = 6)
# ggsave("happiness_plot.png", plot_happiness, width = 10, height = 6)

#swb_us as it is, summarised, in latex format: 
kable(swb_us, format = "latex")

############################# EXTRAPOLATION
# Create linear models
lm_satisfaction <- lm(mean_life_satisfaction ~ S020, data = swb_us)
lm_happiness <- lm(mean_happiness ~ S020, data = swb_us)

# Create a data frame with all years from 1970 to 2017
all_years <- data.frame(S020 = 1970:2017)

# Predict values for all years
predictions <- all_years %>%
  mutate(
    mean_life_satisfaction = predict(lm_satisfaction, newdata = .),
    mean_happiness = predict(lm_happiness, newdata = .)
  )

# Combine predictions with actual data, preferring actual data where available
final_table <- predictions %>%
  left_join(swb_us, by = "S020") %>%
  mutate(
    mean_life_satisfaction = coalesce(mean_life_satisfaction.y, mean_life_satisfaction.x),
    mean_happiness = coalesce(mean_happiness.y, mean_happiness.x)
  ) %>%
  select(S020, mean_life_satisfaction, mean_happiness) %>% 
  #rename S020 column to Time
  rename(Time = S020)
#final_table is a dataset with three columns: Time, mean_life_satisfaction, and mean_happiness


########### POP DATA 
#load pop data 
#un_dat <- read_csv("./data/un/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv.gz") #commented out bc un_7023 is saved

#filter for north american region/subregion/country/area
## Bermuda is LocID 60. Greenland is LocID 304. Saint Pierre and Miquelon is 666. Each are excluded due to bird data probs not there
## Canada is locID 124
## USA is LocID 840
###############################################################################
########################## COMMENTED OUT BECAUSE SAVED un_7023 ###############
# un_f <- un_dat %>% 
#   select("LocID", "Location", "PopTotal", "Time") %>% 
#   filter(LocID == 840 | LocID == 124) %>% 
#   #at this point, still differentiated by age group
#   group_by(Time) %>% 
#   summarise(PopTot = sum(PopTotal)) %>% 
#   ungroup()
# 
# #keep 1970 to 2017
# un_7023 <- filter(un_f, Time >= 1970)
# #save to avoid having to load un_dat again
# write_rds(un_7023, "./data/un/un_7023.rds")
###############################################################################
###############################################################################
#note that population counts are in 1000s. The colnames are Time and PopTot
#read un_7023 if neccesary
un_7023 <- readRDS("~/GitHub/WAWex/data/un/un_7023.rds")

#merge un_7023 and final_table
na_pop_us_swb <- merge(final_table, un_7023, by = "Time", all = TRUE) %>% 
  #only keep up to 2017
  filter(Time < 2018) %>% 
  #multiply PopTot by a thousand to it's now in individuals not thousands of individuals
  mutate(PopTot = PopTot*1000, 
         LSTot = PopTot*mean_life_satisfaction) 
#LSTot is the variable we're interested in, standing for total life satisfaction at that period of time

#load bird data 
overall_bird_n <- read_csv("./AdamCSmithCWS-Estimating_Change_in_NorthAmerican_Birds-a78d595/overall avifauna trajectories N and loss.csv") %>% 
  select("year", "N_med", "Loss_med")

#merge with the rest of the data
# Assuming na_pop_us_swb has "Time" column and overall_bird_n has "year" column
# First rename year in bird dataset to match the other data
names(overall_bird_n)[names(overall_bird_n) == "year"] <- "Time"

# Merge the datasets
swb_bird_na <- merge(na_pop_us_swb, 
                     overall_bird_n[c("Time", "N_med", "Loss_med")],
                     by = "Time", 
                     all.x = TRUE)

# Order by Time if not already
swb_bird_na <- swb_bird_na[order(swb_bird_na$Time),]

################## RAW BIRD, SWB, TIME DATA HAS NOW BEEN ACHIEVED. time_swb_bird.png plot can now be made.

################## NOW manipulate as desired to GET BIRDS LOST PER UTIL DATA
#produce number of birds lost per util
#number of LS utils in base year
LSTot_base <- swb_bird_na$LSTot[[1]]
N_med_base <- swb_bird_na$N_med[[1]]
LS_constant_Hadza <- 8.245
swb_bird_na <- swb_bird_na %>%
          #In all the years from 1970 to the present year, how many extra LS-point-years do North American humans have compared to if population and LS per person hadn't changed since 1970?
  mutate(LS_c = sapply(1:n(), function(i) sum(LSTot[1:i])) - row_number()*LSTot_base,
         #In all the years from 1970 to the present year, how many extra bird-years were lost compared to if bird populations hadn't changed since 1970? 
         Loss_med_c = sapply(1:n(), function(i) -sum(N_med[1:i])) + row_number()*N_med_base, #sign flip to report loss as a positive number
         #In all the years from 1970 to the present year, how many extra bird LS-point-years were lost compared to if bird populations hadn't changed since 1970?
         Loss_LS_bird_c = Loss_med_c * LS_constant_Hadza,
         #In all the years from 1970 to the present year, how many human LS-point-years were gained for every bird-year lost?
         LS_per_bird_c = ifelse(Loss_med_c == 0, NA, LS_c/Loss_med_c),
         #In all the years from 1970 to the present year, how many human LS-point-years were gained for every bird LS-point-year lost?
         LS_per_bird_LS_c = ifelse(Loss_med_c == 0, NA, LS_c/Loss_LS_bird_c))
##save swb_bird_na
write_rds(swb_bird_na, "./dat/swb_bird_na_raw.rds")

############ ANY FURTHER MUTATIONS AFTER SWITCHING LAPTOP
swb_bird_na <- read_rds("./dat/swb_bird_na_raw.rds")
GDP <- read_rds("dat/world_bank/GDP.rds")

# new columns
swb_bird_na <- swb_bird_na %>% 
  mutate(# Bird LSTot
    LSTot_bird = N_med * LS_constant_Hadza) %>% 
  merge(GDP, by = "Time", all.x = TRUE)



