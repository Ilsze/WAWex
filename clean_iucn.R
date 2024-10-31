#this file views and manipulates iucn bird data
#the data folder contains... 
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, stringr) 

#find out how many entries are "the global population has not been quantified". 
#If this proportion exceeds 30%, this approach may not be worth taking. 
assessments <- read_csv("./data/iucn/assessments.csv") 
assessments1 <- read_csv("./data/iucn/assessments1.csv") 
ass <- rbind(assessments, assessments1)
sum(grepl("^The global population size has not been quantified", ass$population, ignore.case = TRUE)) #8652 out of 13856 rows begin this way (62%). At most 5204 global estimates available
#meanwhile, this paper makes Global abundance estimates for 9,700 bird species: https://www.pnas.org/doi/full/10.1073/pnas.2023170118
#how many estimate the global population size? 
search_terms <- c("estimate of the global population",
                  "estimate of the population",
                  "the population has been estimated at",
                  "the population is estimated to number",
                  "the population may number",
                  "the population size is preliminarily estimated",
                  "the population was estimated to number",
                  "the population size that exceeds",
                  "estimated the total population",
                  "rounded here to",
                  "this equates to",
                  "global total population numbers",
                  "global population size likely exceeds",
                  "global population has been estimated at",
                  "global population is estimated to number",
                  "global population may number",
                  "global population size is preliminarily estimated",
                  "global population was estimated to number",
                  "global population size that exceeds",
                  "global population size likely exceeds")
ass$population %>%
  str_detect(regex(paste(search_terms, collapse = "|"), ignore_case = TRUE)) %>%
  sum()
#this is the relevant chat: https://claude.ai/chat/c4356c75-649f-40df-8f52-adb1e286545c

