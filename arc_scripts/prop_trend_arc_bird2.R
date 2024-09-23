library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv) #for csv and data manipulation
setwd("/data/sfop-surrogacy/sfop1119/maya")
source("./helper.R")

#To ask about: Should I first fill in the NULL that intervenes valid data with 
# linearly increasing or decreasing values depending on the trend

##12 hours for just under approximately 300,000 rows
##24 hours for maybe 550,000 rows
##36 hours for 825,000 rows


# load all species (including non-animals' metadata) from 2021
bio_all_mdat <- read_csv("./data/bio_metadata_24_06_2021.csv")

#bird raw split
bio_bird <- read_csv("./data/bio_bird_dat.csv") #bird #1,488,060
bio_bird_n <- nrow(bio_bird)
bio_bird_eindex <- floor(bio_bird_n/2)

bio_bird2 <- bio_bird[(bio_bird_eindex + 1):bio_bird_n,] #744030
bio_bird2_combined <- add_mdat(bio_bird2, bio_all_mdat)
min_year = 1923
max_year = 2015
bio_bird2_pprepped <- prop_prep_ex(bio_bird2_combined, min_year, max_year) #1924 to 2014 -> 1923 to 2015
write_rds(bio_bird2_pprepped, "data/pprepped/bio_bird2_pprepped.rds")
bio_bird2_filled <- tibble()
for(i in 1:nrow(bio_bird2_pprepped)) {
  filled_row <- fill_missing(bio_bird2_pprepped[i,], i, "bird2")
  if(!is.null(filled_row)) {
    bio_bird2_filled <- rbind(bio_bird2_filled, filled_row)
  }
}
write_rds(bio_bird2_filled, "data/pfilled/bio_bird2_filled.rds")
