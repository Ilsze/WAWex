library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv) #for csv and data manipulation
setwd("C:/Users/jojos/OneDrive/Documents/R/maya")
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

bio_bird1 <- bio_bird[1:bio_bird_eindex,] #744030
bio_bird1_combined <- add_mdat(bio_bird1, bio_all_mdat)
bio_bird1_pprepped <- prop_prep(bio_bird1_combined)
write_rds(bio_bird1_pprepped, "data/pprepped/bio_bird1_pprepped.rds")
bio_bird1_filled <- tibble()
for(i in 1:nrow(bio_bird1_pprepped)) {
  filled_row <- fill_missing(bio_bird1_pprepped[i,], i, "bird1")
  if(!is.null(filled_row)) {
    bio_bird1_filled <- rbind(bio_bird1_filled, filled_row)
  }
}
write_rds(bio_bird1_filled, "data/pfilled/bio_bird1_filled.rds")


