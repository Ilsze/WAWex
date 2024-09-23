library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv) #for csv and data manipulation
setwd("/data/sfop-surrogacy/sfop1119/maya")
source("./helper.R")

##12 hours for just under approximately 300,000 rows
##24 hours for maybe 550,000 rows
##36 hours for 825,000 rows


# load all species (including non-animals' metadata) from 2021
bio_all_mdat <- read_csv("./data/bio_metadata_24_06_2021.csv")

#fish raw split
bio_fish1 <- read_csv("./data/bio_fish1_dat.csv") #fish1 #2.056,478
bio_fish2 <- read_csv("./data/bio_fish2_dat.csv") #fish2 #1,192,388
bio_fish <- rbind(bio_fish1, bio_fish2) #2,248,866
bio_fish_n <- nrow(bio_fish)
bio_fish_index1 <- floor(bio_fish_n/3)
bio_fish_index2 <- floor(2 * (bio_fish_n/3))

bio_fish2.0 <- bio_fish[(bio_fish_index1 + 1):bio_fish_index2,] #749,622
bio_fish2.0_combined <- add_mdat(bio_fish2.0, bio_all_mdat)
min_year = 1961
max_year = 2016
bio_fish2.0_pprepped <- prop_prep_ex(bio_fish2.0_combined, min_year, max_year) #1962 to 2016 --> 1961 to 2016
write_rds(bio_fish2.0_pprepped, "data/pprepped/bio_fish2.0_pprepped.rds") 
bio_fish2.0_filled <- tibble()
for(i in 1:nrow(bio_fish2.0_pprepped)) {
  filled_row <- fill_missing(bio_fish2.0_pprepped[i,], i, "fish2.0")
  if(!is.null(filled_row)) {
    bio_fish2.0_filled <- rbind(bio_fish2.0_filled, filled_row)
  }
}
write_rds(bio_fish2.0_filled, "data/pfilled/bio_fish2.0_filled.rds")




