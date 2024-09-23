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
bio_fish1 <- read_csv("./data/bio_fish1_dat.csv") #fish1 #1.056,478
bio_fish2 <- read_csv("./data/bio_fish2_dat.csv") #fish2 #1,192,388
bio_fish <- rbind(bio_fish1, bio_fish2) #2,248,866
bio_fish_n <- nrow(bio_fish)
bio_fish_index1 <- floor(bio_fish_n/3)
bio_fish_index2 <- floor(2 * (bio_fish_n/3))

bio_fish3.0 <- bio_fish[(bio_fish_index2 + 1):bio_fish_n,]  #749,622
bio_fish3.0_combined <- add_mdat(bio_fish3.0, bio_all_mdat)
bio_fish3.0_pprepped <- prop_prep(bio_fish3.0_combined) #1961 to 2016 #no change needed
write_rds(bio_fish3.0_pprepped, "data/pprepped/bio_fish3.0_pprepped.rds")
bio_fish3.0_filled <- tibble()
for(i in 1:nrow(bio_fish3.0_pprepped)) {
  filled_row <- fill_missing(bio_fish3.0_pprepped[i,], i, "fish3.0")
  if(!is.null(filled_row)) {
    bio_fish3.0_filled <- rbind(bio_fish3.0_filled, filled_row)
  }
}
write_rds(bio_fish3.0_filled, "data/pfilled/bio_fish3.0_filled.rds")