library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv) #for csv and data manipulation
setwd("/data/sfop-surrogacy/sfop1119/maya")
source("./helper.R")


##12 hours for just under approximately 300,000 rows
##24 hours for maybe 550,000 rows
##36 hours for 825,000 rows

# load all species (including non-animals' metadata) from 2021
bio_all_mdat <- read_csv("./data/bio_metadata_24_06_2021.csv")

bio_minv <- read_csv("./data/bio_minv_dat.csv") #marine invertebrate #819,066
bio_minv_combined <- add_mdat(bio_minv, bio_all_mdat)
bio_minv_pprepped <- prop_prep(bio_minv_combined)
write_rds(bio_minv_pprepped, "data/pprepped/bio_minv_pprepped.rds")
bio_minv_filled <- tibble()
for(i in 1:nrow(bio_minv_pprepped)) {
  filled_row <- fill_missing(bio_minv_pprepped[i,], i, "minv")
  if(!is.null(filled_row)) {
    bio_minv_filled <- rbind(bio_minv_filled, filled_row)
  }
}
write_rds(bio_minv_filled, "data/pfilled/bio_minv_filled.rds")


