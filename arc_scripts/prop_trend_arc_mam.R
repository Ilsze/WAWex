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
 
bio_mam <- read_csv("./data/bio_mam_dat.csv") #mammal #399,499
bio_mam_combined <- add_mdat(bio_mam, bio_all_mdat)
bio_mam_pprepped <- prop_prep(bio_mam_combined)
write_rds(bio_mam_pprepped, "data/pprepped/bio_mam_pprepped.rds")
bio_mam_filled <- tibble()
for(i in 1:nrow(bio_mam_pprepped)) {
  filled_row <- fill_missing(bio_mam_pprepped[i,], i, "mam")
  if(!is.null(filled_row)) {
    bio_mam_filled <- rbind(bio_mam_filled, filled_row)
  }
}
write_rds(bio_mam_filled, "data/pfilled/bio_mam_filled.rds")
