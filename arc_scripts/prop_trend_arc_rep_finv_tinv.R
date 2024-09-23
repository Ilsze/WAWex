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

bio_rep <- read_csv("./data/bio_rep_dat.csv") #reptile #4,299
bio_rep_combined <- add_mdat(bio_rep, bio_all_mdat)
bio_rep_pprepped <- prop_prep(bio_rep_combined)
write_rds(bio_rep_pprepped, "data/pprepped/bio_rep_pprepped.rds")
bio_rep_filled <- tibble()
for(i in 1:nrow(bio_rep_pprepped)) {
  filled_row <- fill_missing(bio_rep_pprepped[i,], i, "rep")
  if(!is.null(filled_row)) {
    bio_rep_filled <- rbind(bio_rep_filled, filled_row)
  }
}
write_rds(bio_rep_filled, "data/pfilled/bio_rep_filled.rds")

bio_finv <- read_csv("./data/bio_finv_dat.csv") #freshwater invertebrate #92,668
bio_finv_combined <- add_mdat(bio_finv, bio_all_mdat)
bio_finv_pprepped <- prop_prep(bio_finv_combined)
write_rds(bio_finv_pprepped, "data/pprepped/bio_finv_pprepped.rds")
bio_finv_filled <- tibble()
for(i in 1:nrow(bio_finv_pprepped)) {
  filled_row <- fill_missing(bio_finv_pprepped[i,], i, "finv")
  if(!is.null(filled_row)) {
    bio_finv_filled <- rbind(bio_finv_filled, filled_row)
  }
}
write_rds(bio_finv_filled, "data/pfilled/bio_finv_filled.rds")

bio_tinv <- read_csv("./data/bio_tinv_dat.csv") #terrestrial invertebrate #195,506
bio_tinv_combined <- add_mdat(bio_tinv, bio_all_mdat)
bio_tinv_pprepped <- prop_prep(bio_tinv_combined)
write_rds(bio_tinv_pprepped, "data/pprepped/bio_tinv_pprepped.rds")
bio_tinv_filled <- tibble()
for(i in 1:nrow(bio_tinv_pprepped)) {
  filled_row <- fill_missing(bio_tinv_pprepped[i,], i, "tinv")
  if(!is.null(filled_row)) {
    bio_tinv_filled <- rbind(bio_tinv_filled, filled_row)
  }
}
write_rds(bio_tinv_filled, "data/pfilled/bio_tinv_filled.rds")






