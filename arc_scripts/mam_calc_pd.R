library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv) #for csv and data manipulation
setwd("/data/sfop-surrogacy/sfop1119/maya")
source("./helper.R")

bio_mam_filled <- read_rds("data/pfilled/bio_mam_filled.rds")

#current value
bio_mam_pd <- calc_pd(bio_mam_filled)
bio_mam_pda <- prop_agg_species(bio_mam_pd)
bio_mam_pda_long <- prop_species_to_plot(bio_mam_pda)
#save bio_mam_pda_long
write_rds(bio_mam_pda_long, "data/pda_long/bio_mam_pda_long.rds")

#local median
bio_mam_pd_lm <- calc_pd_lm(bio_mam_filled)
bio_mam_pda_lm <- prop_agg_species(bio_mam_pd_lm)
bio_mam_pda_long_lm <- prop_species_to_plot(bio_mam_pda_lm)
#save bio_mam_pda_long_lm
write_rds(bio_mam_pda_long_lm, "data/pda_long/bio_mam_pda_long_lm.rds")
