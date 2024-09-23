library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv) #for csv and data manipulation
setwd("/data/sfop-surrogacy/sfop1119/maya")
source("./helper.R")

bio_fish1.0_filled <- read_rds("data/pfilled/bio_fish1.0_filled.rds")
bio_fish2.0_filled <- read_rds("data/pfilled/bio_fish2.0_filled.rds")
bio_fish3.0_filled <- read_rds("data/pfilled/bio_fish3.0_filled.rds")
bio_fish_filled <- rbind(bio_fish1.0_filled, bio_fish2.0_filled, bio_fish3.0_filled)

#current value
bio_fish_pd <- calc_pd(bio_fish_filled)
bio_fish_pda <- prop_agg_species(bio_fish_pd)
bio_fish_pda_long <- prop_species_to_plot(bio_fish_pda)
#save bio_fish_pda_long
write_rds(bio_fish_pda_long, "data/pda_long/bio_fish_pda_long.rds")

#local median
bio_fish_pd_lm <- calc_pd_lm(bio_fish_filled)
bio_fish_pda_lm <- prop_agg_species(bio_fish_pd_lm)
bio_fish_pda_long_lm <- prop_species_to_plot(bio_fish_pda_lm)
#save bio_fish_pda_long_lm
write_rds(bio_fish_pda_long_lm, "data/pda_long/bio_fish_pda_long_lm.rds")
