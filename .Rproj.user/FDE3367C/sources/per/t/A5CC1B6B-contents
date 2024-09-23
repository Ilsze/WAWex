library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv) #for csv and data manipulation
setwd("/data/sfop-surrogacy/sfop1119/maya")
source("./helper.R")

bio_bird1_filled <- read_rds("data/pfilled/bio_bird1_filled.rds")
bio_bird2_filled <- read_rds("data/pfilled/bio_bird2_filled.rds")
bio_bird_filled <- rbind(bio_bird1_filled, bio_bird2_filled)

#current value
bio_bird_pd <- calc_pd(bio_bird_filled)
bio_bird_pda <- prop_agg_species(bio_bird_pd)
bio_bird_pda_long <- prop_species_to_plot(bio_bird_pda)
#save bio_bird_pda_long
write_rds(bio_bird_pda_long, "data/pda_long/bio_bird_jank_pda_long.rds")

#local median
bio_bird_pd_lm <- calc_pd_lm(bio_bird_filled)
bio_bird_pda_lm <- prop_agg_species(bio_bird_pd_lm)
bio_bird_pda_long_lm <- prop_species_to_plot(bio_bird_pda_lm)
#save bio_bird_pda_long_lm
write_rds(bio_bird_pda_long_lm, "data/pda_long/bio_bird_jank_pda_long_lm.rds")
