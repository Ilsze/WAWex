#The main biotime file. No LPD here

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv) #for csv and data manipulation
source("./helper_prop_trend.R")

#LPD_original <- read_csv("./data/LPD2022_public_dat.csv") 

# load animal time series data (I think this is 2018) (ALL PLOTS)
bio_amph <- read_csv("./data/biotime/bio_amph_dat.csv") #amphibian
bio_bird <- read_csv("./data/biotime/bio_bird_dat.csv") #bird
bio_fish1 <- read_csv("./data/biotime/bio_fish1_dat.csv") #fish1
bio_fish2 <- read_csv("./data/biotime/bio_fish2_dat.csv") #fish2 
bio_fish <- rbind(bio_fish1, bio_fish2)
bio_mam <- read_csv("./data/biotime/bio_mam_dat.csv") #mammal
bio_rep <- read_csv("./data/biotime/bio_rep_dat.csv") #reptile
bio_finv <- read_csv("./data/biotime/bio_finv_dat.csv") #freshwater invertebrate
bio_minv <- read_csv("./data/biotime/bio_minv_dat.csv") #marine invertebrate
bio_tinv <- read_csv("./data/biotime/bio_tinv_dat.csv") #terrestrial invertebrate
# stack time series data
# bio <- rbind(bio_amph, bio_bird, bio_fish, bio_mam, bio_rep, bio_finv, bio_minv, bio_tinv)

# load all species (including non-animals' metadata) from 2021
bio_all_mdat <- read_csv("./data/bio_metadata_24_06_2021.csv")

#combine data with metadata
bio_amph_combined <- add_mdat(bio_amph, bio_all_mdat)
bio_bird_combined <- add_mdat(bio_bird, bio_all_mdat)
bio_fish_combined <- add_mdat(bio_fish, bio_all_mdat)
bio_mam_combined <- add_mdat(bio_mam, bio_all_mdat)
bio_rep_combined <- add_mdat(bio_rep, bio_all_mdat)
bio_finv_combined <- add_mdat(bio_finv, bio_all_mdat)
bio_minv_combined <- add_mdat(bio_minv, bio_all_mdat)
bio_tinv_combined <- add_mdat(bio_tinv, bio_all_mdat)

##same as above, but removes rows with 1 or fewer time series data (PLOTS 3 AND 4)
##this function DOES remove rows with 1 or fewer time series data
bio_amph_pprepped <- prop_prep(bio_amph_combined)
write_rds(bio_amph_pprepped, "data/pprepped/bio_amph_pprepped.rds")
bio_bird_pprepped <- prop_prep(bio_bird_combined)
write_rds(bio_bird_pprepped, "data/pprepped/bio_bird_pprepped.rds")
bio_fish_pprepped <- prop_prep(bio_fish_combined)
write_rds(bio_fish_pprepped, "data/pprepped/bio_fish_pprepped.rds")
bio_mam_pprepped <- prop_prep(bio_mam_combined)
write_rds(bio_mam_pprepped, "data/pprepped/bio_mam_pprepped.rds")
bio_rep_pprepped <- prop_prep(bio_rep_combined)
write_rds(bio_rep_pprepped, "data/pprepped/bio_rep_pprepped.rds")
bio_finv_pprepped <- prop_prep(bio_finv_combined)
write_rds(bio_finv_pprepped, "data/pprepped/bio_finv_pprepped.rds")
bio_minv_pprepped <- prop_prep(bio_minv_combined)
write_rds(bio_minv_pprepped, "data/pprepped/bio_minv_pprepped.rds")
bio_tinv_pprepped <- prop_prep(bio_tinv_combined)
write_rds(bio_tinv_pprepped, "data/pprepped/bio_tinv_pprepped.rds")

#fill missing data (PLOTS 3 AND POSSIBLY 4) 
##initiate filled data
print("fill amph")
bio_amph_filled <- tibble()
for(i in 1:nrow(bio_amph_pprepped)) {
  filled_row <- fill_missing(bio_amph_pprepped[i,], i, "amph")
  if(!is.null(filled_row)) {
    bio_amph_filled <- rbind(bio_amph_filled, filled_row)
  }
}
write_rds(bio_amph_filled, "data/pfilled/bio_amph_filled.rds")
print("fill bird")
bio_bird_filled <- tibble()
for(i in 1:nrow(bio_bird_pprepped)) {
  filled_row <- fill_missing(bio_bird_pprepped[i,], i, "bird")
  if(!is.null(filled_row)) {
    bio_bird_filled <- rbind(bio_bird_filled, filled_row)
  }
}
write_rds(bio_bird_filled, "data/pfilled/bio_bird_filled.rds")
print("fill fish")
bio_fish_filled <- tibble()
for(i in 1:nrow(bio_fish_pprepped)) {
  filled_row <- fill_missing(bio_fish_pprepped[i,], i, "fish")
  if(!is.null(filled_row)) {
    bio_fish_filled <- rbind(bio_fish_filled, filled_row)
  }
}
write_rds(bio_fish_filled, "data/pfilled/bio_fish_filled.rds")
print("fill mam")
bio_mam_filled <- tibble()
for(i in 1:nrow(bio_mam_pprepped)) {
  filled_row <- fill_missing(bio_mam_pprepped[i,], i, "mam")
  if(!is.null(filled_row)) {
    bio_mam_filled <- rbind(bio_mam_filled, filled_row)
  }
}
write_rds(bio_mam_filled, "data/pfilled/bio_mam_filled.rds")
print("fill rep")
bio_rep_filled <- tibble()
for(i in 1:nrow(bio_rep_pprepped)) {
  filled_row <- fill_missing(bio_rep_pprepped[i,], i, "rep")
  if(!is.null(filled_row)) {
    bio_rep_filled <- rbind(bio_rep_filled, filled_row)
  }
}
write_rds(bio_rep_filled, "data/pfilled/bio_rep_filled.rds")
print("fill finv")
bio_finv_filled <- tibble()
for(i in 1:nrow(bio_finv_pprepped)) {
  filled_row <- fill_missing(bio_finv_pprepped[i,], i, "finv")
  if(!is.null(filled_row)) {
    bio_finv_filled <- rbind(bio_finv_filled, filled_row)
  }
}
write_rds(bio_finv_filled, "data/pfilled/bio_finv_filled.rds")
print("fill minv")
bio_minv_filled <- tibble()
for(i in 1:nrow(bio_minv_pprepped)) {
  filled_row <- fill_missing(bio_minv_pprepped[i,], i, "minv")
  if(!is.null(filled_row)) {
    bio_minv_filled <- rbind(bio_minv_filled, filled_row)
  }
}
write_rds(bio_minv_filled, "data/pfilled/bio_minv_filled.rds")
print("fill tinv")
bio_tinv_filled <- tibble()
for(i in 1:nrow(bio_tinv_pprepped)) {
  filled_row <- fill_missing(bio_tinv_pprepped[i,], i, "tinv")
  if(!is.null(filled_row)) {
    bio_tinv_filled <- rbind(bio_tinv_filled, filled_row)
  }
}
write_rds(bio_tinv_filled, "data/pfilled/bio_tinv_filled.rds")

################################ USING ARC
bio_amph_filled <- read_rds("data/pfilled/bio_amph_filled.rds")
bio_bird1_filled <- read_rds("data/pfilled/bio_bird1_filled.rds")
bio_bird2_filled <- read_rds("data/pfilled/bio_bird2_filled.rds")
bio_fish1.0_filled <- read_rds("data/pfilled/bio_fish1.0_filled.rds")
bio_fish2.0_filled <- read_rds("data/pfilled/bio_fish2.0_filled.rds")
bio_fish3.0_filled <- read_rds("data/pfilled/bio_fish3.0_filled.rds")



##################### PROP CHANGE PLOTS
## currently, filled total data is named bio_[group]_filled
#check that the year columns have no NA values
##just for mismatched column accidental instance, drop 1923 and 2015 on bird1
bio_bird1_filled <- select(bio_bird1_filled, -y_1923, -y_2015)
bio_bird_filled <- rbind(bio_bird1_filled, bio_bird2_filled)



#calculate prop change relative to moving lagged median
bio_amph_pd <- calc_pd(bio_amph_filled) #done
bio_bird_pd <- calc_pd(bio_bird_filled) #waiting for ARC
bio_fish_pd <- calc_pd(bio_fish_filled) #waiting for ARC
bio_mam_pd <- calc_pd(bio_mam_filled) #waiting for ARC
bio_rep_pd <- calc_pd(bio_rep_filled) #WIP
bio_finv_pd <- calc_pd(bio_finv_filled) #WIP
bio_tinv_pd <- calc_pd(bio_tinv_filled) #WIP
bio_minv_pd <- calc_pd(bio_minv_filled) #waiting for arc


#aggregate proportional change
bio_amph_pda <- prop_agg_species(bio_amph_pd)
bio_bird_pda <- prop_agg_species(bio_bird_pd)
bio_fish_pda <- prop_agg_species(bio_fish_pd)
bio_mam_pda <- prop_agg_species(bio_mam_pd)
bio_rep_pda <- prop_agg_species(bio_rep_pd)
bio_finv_pda <- prop_agg_species(bio_finv_pd)
bio_tinv_pda <- prop_agg_species(bio_tinv_pd)
bio_minv_pda <- prop_agg_species(bio_minv_pd)

#calibration would take place at this stage

#prepare data for plotting proportional change graphs
bio_amph_pda_long <- prop_species_to_plot(bio_amph_pda)
bio_bird_pda_long <- prop_species_to_plot(bio_bird_pda)
bio_fish_pda_long <- prop_species_to_plot(bio_fish_pda)
bio_mam_pda_long <- prop_species_to_plot(bio_mam_pda)
bio_rep_pda_long <- prop_species_to_plot(bio_rep_pda)
bio_finv_pda_long <- prop_species_to_plot(bio_finv_pda)
bio_tinv_pda_long <- prop_species_to_plot(bio_tinv_pda)
bio_minv_pda_long <- prop_species_to_plot(bio_minv_pda)

#save_pda_long
write_rds(bio_amph_pda_long, "./data/pda_long/bio_amph_pda_long.rds")
write_rds(bio_rep_pda_long, "./data/pda_long/bio_rep_pda_long.rds")
write_rds(bio_finv_pda_long, "./data/pda_long/bio_finv_pda_long.rds")
write_rds(bio_tinv_pda_long, "./data/pda_long/bio_tinv_pda_long.rds")

#generate and save plots for relative change
plot_prop_species("Amphibian", bio_amph_pda_long)
plot_prop_species("Bird", bio_bird_jank_pda_long) #not actually jank
plot_prop_species("Fish", bio_fish_pda_long)
plot_prop_species("Mammal", bio_mam_pda_long)
plot_prop_species("Reptile", bio_rep_pda_long)
plot_prop_species("Freshwater Invertebrate", bio_finv_pda_long)
plot_prop_species("Terrestrial Invertebrate", bio_tinv_pda_long)
plot_prop_species("Marine Invertebrate", bio_minv_pda_long)

##accumulate pda_long
bio_amph_cu <- accumulate(bio_amph_pda_long)
bio_rep_cu <- accumulate(bio_rep_pda_long)
bio_finv_cu <- accumulate(bio_finv_pda_long)
bio_tinv_cu <- accumulate(bio_tinv_pda_long)
bio_bird_cu <- accumulate(bio_bird_jank_pda_long) #not actually jank

#generate and save plots for accumulated normalised change
plot_prop_species_cu("Amphibian", bio_amph_cu)
plot_prop_species_cu("Bird", bio_bird_cu)
plot_prop_species_cu("Fish", bio_fish_cu)
plot_prop_species_cu("Mammal", bio_mam_cu)
plot_prop_species_cu("Reptile", bio_rep_cu)
plot_prop_species_cu("Freshwater Invertebrate", bio_finv_cu)
plot_prop_species_cu("Terrestrial Invertebrate", bio_tinv_cu)
plot_prop_species_cu("Marine Invertebrate", bio_minv_cu)



########### RELATIVE TO LOCAL MEDIAN VERSION

#calculate prop change of local median relative to moving lagged median
bio_amph_pd_lm <- calc_pd_lm(bio_amph_filled)
bio_bird_pd_lm <- calc_pd_lm(bio_bird_filled)
bio_fish_pd_lm <- calc_pd_lm(bio_fish_filled)
bio_mam_pd_lm <- calc_pd_lm(bio_mam_filled)
bio_rep_pd_lm <- calc_pd_lm(bio_rep_filled)
bio_finv_pd_lm <- calc_pd_lm(bio_finv_filled)
bio_tinv_pd_lm <- calc_pd_lm(bio_tinv_filled)
bio_minv_pd_lm <- calc_pd_lm(bio_minv_filled)


#aggregate proportional change
bio_amph_pda_lm <- prop_agg_species(bio_amph_pd_lm)
bio_bird_pda_lm <- prop_agg_species(bio_bird_pd_lm)
bio_fish_pda_lm <- prop_agg_species(bio_fish_pd_lm)
bio_mam_pda_lm <- prop_agg_species(bio_mam_pd_lm)
bio_rep_pda_lm <- prop_agg_species(bio_rep_pd_lm)
bio_finv_pda_lm <- prop_agg_species(bio_finv_pd_lm)
bio_tinv_pda_lm <- prop_agg_species(bio_tinv_pd_lm)
bio_minv_pda_lm <- prop_agg_species(bio_minv_pd_lm)

#prepare data for plotting proportional change graphs
bio_amph_pda_long_lm <- prop_species_to_plot(bio_amph_pda_lm)
bio_bird_pda_long_lm <- prop_species_to_plot(bio_bird_pda_lm)
bio_fish_pda_long_lm <- prop_species_to_plot(bio_fish_pda_lm)
bio_mam_pda_long_lm <- prop_species_to_plot(bio_mam_pda_lm)
bio_rep_pda_long_lm <- prop_species_to_plot(bio_rep_pda_lm)
bio_finv_pda_long_lm <- prop_species_to_plot(bio_finv_pda_lm)
bio_tinv_pda_long_lm <- prop_species_to_plot(bio_tinv_pda_lm)
bio_minv_pda_long_lm <- prop_species_to_plot(bio_minv_pda_lm)

#write to file
write_rds(bio_amph_pda_long_lm, "./data/pda_long_lm/bio_amph_pda_long_lm.rds")
write_rds(bio_rep_pda_long_lm, "./data/pda_long_lm/bio_rep_pda_long_lm.rds")
write_rds(bio_finv_pda_long_lm, "./data/pda_long_lm/bio_finv_pda_long_lm.rds")
write_rds(bio_tinv_pda_long_lm, "./data/pda_long_lm/bio_tinv_pda_long_lm.rds")

#generate and save plots for relative change of local median
plot_prop_species_lm("Amphibian", bio_amph_pda_long_lm)
plot_prop_species_lm("Bird", bio_bird_jank_pda_long_lm)
plot_prop_species_lm("Fish", bio_fish_pda_long_lm)
plot_prop_species_lm("Mammal", bio_mam_pda_long_lm)
plot_prop_species_lm("Reptile", bio_rep_pda_long_lm)
plot_prop_species_lm("Freshwater Invertebrate", bio_finv_pda_long_lm)
plot_prop_species_lm("Terrestrial Invertebrate", bio_tinv_pda_long_lm)
plot_prop_species_lm("Marine Invertebrate", bio_minv_pda_long_lm)

#ARC not correct or up to date. needs helper and prop-trend






