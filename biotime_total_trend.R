#This file graphs biotime animal group population trends
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png) #for csv and data manipulation
source("./helper.R")

#To ask about: Should I first fill in the NULL that intervenes valid data with 
# linearly increasing or decreasing values depending on the trend

#LPD_original <- read_csv("./data/LPD2022_public_dat.csv") 

# load animal time series data (I think this is 2018) (ALL PLOTS)
bio_amph <- read_csv("./data/bio_amph_dat.csv") #amphibian
bio_bird <- read_csv("./data/bio_bird_dat.csv") #bird
bio_fish1 <- read_csv("./data/bio_fish1_dat.csv") #fish1
bio_fish2 <- read_csv("./data/bio_fish2_dat.csv") #fish2 
bio_fish <- rbind(bio_fish1, bio_fish2)
bio_mam <- read_csv("./data/bio_mam_dat.csv") #mammal
bio_rep <- read_csv("./data/bio_rep_dat.csv") #reptile
bio_finv <- read_csv("./data/bio_finv_dat.csv") #freshwater invertebrate
bio_minv <- read_csv("./data/bio_minv_dat.csv") #marine invertebrate
bio_tinv <- read_csv("./data/bio_tinv_dat.csv") #terrestrial invertebrate
# stack time series data
bio <- rbind(bio_amph, bio_bird, bio_fish, bio_mam, bio_rep, bio_finv, bio_minv, bio_tinv)

# load all species (including non-animals' metadata) from 2021
bio_all_mdat <- read_csv("./data/bio_metadata_24_06_2021.csv")

#after assessment in scratch.R, I find that for 
# "study_id - genus_species - longitude - latitude" level, and 
# "genus_species - longitude - latitude" level, the number of rows is the same 
# in BioTIME. There is variation, at both of these levels, in the PLOT, DEPTH, 
# and BIOMASS variables, so they should be dropped for now.

##check whether any "record" differs in their "study"
#check_record_study(bio)

################# GROUP RAW OBSERVATION TOTALS (PLOTS 1 and 2)
##pivot datasets so that rows contain several years. group only by genus_species
## for now. Be warned that if units differed between rows of the same 
## genus_species, they could have been recorded wrong. 
##this function DOES NOT remove rows with 1 or fewer time series data. 
bio_amph_raw <- raw_prep(bio_amph)
bio_bird_raw <- raw_prep(bio_bird)
bio_fish_raw <- raw_prep(bio_fish)
bio_mam_raw <- raw_prep(bio_mam)
bio_rep_raw <- raw_prep(bio_rep)
bio_finv_raw <- raw_prep(bio_finv)
bio_minv_raw <- raw_prep(bio_minv)
bio_tinv_raw <- raw_prep(bio_tinv)


#fill missing data (PLOT 2) 
##initiate filled data
print("fill amph")
bio_amph_filled <- tibble()
for(i in 1:nrow(bio_amph_raw)) {
  filled_row <- fill_missing(bio_amph_raw[i,])
  if(!is.null(filled_row)) {
    bio_amph_filled <- rbind(bio_amph_filled, filled_row)
  }
}
print("fill bird")
bio_bird_filled <- tibble()
for(i in 1:nrow(bio_bird_raw)) {
  filled_row <- fill_missing(bio_bird_raw[i,])
  if(!is.null(filled_row)) {
    bio_bird_filled <- rbind(bio_bird_filled, filled_row)
  }
}
print("fill fish")
bio_fish_filled <- tibble()
for(i in 1:nrow(bio_fish_raw)) {
  filled_row <- fill_missing(bio_fish_raw[i,])
  if(!is.null(filled_row)) {
    bio_fish_filled <- rbind(bio_fish_filled, filled_row)
  }
}
print("fill mam")
bio_mam_filled <- tibble()
for(i in 1:nrow(bio_mam_raw)) {
  filled_row <- fill_missing(bio_mam_raw[i,])
  if(!is.null(filled_row)) {
    bio_mam_filled <- rbind(bio_mam_filled, filled_row)
  }
}
print("fill rep")
bio_rep_filled <- tibble()
for(i in 1:nrow(bio_rep_raw)) {
  filled_row <- fill_missing(bio_rep_raw[i,])
  if(!is.null(filled_row)) {
    bio_rep_filled <- rbind(bio_rep_filled, filled_row)
  }
}
print("fill finv")
bio_finv_filled <- tibble()
for(i in 1:nrow(bio_finv_raw)) {
  filled_row <- fill_missing(bio_finv_raw[i,])
  if(!is.null(filled_row)) {
    bio_finv_filled <- rbind(bio_finv_filled, filled_row)
  }
}
print("fill minv")
bio_minv_filled <- tibble()
for(i in 1:nrow(bio_minv_raw)) {
  filled_row <- fill_missing(bio_minv_raw[i,])
  if(!is.null(filled_row)) {
    bio_minv_filled <- rbind(bio_minv_filled, filled_row)
  }
}
print("fill tinv")
bio_tinv_filled <- tibble()
for(i in 1:nrow(bio_tinv_raw)) {
  filled_row <- fill_missing(bio_tinv_raw[i,])
  if(!is.null(filled_row)) {
    bio_tinv_filled <- rbind(bio_tinv_filled, filled_row)
  }
}



#### jump to next section for prop graphs

#how much useable data on the binomial-longitude-latitude level? 
# sum(nrow(bio_amph_raw), nrow(bio_bird_raw), nrow(bio_fish_raw), nrow(bio_mam_raw), 
#     nrow(bio_rep_raw), nrow(bio_finv_raw), nrow(bio_minv_raw), nrow(bio_tinv_raw)) #21993

##now get raw totals
## raw_total is a function that produces raw yearly totals for each group
bio_amph_total <- raw_total(bio_amph_raw)
bio_bird_total <- raw_total(bio_bird_raw)
bio_fish_total <- raw_total(bio_fish_raw)
bio_mam_total <- raw_total(bio_mam_raw)
bio_rep_total <- raw_total(bio_rep_raw)
bio_finv_total <- raw_total(bio_finv_raw)
bio_minv_total <- raw_total(bio_minv_raw)
bio_tinv_total <- raw_total(bio_tinv_raw)

## get filled totals
bio_amph_ftotal <- raw_total(bio_amph_filled)
bio_bird_ftotal <- raw_total(bio_bird_filled)
bio_fish_ftotal <- raw_total(bio_fish_filled)
bio_mam_ftotal <- raw_total(bio_mam_filled)
bio_rep_ftotal <- raw_total(bio_rep_filled)
bio_finv_ftotal <- raw_total(bio_finv_filled)
bio_minv_ftotal <- raw_total(bio_minv_filled)
bio_tinv_ftotal <- raw_total(bio_tinv_filled)

##plot raw total plots
plot_amph_total <- plot_raw_total("Amphibian", bio_amph_total)
plot_bird_total <- plot_raw_total("Bird", bio_bird_total)
plot_fish_total <- plot_raw_total("Fish", bio_fish_total)
plot_mam_total <- plot_raw_total("Mammal", bio_mam_total)
plot_rep_total <- plot_raw_total("Reptile", bio_rep_total)
plot_finv_total <- plot_raw_total("Freshwater Invertebrate", bio_finv_total)
plot_minv_total <- plot_raw_total("Marine Invertebrate", bio_minv_total)
plot_tinv_total <- plot_raw_total("Terrestrial Invertebrate", bio_tinv_total)
##saved plots are currently up to date

##Put raw total plots all on the same page
# Read PNG files into a list
rt_imgs <- list.files(path = "./output/group_raw_observed_totals", pattern = "*.png", full.names = TRUE)
rt_img_list <- lapply(rt_imgs, readPNG)
# Convert the PNG images to raster objects
rt_raster_list <- lapply(rt_img_list, function(img) {
  rasterGrob(img, interpolate = FALSE)
})
#Arrange the images in a grid and save the combined image
combined_raw_totals <- grid.arrange(grobs = rt_raster_list, ncol = 3)  # Adjust ncol as needed
# Save the combined plot
ggsave("./output/group_raw_observed_totals/combined_raw_totals.png", combined_raw_totals, width = 20, height = 20, units = "in", dpi = 300)

## plot filled total plots 
plot_amph_ftotal <- plot_filled_total("Amphibian", bio_amph_ftotal)
plot_bird_ftotal <- plot_filled_total("Bird", bio_bird_ftotal)
plot_fish_ftotal <- plot_filled_total("Fish", bio_fish_ftotal)
plot_mam_ftotal <- plot_filled_total("Mammal", bio_mam_ftotal)
plot_rep_ftotal <- plot_filled_total("Reptile", bio_rep_ftotal)
plot_finv_ftotal <- plot_filled_total("Freshwater Invertebrate", bio_finv_ftotal)
plot_minv_ftotal <- plot_filled_total("Marine Invertebrate", bio_minv_ftotal)
plot_tinv_ftotal <- plot_filled_total("Terrestrial Invertebrate", bio_tinv_ftotal)
##saved plots are currently up to date

##Put filled total plots all on the same page
# Read PNG files into a list
ft_imgs <- list.files(path = "./output/group_filled_observed_totals", pattern = "*.png", full.names = TRUE)
ft_img_list <- lapply(ft_imgs, readPNG)
# Convert the PNG images to raster objects
ft_raster_list <- lapply(ft_img_list, function(img) {
  rasterGrob(img, interpolate = FALSE)
})
#Arrange the images in a grid and save the combined image
combined_filled_totals <- grid.arrange(grobs = ft_raster_list, ncol = 3)  # Adjust ncol as needed
# Save the combined plot
ggsave("./output/group_filled_observed_totals/combined_filled_totals.png", combined_filled_totals, width = 20, height = 20, units = "in", dpi = 300)





##EVENTUAL TO DO: COMBINE LPD_DAT WITH BIOTIME DAT




############################ PROCESSING METADATA, MERGING DAT WITH MDAT, AND STACKING


#remove columns we don't need (for now). Note that some columns are mistakenly named and these are dropped
bio_amph_mdat_kept <- select(bio_amph_mdat, STUDY_ID, REALM, CLIMATE, BIOME_MAP, GROUP, TAXA) 
bio_bird_mdat_kept <- select(bio_bird_mdat, STUDY_ID, REALM, CLIMATE, BIOME_MAP, GROUP, TAXA)
bio_fish1_mdat_kept <- select(bio_fish1_mdat, STUDY_ID, REALM, CLIMATE, BIOME_MAP, GROUP, TAXA)
bio_fish2_mdat_kept <- select(bio_fish2_mdat, STUDY_ID, REALM, CLIMATE, BIOME_MAP, GROUP, TAXA)
bio_mam_mdat_kept <- select(bio_mam_mdat, STUDY_ID, REALM, CLIMATE, BIOME_MAP, GROUP, TAXA)
bio_rep_mdat_kept <- select(bio_rep_mdat, STUDY_ID, REALM, CLIMATE, BIOME_MAP, GROUP, TAXA)
bio_finv_mdat_kept <- select(bio_finv_mdat, STUDY_ID, REALM, CLIMATE, BIOME_MAP, GROUP, TAXA)
bio_minv_mdat_kept <- select(bio_minv_mdat, STUDY_ID, REALM, CLIMATE, BIOME_MAP, GROUP, TAXA)
bio_tinv_mdat_kept <- select(bio_tinv_mdat, STUDY_ID, REALM, CLIMATE, BIOME_MAP, GROUP, TAXA)



#stack all the datasets
bio_dat_narrow <- rbind(bio_amph, bio_bird, bio_fish1, bio_fish2, bio_mam, bio_rep, bio_finv, bio_minv, bio_tinv)
#nrow is 6,762,303


################# COMPARING LP WITH BIOTIME

LPD_count_study <- LPD_dat %>% 
  group_by(Citation, Binomial, Latitude, Longitude)
print(paste("num rows LPD at the study-ll-binomial level is:", nrow(LPD_count_study)))
LPD_count_ll <- LPD_count_study %>% 
  group_by(Binomial, Latitude, Longitude)
print(paste("num rows LPD at the ll-binomial level is:", nrow(LPD_count_ll)))
# LPD has 32,680 entries at the both levels. Call it longitude-latitude-binomial level

bio_count_study <- bio_dat %>% 
  group_by(STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE)
print(paste("num rows bio at the study-ll-binomial level is:", nrow(bio_count_study)))
bio_count_ll <- bio_count_study %>% 
  group_by(GENUS_SPECIES, LATITUDE, LONGITUDE)
print(paste("num rows bio at the ll-binomial level is:", nrow(bio_count_ll)))
# BioTIME has 6,762,303 entries at both levels. Call it longitude-latitude-binomial level.
# Now remove rows with only one year-level data point

#now need to check for useable data. pivot_wide bio_dat so that row that share 
# study_id, genus_species, and longitude and latitude are placed into the same 
# row. All other data except abundance, biomas, year, plot, depth, sample_desc
# should be shared by that grouping. These values should be pivoted wide. The 
# other values like genus_species, realm, climate, biome_map, group, and taxa 
# should be maintained.  

  




########### OLD AND OUTDATED DATA MANIPULATION
