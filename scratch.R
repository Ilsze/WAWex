## this file engages with LPD and BioTIME datasets

#make bird columns compatible
bio_bird2_extended <- format_and_stack(bio_bird1_filled, bio_bird2_filled)
bio_bird2_filledex <- tibble()
for(i in 1:nrow(bio_bird2_extended)) {
  filled_row <- fill_missing(bio_bird2_extended[i,], i, "bird")
  if(!is.null(bio_bird2_filledex)) {
    bio_bird2_filledex <- rbind(bio_bird2_filledex, filled_row)
  }
}
write_rds(bio_bird2_filledex, "data/pfilledex/bio_bird2_filledex.rds")
bio_bird_filled <- rbind(bio_bird1_filled, bio_bird2_filledex)
write_rds(bio_bird2_filled, "data/pfilled/bio_bird_filled.rds")

################# CHECK ISSUE WITH DISAGGREGATED PROP_PREP IN FISH
problematic_group2 <- bio_fish_combined %>%
  filter(STUDY_ID == 195,
         GENUS_SPECIES == "Caprimulgus vociferus",
         YEAR == 1981)
         # LATITUDE == 39.5167,
         # LONGITUDE == -75.9167,
         # PLOT == "1945",
         # GRAIN_SIZE_TEXT == "50 point counts on a 25 mile transect",
         # GRAIN_SQ_KM == 25.42715,
         # AREA_SQ_KM == 13104786,
         # ABUNDANCE_TYPE == "Count")

print(problematic_group$ABUNDANCE)

summary(problematic_group$ABUNDANCE)

# JUNK FOR MY JANKY CODE
if (curr_year == min_year) {
  #if first column, then prop change value should be 1
  ##code here
} else {
###################### OLD YEAR ON YEAR PROP CHANGE

#Create year-on-year proportional changes
LPD_dat <- LPD_original %>% 
  as.data.frame() %>%
  # Convert "NULL" to NA across columns 34:104
  mutate(across(34:104, ~ifelse(. == "NULL", NA, as.numeric(.)))
         
         # A better version will note that units are different. "Number of breeding pairs", "mean encounter rate/km", "Number of nests", "Number of femals", 
         #"Number of groups per Km??" etc are some weird ones that the current proportional approach works ok with
         # Calculate year-on-year changes. ## A better version will take current_col compared to prev_non-NA_col, rather than only the one directly before it or nothing
         LPD_dat <- LPD_dat %>% 
         #   mutate(across(35:104, 
         #                 .fns = list(d = ~{ #the tilde defines a function that will be applied to each specified column, storing the new columns with names prefixed by d
         #                   current_col <- .
         #                   prev_col <- get(as.character(as.numeric(cur_column()) - 1))
         #                   ifelse(!is.na(current_col) & !is.na(prev_col), 
         #                          (current_col - prev_col) / prev_col, 
         #                          NA)
         #                 }),
         #                 .names = "d_prop_{.col}")) %>% 
           # Calculate relative to earliest non-NA value ##THIS IS THE ONE WE'RE INTERESTED IN. BETTER FOR VISUALISATION, BUT LESS ROBUST TO OUTLIERS
           mutate(across(34:104, 
                         .fns = list(d_init = ~{
                           earliest_non_na <- first(na.omit(.))
                           ifelse(!is.na(.) & !is.na(earliest_non_na),
                                  . / earliest_non_na - 1,
                                  NA)
                         }),
                         .names = "d_init_{.col}")) %>%
           # Rename original columns
           rename_with(~paste0("y_", .), 34:104)
         
         ##create population medians and weightsBio
         LPD_dat <- LPD_dat %>% 
           rowwise() %>%
           mutate(pop_med = median(c_across(34:104), na.rm = TRUE)) %>% 
           ungroup() 
         ##create weights that are uniform across binomial group? No, I want weights to varied across a binomial group, but sum to 1 within a binomial group
         LPD_dat <- LPD_dat %>% 
           group_by(Binomial) %>%
           mutate(init_weight = pop_med / sum(pop_med)) %>%
           ungroup()
         
         ##create binomial-grouped dataset
         # Create LPD_binom dataset
         LPD_binom <- LPD_dat %>% 
           group_by(Binomial, Class, Order, Family, Genus, Species) %>% 
           mutate(across(starts_with("d_init_"), 
                         ~sum(. * init_weight, na.rm = TRUE),
                         .names = "d_init_weighted_{.col}")) %>%
           ungroup()
         
         # Check if init_weight sums to 1 for each Binomial group
         weight_check <- LPD_dat %>%
           group_by(Binomial) %>%
           summarise(weight_sum = sum(init_weight, na.rm = TRUE)) %>%
           mutate(is_sum_one = abs(weight_sum - 1) < 1e-10) %>%   # Using a small tolerance for floating-point arithmetic
           ungroup() 
         
         
         ################################# BioTIME
         # Assuming your dataframe is called 'df'
         Bio_dat <- Bio_original %>%
           group_by(STUDY_ID, YEAR, DAY, MONTH, ID_SPECIES, GENUS, SPECIES, GENUS_SPECIES, 
                    LATITUDE, LONGITUDE) %>%
           summarise(sum.allrawdata.ABUNDANCE = sum(sum.allrawdata.ABUNDANCE, na.rm = TRUE),
                     .groups = 'drop') %>%
           # For columns with differing values that you want to drop, they will be automatically excluded
           ungroup()
         
         # View the result
         View(Bio_dat)
         
         
         
         ##scratch
         print(names(LPD_dat[170]))
         print(which(names(LPD_dat) == "d_init_1950"))
         non_na_count <- sum(!is.na(LPD_dat[,34:104]))
         print(non_na_count))


########################## BIO DATE CHECKS
min(as.numeric(bio_amph$YEAR))
#1976
max(as.numeric(bio_amph$YEAR))
#2016

min(as.numeric(bio_bird$YEAR))
#1923
max(as.numeric(bio_bird$YEAR))
#2015

min(as.numeric(bio_fish1$YEAR))
#1923
max(as.numeric(bio_fish1$YEAR))
#2015
min(as.numeric(bio_fish2$YEAR))
#1961
max(as.numeric(bio_fish2$YEAR))
#2016

min(as.numeric(bio_mam$YEAR))
#1961
max(as.numeric(bio_mam$YEAR))
#2016

min(as.numeric(bio_rep$YEAR))
#1975
max(as.numeric(bio_rep$YEAR))
#2014

min(as.numeric(bio_finv$YEAR))
#1917
max(as.numeric(bio_finv$YEAR))
#2016

min(as.numeric(bio_minv$YEAR))
#1874
max(as.numeric(bio_minv$YEAR))
#2016

min(as.numeric(bio_tinv$YEAR))
#1898
max(as.numeric(bio_tinv$YEAR))
#2018

################################### BIO_AMPH ORIGINAL DATA CHECKS
#check that datasets that share STUDY_ID and GENUS_SPECIES necessarily share LATITUDE, LONGITUDE, PLOT and DEPTH
check_consistency_study <- bio_amph %>%
  group_by(STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
  summarise(
    plot_consistent = n_distinct(PLOT) == 1, ##finding: plot is not consistent on the study_genus_species_latitude_longitude level
    depth_consistent = n_distinct(DEPTH) == 1, ## finding: depth is consistent on the study genus_species_latitude_longitude level
    .groups = 'drop'
  ) %>%
  summarise(
    all_consistent = all(depth_consistent) #plot_consistent)
  )
print(check_consistency)

#check that datasets that share STUDY_ID and GENUS_SPECIES necessarily share LATITUDE, LONGITUDE, PLOT andd DEPTH
check_consistency <- bio_amph %>%
  group_by(GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
  summarise(
    plot_consistent = n_distinct(PLOT) == 1, ##finding: plot is NOT consistent on the genus_species_latitude_longitude level
    depth_consistent = n_distinct(DEPTH) == 1, ## finding: depth is consistent on the genus_species_latitude_longitude level
    .groups = 'drop'
  ) %>%
  summarise(
    all_consistent = all(depth_consistent) #plot_consistent)
  )
print(check_consistency)

# Check with biomas is consistent on the abundance-sbll level. It should be.
check_consistency <- bio_amph %>%
  group_by(ABUNDANCE, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
  summarise(
    biomas_consistent = n_distinct(BIOMAS) == 1,
    .groups = 'drop'
  ) %>%
  summarise(
    all_consistent = all(biomas_consistent) 
  )
print(check_consistency) #result: at the abundance-bll level, amphibians' biomas is consistent

# Check with sample_desc is consistent on the abundance-sbll level. I don't expect it to be. If it isn't, it needs to be dropped, or we need to have...
check_consistency <- bio_amph %>%
  group_by(ABUNDANCE, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
  summarise(
    biomas_consistent = n_distinct(BIOMAS) == 1,
    .groups = 'drop'
  ) %>%
  summarise(
    all_consistent = all(biomas_consistent) 
  )
print(check_consistency) #result: at the abundance-bll level, amphibians' biomas is consistent

#check number of rows when grouped by study and bll
 nrow(group_by(bio_amph, STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 2646
 
#check number of rows when grouped without-study and bll
 nrow(group_by(bio_amph, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 2646
 
 
 ################################### BIO_BIRD ORIGINAL DATA CHECKS
 #check that datasets that share STUDY_ID and GENUS_SPECIES necessarily share LATITUDE, LONGITUDE, PLOT andd DEPTH
 check_consistency_study <- bio_bird %>%
   group_by(STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
   summarise(
     depth_consistent = n_distinct(DEPTH) == 1, ## finding: depth is NOT consistent on the study genus_species_latitude_longitude level
     .groups = 'drop'
   ) %>%
   summarise(
     all_consistent = all(depth_consistent) #plot_consistent)
   )
 print(check_consistency)
 
 #check that datasets that share STUDY_ID and GENUS_SPECIES necessarily share LATITUDE, LONGITUDE, PLOT andd DEPTH
 check_consistency <- bio_bird %>%
   group_by(GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
   summarise(
     depth_consistent = n_distinct(DEPTH) == 1, ## finding: depth is NOT consistent on the genus_species_latitude_longitude level
     .groups = 'drop'
   ) %>%
   summarise(
     all_consistent = all(depth_consistent) #plot_consistent)
   )
 print(check_consistency)
 
 # Check with biomas is consistent on the abundance-sbll level. It should be.
 check_consistency <- bio_bird %>%
   group_by(ABUNDANCE, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
   summarise(
     biomas_consistent = n_distinct(BIOMAS) == 1,
     .groups = 'drop'
   ) %>%
   summarise(
     all_consistent = all(biomas_consistent) 
   )
 print(check_consistency) #result: at the abundance-bll level, birds' biomas is NOT consistent
 
 
 
 #check number of rows when grouped by study and bll
 nrow(group_by(bio_bird, STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 1488060
 
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_bird, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 1488060
 
 
 ##################BIO_FISH ORIGINAL DATA CHECK
 
 #check number of rows when grouped by study and bll
 nrow(group_by(bio_fish1, STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 1488060
 
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_fish1, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 1488060
 
 # Check with biomas is consistent on the abundance-sbll level. It should be.
 check_consistency <- bio_fish1 %>%
   group_by(ABUNDANCE, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
   summarise(
     biomas_consistent = n_distinct(BIOMAS) == 1,
     .groups = 'drop'
   ) %>%
   summarise(
     all_consistent = all(biomas_consistent) 
   )
 print(check_consistency)
 

 #check number of rows when grouped by study and bll
 nrow(group_by(bio_fish2, STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 1192388
 
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_fish2, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 1192388
 
 # Check with biomas is consistent on the abundance-sbll level. It should be.
 check_consistency <- bio_bird %>%
   group_by(ABUNDANCE, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
   summarise(
     biomas_consistent = n_distinct(BIOMAS) == 1,
     .groups = 'drop'
   ) %>%
   summarise(
     all_consistent = all(biomas_consistent) 
   )
 print(check_consistency)
 
############ MAMMAL
 #check number of rows when grouped by study and bll
 nrow(group_by(bio_mam, STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 1192388
 
 #check number of rows when grouped by study and bll
 nrow(group_by(bio_mam, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 1192388
 
 # Check with biomas is consistent on the abundance-sbll level. It should be.
 check_consistency <- bio_mam %>%
   group_by(ABUNDANCE, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
   summarise(
     biomas_consistent = n_distinct(BIOMAS) == 1,
     .groups = 'drop'
   ) %>%
   summarise(
     all_consistent = all(biomas_consistent) 
   )
 print(check_consistency)
 
 ########################## REPTILE
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_rep, STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 4299
 
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_rep, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 4299
 
 # Check with biomas is consistent on the abundance-sbll level. It should be.
 check_consistency <- bio_rep %>%
   group_by(ABUNDANCE, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
   summarise(
     biomas_consistent = n_distinct(BIOMAS) == 1,
     .groups = 'drop'
   ) %>%
   summarise(
     all_consistent = all(biomas_consistent) 
   )
 print(check_consistency)
 
 ######################################## INVERTEBRATES
 
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_finv, STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 92668
 
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_finv, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 92668
 
 # Check with biomas is consistent on the abundance-sbll level. It should be.
 check_consistency <- bio_finv %>%
   group_by(ABUNDANCE, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
   summarise(
     biomas_consistent = n_distinct(BIOMAS) == 1,
     .groups = 'drop'
   ) %>%
   summarise(
     all_consistent = all(biomas_consistent) 
   )
 print(check_consistency)
 
 
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_minv, STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 819066
 
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_minv, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 819066
 
 # Check with biomas is consistent on the abundance-sbll level. It should be.
 check_consistency <- bio_minv %>%
   group_by(ABUNDANCE, GENUS_SPECIES, LATITUDE, LONGITUDE) %>%
   summarise(
     biomas_consistent = n_distinct(BIOMAS) == 1,
     .groups = 'drop'
   ) %>%
   summarise(
     all_consistent = all(biomas_consistent) 
   )
 print(check_consistency)
 
 
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_tinv, STUDY_ID, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 195506
 
 #check number of rows when grouped without-study and bll
 nrow(group_by(bio_tinv, GENUS_SPECIES, LATITUDE, LONGITUDE))
 #result: 195506
 
 
 #################### FOR WRONG METADATA
 #commented out bc this is the wrong metaddata. Has parsing issues where colnames don't align with content
 # bio_amph_mdat <- read_csv("./data/bio_amph_metadata.csv") 
 # bio_bird_mdat <- read_csv("./data/bio_bird_metadata.csv")
 # bio_fish1_mdat <- read_csv("./data/bio_fish1_metadata.csv")
 # bio_fish2_mdat <- read_csv("./data/bio_fish2_metadata.csv")
 # bio_fish_mdat <- rbind(bio_fish1_mdat, bio_fish2_mdat)
 # bio_mam_mdat <- read_csv("./data/bio_mam_metadata.csv")
 # bio_rep_mdat <- read_csv("./data/bio_rep_metadata.csv")
 # bio_finv_mdat <- read_csv("./data/bio_finv_metadata.csv")
 # bio_minv_mdat <- read_csv("./data/bio_minv_metadata.csv")
 # bio_tinv_mdat <- read_csv("./data/bio_tinv_metadata.csv")
 
 # rename foolishly named column (GROUP)
 names(bio_amph_mdat)[names(bio_amph_mdat) == "STUDY_ID...6"] <- "GROUP"
 names(bio_bird_mdat)[names(bio_bird_mdat) == "STUDY_ID...6"] <- "GROUP"
 names(bio_fish1_mdat)[names(bio_fish1_mdat) == "STUDY_ID...6"] <- "GROUP"
 names(bio_fish2_mdat)[names(bio_fish2_mdat) == "STUDY_ID...6"] <- "GROUP"
 names(bio_mam_mdat)[names(bio_mam_mdat) == "STUDY_ID...6"] <- "GROUP"
 names(bio_rep_mdat)[names(bio_rep_mdat) == "STUDY_ID...6"] <- "GROUP"
 names(bio_finv_mdat)[names(bio_finv_mdat) == "STUDY_ID...6"] <- "GROUP"
 names(bio_minv_mdat)[names(bio_minv_mdat) == "STUDY_ID...6"] <- "GROUP"
 names(bio_tinv_mdat)[names(bio_tinv_mdat) == "STUDY_ID...6"] <- "GROUP"
 
 # rename foolishly named column (STUDY_ID)
 names(bio_amph_mdat)[names(bio_amph_mdat) == "STUDY_ID...1"] <- "STUDY_ID"
 names(bio_bird_mdat)[names(bio_bird_mdat) == "STUDY_ID...1"] <- "STUDY_ID"
 names(bio_fish1_mdat)[names(bio_fish1_mdat) == "STUDY_ID...1"] <- "STUDY_ID"
 names(bio_fish2_mdat)[names(bio_fish2_mdat) == "STUDY_ID...1"] <- "STUDY_ID"
 names(bio_mam_mdat)[names(bio_mam_mdat) == "STUDY_ID...1"] <- "STUDY_ID"
 names(bio_rep_mdat)[names(bio_rep_mdat) == "STUDY_ID...1"] <- "STUDY_ID"
 names(bio_finv_mdat)[names(bio_finv_mdat) == "STUDY_ID...1"] <- "STUDY_ID"
 names(bio_minv_mdat)[names(bio_minv_mdat) == "STUDY_ID...1"] <- "STUDY_ID"
 names(bio_tinv_mdat)[names(bio_tinv_mdat) == "STUDY_ID...1"] <- "STUDY_ID"
 
 