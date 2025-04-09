##what is the minimal definition of each row? 
LPD_grouped <- LPD_original %>% #original has 32,680
  summarise(unique_combinations = n_distinct(Binomial, Latitude, Longitude, Citation))
print(LPD_grouped$unique_combinations) #only has 23,962


bio_grouped <- bio %>% #original has 6,475,081
  summarise(unique_combinations = n_distinct(GENUS_SPECIES, ABUNDANCE, YEAR, LATITUDE, 
     LONGITUDE, STUDY_ID, SAMPLE_DESC)) #the only vars left out were PLOT and DEPTH and BIOMAS
print(bio_grouped$unique_combinations) #only has 3,759,996

#check if consistent
inconsistent_groups <- bio %>%
  group_by(GENUS_SPECIES, ABUNDANCE, YEAR, LATITUDE, 
           LONGITUDE, STUDY_ID, SAMPLE_DESC) %>%
  summarise(
    plot_consistent = n_distinct(PLOT) == 1,
    depth_consistent = n_distinct(DEPTH) == 1,
    biomas_consistent = n_distinct(BIOMAS) == 1,
    .groups = 'drop'
  ) %>%
  filter(!plot_consistent | !depth_consistent | !biomas_consistent)

print(inconsistent_groups) #9913 row for which all the above is shared, but something else differs
