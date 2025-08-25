
library(pacman)
p_load(dplyr, R.matlab, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, tidyverse, readxl, openxlsx, foreign, broom, knitr, data.table, dlm, 
       patchwork, hrbrthemes, scales)


manually_curated_data <- read_csv("greenspoon/manually_curated_data.csv")
marine_species <- read_csv("greenspoon/marine_mammal_species.csv")
gen_length <- read_csv("greenspoon/gen_length_data.csv")

mc_dat <- manually_curated_data %>% 
  select("Species ID", "Order", "Family", "Genus", "Species",
         "Authority", "Synonyms", "Common names (Eng)", "Red List status", 
         "Red List criteria", "Year assessed", "Population trend", 
         "Extracted total population...33",  "mature/total/partial...32", 
         "Population details", "Extracted population decline", 
         "Population decline years", "Population decline details") %>% 
  # remove rows for which both Extracted population decline and population decline 
  # years are both missing
  filter(!(is.na(`Extracted population decline`) & is.na(`Population decline years`)
           & is.na(`Population decline details`))) %>% 
  # Create binomial column in mc_dat 
  mutate(Binomial = paste(Genus, Species, sep = " ")) %>% 
  # Merge gen_length
  merge(gen_length, 
        by.x = "Binomial",
        by.y = "binomial",
        all.x = TRUE) #keeps that which is in mc_dat but doesn't have gen_length data

# separate datasets into marine and terrestrial
# Split into marine and terrestrial datasets
mc_marine <- mc_dat %>%
  filter(Binomial %in% marine_species$Binomial)

mc_terrestrial <- mc_dat %>%
  filter(!Binomial %in% marine_species$Binomial)

mc_bat_rodent <- mc_terrestrial %>% 
  filter(Order %in% c("CHIROPTERA", "RODENTIA"))
  
pop_decline_details_for_claude <- mc_dat %>% 
  select("Species ID", "Red List status", "Population trend", 
         "Extracted population decline", "Population decline years", 
         "Population decline details", "gen_length_d")

### load manually cleaned bat_rodent data
mc_br_regex <- read_xlsx("greenspoon/mc_bat_rodent_to_regex.xlsx")

# get remaining mammal data
mc_t_other <- mc_terrestrial %>%
  filter(!`Species ID` %in% mc_br_regex$`Species ID`) %>% 
  filter(`Red List status` != "DD") %>%
  mutate(EPD_point = case_when(
    `Red List status` == "CR" ~ 0.9,
    `Red List status` == "EN" ~ 0.6,
    `Red List status` == "VU" ~ 0.4,
    `Red List status` == "NT" ~ 0.25,
    `Red List status` == "LC" ~ 0.1,
    TRUE ~ NA_real_  # for any other values
  ))

# Produce clean dataset
mc_t_clean <- bind_rows(
  mc_br_regex %>% mutate(EPD_point = as.numeric(EPD_point)),
  mc_t_other) %>% 
  #populate empty APDY_num with 13.682, the average APDY_num in the rows looked over carefully
  mutate(APDY_num = ifelse(is.na(APDY_num), 13.682, APDY_num))

# Clean "increasing" by hand
mc_t_medit <- read_xlsx("greenspoon/mc_t_clean_medit.xlsx")

#calculate constant_k
mc_t <- mc_t_medit %>% 
  mutate(constant_k = log(EPD)/APDY_num)

#bring in binomials with population estimates (terrestrial) for weighting
pop_dat <- read_csv("greenspoon/species_w_pop_reports.csv") # 392 rows - the species for which direct pop reports were available
pop_dat2 <- read_csv("greenspoon/wild_land_mammal_biomass_inluding_populations.csv") # 4795 the above + the species for which pop reports were predicted

#create mc_t dataset with pop reports
mc_t_pop <- merge(mc_t, pop_dat, 
                  by.x = "Binomial",
                  by.y = "binomial",
                  all.x = FALSE) #removes the rows in mc_t that don't have a match in pop_dat
                  # gives 175 entries
#dismiss because sorting by population gives way too many CETARTIODACTYLA order animals

mc_t_pop2 <- merge(mc_t, pop_dat2, 
                  by.x = "Binomial",
                  by.y = "binomial",
                  all.x = FALSE) #removes the rows in mc_t that don't have a match in pop_dat
                  # gives 925 entries
#sorting by pop is good sanity check - much Chiroptera and rodentia, as expected

#now find weighted average constant_k
k_bar <- weighted.mean(mc_t_pop2$constant_k, mc_t_pop2$estimated_population) # k = -0.0106 Population decreases by about 1% per year


write.xlsx(mc_terrestrial, "greenspoon/mc_terrestrial.xlsx")
write.xlsx(mc_bat_rodent, "greenspoon/mc_bat_rodent.xlsx")
write.xlsx(mc_t_other, "greenspoon/mc_t_other.xlsx")
write.xlsx(mc_t_clean, "greenspoon/mc_t_clean.xlsx")


####################1
#. MARINE MAMMALS #
###################
# Load the .mat file
data <- readMat("/Users/jojos/Documents/GitHub/WAWex/dat/NatComm_DataCode/data/species/Level12_categories.mat")

str(data, max.level = 2)  # Limit depth to avoid overwhelming output
names(data)  # See all the variable names

# Check dimensions of each element
lapply(data, function(x) if(is.array(x) || is.matrix(x)) dim(x) else length(x))

# Look at the largest array (likely your main data)
largest_element <- which.max(sapply(data, function(x) 
  if(is.array(x)) prod(dim(x)) else length(x)))
dim(data[[largest_element]])


# Next steps
# get remaining no-need-to-regex terrestrial mammal files and run different process
# rbind



small_test_for_claude <- pop_decline_details_for_claude[c(1, 13:15), ]
entire_task_for_claude <- pop_decline_details_for_claude[c(1, 16:nrow(pop_decline_details_for_claude)),]

###### PREPARING TO WORK WITH CLAUDE
write.xlsx(pop_decline_details_for_claude, "greenspoon/population_decline_details_for_claude.xlsx")
write_csv(pop_decline_details_for_claude, "greenspoon/population_decline_details_for_claude.csv")

write.xlsx(small_test_for_claude, "greenspoon/small_test_for_claude.xlsx")
write_csv(small_test_for_claude, "greenspoon/small_test_for_claude.csv")



write.xlsx(entire_task_for_claude, "greenspoon/entire_task_for_claude.xlsx")
write_csv(entire_task_for_claude, "greenspoon/entire_task_for_claude.csv")


##investigating data for manual cleaning
# What are the unique population trends that occur in the data? 
unique(pop_decline_details_for_claude$`Population trend`)
#species fixed: 71733227, 39833
# Of the data with non-LC status, is population trend ever stable or increasing? 
# Where are there inappropriate inequality signs in the Population decline years column, indicating flipped columns? 
# Decide not to flag if generation length data is needed. too much work


