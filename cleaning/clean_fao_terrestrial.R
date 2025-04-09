## This file cleans fao data about terrestrial livestock numbers to derive a global aggregated 
##trend and also identify the most-farmed animals. Swine and pigs are already 
##aggregated, but poultry (ducks, geese, chickens) are disaggregated
## Data is continuous from 1961 to 2023.
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)  

livestock <- read_csv("dat/fao/FAOSTAT_data_en_2-21-2025.csv")

livestock_clean <- livestock %>%
  ## remove China to avoid overlap with "China, Hong Kong SAR", "China, Macao SAR"                                    
 ## "China, mainland", and "China, Taiwan Province of" 
  filter(Area != "China") %>% 
#update bee data to be individuals, not colonies, taking 17750 bees per colony 
  ## First update "No" unit to "An" 
  mutate(Unit = if_else(Item == "Bees", "An", Unit),
         Value = if_else(Item == "Bees", 17750*Value, Value),
#convert Items with "1000 An" units to "An" units
         Value = if_else(Unit == "1000 An", Value * 1000, Value),
         Unit = if_else(Unit == "1000 An", "An", Unit)) 
#save 
write.xlsx(livestock_clean, "dat/fao/fao_terrestrial_livestock_clean.xlsx")

#aggregate country to global levels
livestock_g_clean <- livestock_clean %>% 
  group_by(Year, Item) %>% 
  summarise(Value= sum(Value, na.rm = TRUE), .groups = "drop")

#write global version 
write.xlsx(livestock_g_clean, "dat/fao/fao_terrestrial_livestock_global_clean.xlsx")


############# FOR COPYING INTO CALCULATIONS #############################
livestock_g_clean <- read_excel("dat/fao/fao_terrestrial_livestock_global_clean.xlsx")
#keep only Item, Year, and Value
livestock_4_calc <- select(livestock_g_clean, c(Item, Year, Value)) %>% 
  #ascribe welfare potential
  mutate(Welfare_potential = case_when(
    Item == "Bees" ~ 0.071,
    Item %in% c("Chickens", "Ducks", "Geese", "Other birds", "Turkeys") ~ 0.327,
    Item %in% c("Asses", "Buffalo", "Camels", "Cattle", "Goats", "Horses", 
                "Mules and hinnies", "Other camelids", "Rabbits and hares", "Sheep") ~ 0.4195,
    Item %in% c("Other rodents", "Swine / pigs") ~ 0.512,
    TRUE ~ NA_real_
  )) %>% 
  arrange(Item, Year)
##TODO at some point: consider smoothing

#write to file
write.xlsx(livestock_4_calc, "dat/fao/fao_terrestrial_livestock_for_calculations.xlsx")


###################### FOR VIEWING PURPOSES ############################
#View only 2023 data
livestock_clean_2023 <- livestock_clean %>% 
  filter(Year == 2023)


#chicken
ggplot(filter(livestock_4_calc, Item == "Chickens"), aes(x = Year, y = Value)) +
  geom_point() +
  labs(
    title = "Chicken Population Over Time",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal()
#cattle
ggplot(filter(livestock_4_calc, Item == "Cattle"), aes(x = Year, y = Value)) +
  geom_point() +
  labs(
    title = "Cattle Population Over Time",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal()
#pigs and swine
ggplot(filter(livestock_4_calc, Item == "Swine / pigs"), aes(x = Year, y = Value)) +
  geom_point() +
  labs(
    title = "Pigs and Swine Population Over Time",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal()
#bees
ggplot(filter(livestock_4_calc, Item == "Bees"), aes(x = Year, y = Value)) +
  geom_point() +
  labs(
    title = "Bees Population Over Time",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal()
#goats
ggplot(filter(livestock_4_calc, Item == "Goats"), aes(x = Year, y = Value)) +
  geom_point() +
  labs(
    title = "Goats Population Over Time",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal()
#asses
ggplot(filter(livestock_4_calc, Item == "Asses"), aes(x = Year, y = Value)) +
  geom_point() +
  labs(
    title = "Asses Population Over Time",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal()








######### TO MOVE TO SCRATCH_FAO ##########################
#TO CONSIDER: clean countries
#Areas that don't officially exist before a certain time simply don't have 
# entries with that year, e.g South Sudan recognised in 2011
# To make it so that an Area range goes from the very beginning of the dataset to
# its end, should consider mutating Area to be "modern day Area", e.g Sudan (former)
# would be broken into present-Sudan and present-SouthSudan and combined with 2012+ data

#identify animals which have "No" as their unit
unique_items <- livestock %>%
  filter(Unit == "No") %>%
  distinct(Item) %>%
  pull(Item)
## only bees

# Subset dataset for Bees, keeping only Area (e.g country) and Value, and 
## ordering by Value
livestock_bee <- livestock %>%
  filter(Item == "Bees", Year == 2023) %>%
  arrange(desc(Value))
#check cleaned dataset
livestock_clean_bee <- livestock_clean %>%
  filter(Item == "Bees", Year == 2023) %>%
  arrange(desc(Value))
#this tells us that the countries with the most colonies(?) are india, china, tukiye, iran, and ethiopia
##some research (see WAWex working doc) tells us that there are about 17750 bees per colony. 
## Total number of bees is 111,515,297 in 2023
##Compare with Total number of insects for food and feed globally in about 2020: 86 billion

#Next, choose a common unit. Is there an An entry for every 1000 An entry or vice versa? 
# Find out which Items appear in what Units
item_units <- livestock_clean %>%
  distinct(Item, Unit) %>%
  arrange(Item)
# View the result
print(item_units)
# turns out units are distinct across animals -- there's no item with rows in both "An" and "1000 An"

#Most numerous farmed species
twentythree_by_value <- livestock_clean %>% 
  filter(Year == 2023) %>% 
  arrange(desc(Value)) 
unique(twentythree_by_value$Item)
#results say Bees > Chickens > Ducks > Swine/pigs > Geese > Cattle > Sheep > Goats... 

#check missing Values
missing_values <- livestock_clean %>%
  filter(is.na(Value))
# Display missing values
View(missing_values) 

#Find the year bounds for all unusual areas
# Define all unusual areas (including broad and granular regions)
unusual_areas <- c(
  "China", "China, mainland", "China, Hong Kong SAR", "China, Macao SAR", "China, Taiwan Province of",
  "Sudan (former)", "Sudan", "South Sudan", 
  "Serbia and Montenegro", "Serbia", "Montenegro", 
  "Czechoslovakia", "Czechia", "Slovakia", 
  "Belgium-Luxembourg", "Belgium", "Luxembourg",
  "USSR", "Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia", "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", "Republic of Moldova", "Russian Federation", "Tajikistan", "Turkmenistan", "Ukraine", "Uzbekistan",
  "Yugoslav SFR", "Bosnia and Herzegovina", "Croatia", "Montenegro", "North Macedonia", "Serbia", "Serbia and Montenegro", "Slovenia",
  "Ethiopia PDR", "Ethiopia"
)
# Get year availability range for each unusual area
year_ranges <- livestock_clean %>%
  filter(Area %in% unusual_areas) %>%
  group_by(Area) %>%
  summarise(
    first_year = min(Year, na.rm = TRUE),
    last_year = max(Year, na.rm = TRUE),
    .groups = "drop"
  )
# Manually order results by extracting rows in the order of `unusual_areas`
year_ranges_ordered <- map_dfr(unusual_areas, ~ year_ranges %>% filter(Area == .x))
# View results
View(year_ranges_ordered)

# Optionally, show in RStudio's viewer
View(year_ranges)



