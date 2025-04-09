#This file estimates human welfare levels first globally, then at a disaggregated level. 
library(pacman)
p_load(tidyverse, dplyr, ggplot2, gridExtra, mgcv, nleqslv, png, readr, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)

#load human population per country per year 
un_c5023 <- read_excel("dat/un/un_c5023.xlsx")

#load world GDP per country per year
wb_c6023 <- read_excel("dat/world_bank/GDP_world_clean.xlsx")

######## calibrate isoelastic utility function. #####################
india_2018_GDP <- wb_c6023 %>%
  filter(Country_Name == "India", Year == 2018) %>%
  pull(GDP_filled)

canada_2018_GDP <- wb_c6023 %>% 
  filter(Country_Name == "Canada", Year == 2018) %>% 
  pull(GDP_filled)

india_2018_pop <- un_c5023 %>% 
  filter(Country == "India", Year == 2018) %>% 
  pull(Total)

canada_2018_pop <- un_c5023 %>% 
  filter(Country == "Canada", Year == 2018) %>% 
  pull(Total)

burundi_2018_GDP <- wb_c6023 %>%
  filter(Country_Name == "Burundi", Year == 2018) %>%
  pull(GDP_filled)

burundi_2018_pop <- un_c5023 %>% 
  filter(Country == "Burundi", Year == 2018) %>% 
  pull(Total)

#get GDP per capita
iGDPpc_2018 <- india_2018_GDP/india_2018_pop
cGDPpc_2018 <- canada_2018_GDP/canada_2018_pop
bGDPpc_2018 <- burundi_2018_GDP/burundi_2018_pop #245


#results after using wolfram alpha on india and canada suggest
ubar <- -0.0832451
gamma <- 0.70255
#for a sanity check, this gives us u(245) = 17 welfare score for Burundi, which 
#is startlingly close to the quick analysis of 18/100 that I actually estimated.

######################## WELFARE WEIGHTED RESULTS ##############################
#merge population and gdp datasets
# rename Country_Name in wb_c6023
wb_c6023 <- rename(wb_c6023, Country = Country_Name) 

# rename country values to standardise spelling
# Standardize country names
un_c5023 <- un_c5023 %>%
  mutate(Country_std = str_trim(str_to_lower(Country)))

wb_c6023 <- wb_c6023 %>%
  mutate(Country_std = str_trim(str_to_lower(Country)))

### GIVE UP ON THIS COMBINING DATASET SHIT AND USE WORDL BANK MERGED FUCK


################ CHECK WHAT'S DROPPED #####################
country_name_map <- c(
  "bahamas" = "bahamas, the",
  "bolivia (plurinational state of)" = "bolivia",
  "congo" = "congo, rep.",
  "democratic republic of the congo" = "congo, dem. rep.",
  "côte d'ivoire" = "cote d'ivoire",
  "curaçao" = "curacao",
  "egypt" = "egypt, arab rep.",
  "gambia" = "gambia, the",
  "china, hong kong sar" = "hong kong sar, china",
  "iran (islamic republic of)" = "iran, islamic rep.",
  "republic of korea" = "korea, rep.",
  "kosovo (under unsc res. 1244)" = "kosovo",
  "kyrgyzstan" = "kyrgyz republic",
  "lao people's democratic republic" = "lao pdr",
  "china, macao sar" = "macao sar, china",
  "micronesia (fed. states of)" = "micronesia, fed. sts.",
  "republic of moldova" = "moldova",
  "slovakia" = "slovak republic",
  "saint kitts and nevis" = "st. kitts and nevis",
  "saint lucia" = "st. lucia",
  "saint martin (french part)" = "st. martin (french part)",
  "saint vincent and the grenadines" = "st. vincent and the grenadines",
  "united republic of tanzania" = "tanzania",
  "türkiye" = "turkiye",
  "united states of america" = "united states",
  "venezuela (bolivarian republic of)" = "venezuela, rb",
  "united states virgin islands" = "virgin islands (u.s.)",
  "state of palestine" = "west bank and gaza",
  "yemen" = "yemen, rep."
)

# 1) Convert both dataset’s “Country” into a single standardized column (Country_std)
un_c5023 <- un_c5023 %>%
  mutate(
    Country_std = str_trim(str_to_lower(Country))
  )

wb_c6023 <- wb_c6023 %>%
  mutate(
    Country_std = str_trim(str_to_lower(Country))
  )

# 2) Apply your dictionary to UN’s Country_std to match the WB names
un_c5023 <- un_c5023 %>%
  mutate(
    Country_std = ifelse(
      Country_std %in% names(country_name_map),
      country_name_map[Country_std],
      Country_std
    )
  )

# 3) Now merge on Country_std and Year (instead of Country, Year)
human <- merge(un_c5023, wb_c6023, by = c("Country_std", "Year"))


#### USING COUNTRY NAMES
# 4) If you want to see how many countries lined up:
countries_un  <- unique(un_c5023$Country_std)
countries_wb  <- unique(wb_c6023$Country_std)
countries_both <- intersect(countries_un, countries_wb)

dropped_from_un <- setdiff(countries_un, countries_both)
dropped_from_wb <- setdiff(countries_wb, countries_both)

cat("Countries in wb_c6023 but not in un_c5023:\n")
print(dropped_from_wb)
#Rightfully dropped: channel islands
#UTH what's NA? it's been dropped. hopefully no numbers there lol or we have a pivot problem

cat("\nCountries in un_c5023 but not in wb_c6023:\n")
print(dropped_from_un)






#fail bc un doesnt' fucking parse country codes 
#### USING COUNTRY CODES
un_codes <- unique(un_c5023$Code)
wb_codes <- unique(wb_c6023$`Country Code`)
codes_in_both <- intersect(un_codes, wb_codes)
dropped_from_un <- setdiff(un_codes, codes_in_both)
dropped_from_wb <- setdiff(wb_codes, codes_in_both)

