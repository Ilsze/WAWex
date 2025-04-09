## This file cleans world bank data
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, foreign, broom, knitr, data.table, zoo) 

########################### CLEAN POP AND GDP DATA ###########################
wb_pop_gdp <- read_xlsx("dat/world_bank/world_bank_pop_gdp.xlsx")

#which GDP metric is most comprehensive? 
# convert .. to NA
wb_pop_gdp <- wb_pop_gdp %>%
  mutate(across(6:12, ~na_if(., ".."))) %>%
  mutate(across(6:12, as.numeric))
#sum the columns
column_sums <- colSums(wb_pop_gdp[6:12], na.rm = TRUE)
max_index <- which.max(column_sums) #returns the third one down
#actually, don't want LCU, so disqualify 3 and 4 and 5
which.max(c(column_sums[1], column_sums[2], column_sums[6])) #returns 2 of 6
#rename that column 
wb_pop_gdp_s <- rename(wb_pop_gdp, GDP_pc = `GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]`, #UTH DON"T USE LOCAL CURR
                     Country = `Country Name`,
                     Country_Code = `Country Code`,
                     Population = `Population, total [SP.POP.TOTL]`, 
                     Year = Time) %>% 
  #select only the relevant cols
  select(Year, Country, Country_Code, Population, GDP_pc) %>% 
  #and ensure unites are correct
  mutate(Population = as.numeric(Population)) %>% #NAs introduced by coercian bc of missing values, it's perfect
  #arrange country then year
  arrange(Country, Year)
  
#Fill missing data using linear interpolation, and for end values, the closest
#known value. 
GDP_dat <- wb_pop_gdp_s %>%
  group_by(Country) %>%
  mutate(
    GDP_filled = na.approx(GDP_pc, x = Year, na.rm = FALSE),
    GDP_filled = na.locf(GDP_filled, na.rm = FALSE),
    GDP_filled = na.locf(GDP_filled, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  ungroup()

#save to file
write.xlsx(GDP_dat, "dat/world_bank/world_bank_pop_gdp_clean.xlsx")







############################################################################
############ WHEN WAS WORKING WITH SEPERATE POP AND GDP DATA LOL ###########
############################################################################

############ clean GDP data ####################################################
GDP_raw <- read_excel("dat/world_bank/world_bank_GDP.xlsx")

#pivot and clean
GDP_dat <- GDP_raw %>%
  pivot_longer(
    cols = "1960 [YR1960]":"2023 [YR2023]",
    names_to = "Year",
    values_to = "GDP"
  ) %>%
  mutate(Year = as.integer(str_sub(Year, 1, -10)), #remove the last nine characters
  GDP = as.numeric(na_if(GDP, ".."))) %>%  # Convert ".." to NA and then to numeric 
  #remove locations with no data
  filter(!Country_Name %in% c("British Virgin Islands", "Gibraltar", 
                              "Korea, Dem. People's Rep.")) %>% 
  arrange(Country_Name, Year) %>%
  group_by(Country_Name) 

#Fill missing data using linear interpolation, and for end values, the closest
#known value. 
GDP_dat <- GDP_dat %>%
  mutate(
    GDP_filled = na.approx(GDP, x = Year, na.rm = FALSE),  # Linear interpolation
    GDP_filled = na.locf(GDP_filled, na.rm = FALSE),       # Fill remaining NAs from last known value
    GDP_filled = na.locf(GDP_filled, fromLast = TRUE, na.rm = FALSE)  # Fill leading NAs from next known value
  ) %>%
  ungroup()

#save to file
write.xlsx(GDP_dat, "dat/world_bank/GDP_world_clean.xlsx")



########## US AND CANADA ONLY (EARLY BIRD SWB ATTEMPT)  #######################

GDP_long <- GDP_dat %>%
  # Filter for just US and Canada
  filter(`Country Name` %in% c("United States", "Canada")) %>%
  # Select only the columns we need
  select(`Country Name`, matches("^\\d{4}$")) %>%  # This keeps country name and Time columns
  # Pivot longer
  pivot_longer(
    cols = matches("^\\d{4}$"),  # All columns that are Times
    names_to = "Time",           # New column for Times
    HFCEs_to = "GDP"            # New column for GDP HFCEs
  )

##add GDP of both countries
GDP_total <- GDP_long %>%
  group_by(Time) %>%
  summarize(GDP = sum(GDP, na.rm = TRUE)) %>%
  arrange(Time)

##save the dataset
write_rds(GDP_total, "dat/world_bank/GDP.rds")






############ clean HFCE data ####################################################
HFCE_dat <- read_csv("dat/world_bank/world_bank_HFCE.csv")

# Keep only rows of interest and select columns
HFCE_long <- HFCE_dat[1:2, 5:68]
# Rename Times
names(HFCE_long) <- sub(" \\[YR\\d{4}\\]", "", names(HFCE_long))
# Convert all columns to character, replace ".." with NA, then convert to numeric
HFCE_long <- HFCE_long %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~na_if(., ".."))) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(Country = c("Canada", "United States"))
# Pivot longer and perform interpolation
HFCE_long <- HFCE_long %>%
  pivot_longer(cols = -Country,
               names_to = "Time",
               values_to = "HFCE") %>%
  mutate(Time = as.numeric(Time)) %>%
  group_by(Country) %>%
  arrange(Time) %>%
  mutate(HFCE = approx(Time, HFCE, Time)$y) %>%
  ungroup()
#extrapolate head and tail missing HFCEs (1960 to 1969; 2023 for the US)
HFCE_long <- HFCE_long %>%
  group_by(Country) %>%
  arrange(Time) %>%
  mutate(HFCE = if_else(is.na(HFCE), 
                        predict(lm(HFCE ~ Time, na.omit(data.frame(Time = Time, HFCE = HFCE))), 
                                newdata = data.frame(Time = Time)),
                        HFCE)) %>%
  ungroup()
#Write HFCE_long to file
write_rds(HFCE_long, "dat/world_bank/HFCE.rds")

# Create the plot to visualise the data by country for sanity check purposes
ggplot(HFCE_long, aes(x = Time, y = HFCE, color = Country)) +
  geom_point() +  # Add points to see exactly where data exists
  geom_line() +   # Add lines to see the trend
  theme_minimal() +
  labs(title = "Household Final Consumption Expenditure Over Time",
       subtitle = "Missing HFCEs shown as gaps in the lines",
       x = "Time",
       y = "HFCE") +
  scale_x_continuous(breaks = seq(1960, 2023, by = 5))  # Make x-axis more readable


