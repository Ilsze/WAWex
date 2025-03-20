## This file cleans world bank data
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, foreign, broom, knitr, data.table) 

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


############ clean GDP data ####################################################


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
