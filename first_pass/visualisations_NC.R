#visualise population and utility time series by category (animal group)
#It uses calc_tseries as processed after analysis.R. Includes more variables 
# NC_pop, WR_pop, NC_utility, and WR_utility
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, pdf, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)  

calc_tseries <- read_excel("first_pass/calc_tseries.xlsx")
net_series <- read_excel("first_pass/net_series.xlsx") 

# # Remove rows with NA utility if needed
# filtered_data <- calc_tseries %>%
#   filter(!is.na(utility), !is.na(aliveatanytime))

############## A. OPULATION AND UTILITY BY CATEGORY ############################
# Plot 3: NC Utility over time (by category)
ggplot(calc_tseries, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
  geom_line(na.rm = TRUE) + #the default setting, where NA values give line breaks. this just supresses the warning
  labs(title = "Utility Over Time - Neuron Count method", y = "Utility", x = "Year") +
  theme_minimal()
ggsave("first_pass/NC_graphs/A3_NC_utility_trends_f.pdf", width = 10, height = 6)

#Plot 4: NC Utility over time -- no humans (by category)
cts_no_humans <- calc_tseries %>% 
  filter(!Category == "Humans") 
ggplot(cts_no_humans, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
  geom_line(na.rm = TRUE) + #the default setting, where NA values give line breaks. this just supresses the warning
  labs(title = "Utility Over Time - Neuron Count method - No Humans", y = "Utility", x = "Year") +
  theme_minimal()
ggsave("first_pass/NC_graphs/A4_NC_utility_trends_f_nh.pdf", width = 10, height = 6)

#Plot 5: NC Utility over time -- no humans, no fish (by category)
cts_no_humans_fish <- cts_no_humans %>% 
  filter(!Category == "Fish") 
ggplot(cts_no_humans_fish, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
  geom_line(na.rm = TRUE) + #the default setting, where NA values give line breaks. this just supresses the warning
  labs(title = "Utility Over Time - Neuron Count method - No Humans, No Fish", y = "Utility", x = "Year") +
  theme_minimal()
ggsave("first_pass/NC_graphs/A5_NC_utility_trends_f_nhf.pdf", width = 10, height = 6)

#Plot 6: NC Utility over time -- no humans, no fish, now chicken (by category)
cts_no_humans_fish_chickens <- cts_no_humans_fish %>% 
  filter(!Category == "Chickens") 
ggplot(cts_no_humans_fish_chickens, aes(x = Year, y = NC_utility, colour = Category, group = interaction(Group, Category))) +
  geom_line(na.rm = TRUE) + #the default setting, where NA values give line breaks. this just supresses the warning
  labs(title = "Utility Over Time - Neuron Count method - No Humans, No Fish", y = "Utility", x = "Year") +
  theme_minimal()
ggsave("first_pass/NC_graphs/A6_NC_utility_trends_f_nhfc.pdf", width = 10, height = 6)




############    B. NET NC UTILITY   ##########
### Humans and selected farm animals (encompassing)
ggplot(net_series, aes(x = Year, y = net_NC_utility)) +
  geom_line(na.rm = FALSE) + #na.rm regardless the setting = T or F, missing values are removed. = FALSE issues a warning. = TRUE suppresses
  geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +  # light horizontal line at y = 0
  labs(title = "Net Utility Over Time - Neuron Count method", 
       y = "Net Utility", 
       x = "Year") + 
  theme_minimal()
ggsave("first_pass/NC_graphs/B1_net_NC_utility_trends_f.pdf", width = 10, height = 6)

##Humans and non humans on separate lines
# Merge with full join to keep all years
combined_series <- full_join(
  net_series %>% select(Year, net_NC_utility_all = net_NC_utility),
  net_series_nh %>% select(Year, net_NC_utility_no_human = net_NC_utility),
  by = "Year"
)

# Reshape to long format for ggplot
long_series <- combined_series %>%
  pivot_longer(cols = starts_with("net_NC_utility"), 
               names_to = "Series", 
               values_to = "Net_Utility") %>%
  mutate(
    Series = recode(Series,
                    "net_NC_utility_all" = "With Humans",
                    "net_NC_utility_no_human" = "Without Humans")
  )

# Plot with ggplot
ggplot(long_series, aes(x = Year, y = Net_Utility, color = Series)) +
  geom_line(na.rm = TRUE) +  # This will skip NAs at the ends without error
  geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Net Utility Over Time - Neuron Count Method",
       y = "Net Utility",
       x = "Year",
       color = "Series") +
  theme_minimal()

# Save
ggsave("first_pass/NC_graphs/B2_net_utility_comparison.pdf", width = 10, height = 6)

      
     