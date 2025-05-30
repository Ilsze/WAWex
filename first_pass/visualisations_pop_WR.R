#visualise population and utility time series by category (animal group)
#It uses calc_tseries as processed after analysis.R
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, pdf, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)  

calc_tseries <- read_excel("first_pass/calc_tseries.xlsx")
net_series <- read_excel("first_pass/net_series.xlsx") 

# Remove rows with NA utility if needed
filtered_data <- calc_tseries %>%
  filter(!is.na(utility), !is.na(aliveatanytime)) %>% 
  mutate(Category = recode(Category, "Fishes farmed for food (excludes pre-slaughter mortality)" = "Fish for Slaughter"))

############## HUMAN AND SELECTED CAPTIVE ANIMALS ############################

# Plot 1: Population over time
ggplot(filtered_data, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
  geom_line() +
  labs(title = "Population Over Time - Selected Captive Animals", y = "Population (alive at any time)", x = "Year") +
  theme_minimal()
ggsave("first_pass/population_trends/population_trends_f.pdf", width = 10, height = 6)

# Plot 2: Utility over time
ggplot(filtered_data, aes(x = Year, y = utility, colour = Category, group = interaction(Group, Category))) +
  geom_line() +
  labs(title = "Utility Over Time - Selected Captive Animals", y = "Utility", x = "Year") +
  theme_minimal()
ggsave("first_pass/utility_trends_f.pdf", width = 10, height = 6)

############## HUMAN AND SELECTED CAPTIVE ANIMALS NO BEES ######################
hscnb <- filtered_data %>% 
  filter(!Category == "Bees")
View(hscnb)

# Plot 1: Population over time
ggplot(hscnb, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
  geom_line() +
  labs(title = "Population Over Time - Selected Captive Animals, No Bees", y = "Population (alive at any time)", x = "Year") +
  theme_minimal()
ggsave("first_pass/population_trends/population_trends_f_nb.pdf", width = 10, height = 6)

# Plot 2: Utility over time
ggplot(hscnb, aes(x = Year, y = utility, colour = Category, group = interaction(Group, Category))) +
  geom_line() +
  labs(title = "Utility Over Time - Selected Captive Animals, No Bees", y = "Utility", x = "Year") +
  theme_minimal()
ggsave("first_pass/utility_trends_f_nb.pdf", width = 10, height = 6)


############## HUMAN AND SELECTED CAPTIVE ANIMALS NO BEES NO FISH ##############
hscnbf <- filtered_data %>% 
  filter(!Category == "Bees", !Category == "Fish for Slaughter")

# Plot 1: Population over time
ggplot(hscnbf, aes(x = Year, y = aliveatanytime, colour = Category, group = interaction(Group, Category))) +
  geom_line() +
  labs(title = "Population Over Time - Selected Captive Animals, No Bees or Fish", y = "Population (alive at any time)", x = "Year") +
  theme_minimal()
ggsave("first_pass/population_trends/population_trends_f_nbf.pdf", width = 10, height = 6)

# Plot 2: Utility over time
ggplot(hscnbf, aes(x = Year, y = utility, colour = Category, group = interaction(Group, Category))) +
  geom_line() +
  labs(title = "Utility Over Time - Selected Captive Animals, No Bees or Fish", y = "Utility", x = "Year") +
  theme_minimal()
ggsave("first_pass/utility_trends_f_nbf.pdf", width = 10, height = 6)


############    NET UTILITY   ##########
### Humans and selected farm animals (includes bees and fish)
ggplot(net_utility, aes(x = Year, y = net_utility)) +
  geom_line(na.rm = FALSE) + #na.rm false creates breaks in the line where values are missing
  geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +  # light horizontal line at y = 0
  labs(title = "Net Utility Over Time - Humans and Selected Captive Animals", 
       y = "Net Utility", 
       x = "Year") + 
  theme_minimal()
ggsave("first_pass/net_utility_trends_f.pdf", width = 10, height = 6)

### Humans and selected farm animals (excludes bees, fish, and "other birds")
#create net utility without bees, fish, and "other birds", which has data starting 1991
hscnbfob <- hscnbf %>% 
  filter(!Category == "Other birds")

# Step 1: Get min and max years for each Category
year_bounds_nbfob <- hscnbfob %>%
  group_by(Category) %>%
  summarise(min_year = min(Year, na.rm = TRUE),
            max_year = max(Year, na.rm = TRUE))

# Step 2: Find the overlapping year range
nbfob_min_year <- max(year_bounds_nbfob$min_year)  # latest starting year
nbfob_max_year <- min(year_bounds_nbfob$max_year)  # earliest ending year

# Step 3: Filter to that overlapping year range
hscnbfob_trimmed <- hscnbfob %>%
  filter(Year >= nbfob_min_year, Year <= nbfob_max_year)

# Step 4: Calculate net utility
net_utility_nbfob <- hscnbfob_trimmed %>%
  group_by(Year) %>%
  summarise(net_utility = sum(utility))  # default na.rm = FALSE makes any years with NA values resolve to NA

ggplot(net_utility_nbfob, aes(x = Year, y = net_utility)) + 
  geom_line(na.rm = FALSE) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +  # light horizontal line at y = 0
  labs(title = "Net Utility Over Time - Humans and Selected Captive Animals, No Bees, Fish, or Other birds", 
       y = "Net Utility", 
       x = "Year") + 
  theme_minimal()
ggsave("first_pass/net_utility_trends_f_nbfob.pdf", width = 10, height = 6)

### Plot both net utilities on the same graph
# Add labels to each dataset
net_utility <- net_utility %>% mutate(Group = "Includes Bees & Fish & Other birds")
net_utility_nbfob <- net_utility_nbfob %>% mutate(Group = "Excludes Bees & Fish & Other birds")
# Combine the two datasets
net_utility_combined <- bind_rows(net_utility, net_utility_nbfob)
# Plot combined data
ggplot(net_utility_combined, aes(x = Year, y = net_utility, color = Group)) +
  geom_line(na.rm = FALSE) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Net Utility Over Time - Comparison",
       y = "Net Utility", 
       x = "Year",
       color = "Dataset") +
  theme_minimal()
ggsave("first_pass/net_utility_comparison.pdf", width = 10, height = 6)

      
     