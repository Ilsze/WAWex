#This file estimates human welfare levels first globally, then at a disaggregated level. 
#GDP units are in "GDP per capita, PPP (current international $)".
#Its most notable outputs are human_wWL_isoelastic.xlsx and human_wWL_3282.xlsx
##This file feeds into analysis_WR.R and analysis_NC.R files

library(pacman)
p_load(tidyverse, dplyr, ggplot2, gridExtra, mgcv, nleqslv, png, readr, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table)

#load data
wb_6023 <- read_excel("dat/world_bank/world_bank_pop_gdp_clean.xlsx")

#pull anchor data
india_2018_GDP <- wb_6023 %>%
  filter(Country == "India", Year == 2018) %>%
  pull(GDP_filled)
# 6714.966

canada_2018_GDP <- wb_6023 %>% 
  filter(Country == "Canada", Year == 2018) %>% 
  pull(GDP_filled)
# 49982.6

burundi_2018_GDP <- wb_6023 %>%
  filter(Country == "Burundi", Year == 2018) %>%
  pull(GDP_filled)
# 753.4555

##################### TWO-CATEGORY aka 32-82 METHOD  #####################
#Use log midpoint to find threshold
threshold <- exp((log(india_2018_GDP)+log(canada_2018_GDP))/2)
#log accounts for the diminishing returns of income on welfare

######sanity check and visualisation of threshold choice.
#plot a frequency table of wb_6023 for which year = 2018 and input
#a vertical line for threshold

# Function to plot GDP distribution with threshold and save as PDF
# Example usage:
# To generate for 2018:
# plot_gdp_distribution(wb_6023, 2018, threshold)
#
# For a different year:
# plot_gdp_distribution(wb_6023, 1990, threshold)
#
# To display but not save:
# plot_gdp_distribution(wb_6023, 2018, threshold, save_pdf = FALSE)
plot_gdp_distribution <- function(data, year, threshold, 
                                  reference_countries = list(
                                    "India" = india_2018_GDP,
                                    "Canada" = canada_2018_GDP,
                                    "Burundi" = burundi_2018_GDP),
                                  output_dir = "human_welfare_level_visuals",
                                  save_pdf = TRUE) {
  
  # Create output directory if it doesn't exist
  if(save_pdf && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Filter data for specified year
  year_data <- data %>% 
    filter(Year == year)
  
  # Calculate probability mass on each side of threshold
  below_threshold <- sum(year_data$GDP_filled < threshold, na.rm = TRUE)
  above_threshold <- sum(year_data$GDP_filled >= threshold, na.rm = TRUE)
  total_countries <- nrow(year_data)
  
  below_percent <- below_threshold / total_countries * 100
  above_percent <- above_threshold / total_countries * 100
  
  # Calculate histogram counts for y-position scaling
  hist_data <- hist(year_data$GDP_filled, breaks = 30, plot = FALSE)
  max_count <- max(hist_data$counts)
  
  # Create histogram plot
  p <- ggplot(year_data, aes(x = GDP_filled)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
    geom_vline(xintercept = threshold, color = "red", linetype = "dashed", size = 1) +
    annotate("text", x = threshold*1.1, y = max_count*0.9, 
             label = paste("Threshold =", round(threshold, 2)), 
             color = "red", hjust = 0) +
    # Add probability mass labels
    annotate("label", x = threshold/3, y = max_count*0.8, 
             label = paste0(round(below_percent, 1), "% of countries\n(", below_threshold, " countries)"), 
             color = "darkblue", alpha = 0.8) +
    annotate("label", x = threshold*5, y = max_count*0.8, 
             label = paste0(round(above_percent, 1), "% of countries\n(", above_threshold, " countries)"), 
             color = "darkblue", alpha = 0.8) +
    scale_x_log10(labels = scales::comma) +
    labs(title = paste("Distribution of GDP per Capita (", year, ")", sep = ""),
         subtitle = paste("Log midpoint threshold between India and Canada:", round(threshold, 2)),
         x = "GDP per Capita (log scale)",
         y = "Count of Countries") +
    theme_minimal()
  
  # Add reference country labels if they exist in the data for that year
  if(length(reference_countries) > 0) {
    for(country_name in names(reference_countries)) {
      # Check if country's GDP is actually visible on the plot (within reasonable limits)
      if(reference_countries[[country_name]] > min(year_data$GDP_filled, na.rm = TRUE) &
         reference_countries[[country_name]] < max(year_data$GDP_filled, na.rm = TRUE) * 2) {
        p <- p + annotate("text", x = reference_countries[[country_name]], y = 5, 
                          label = country_name, color = "darkgreen")
      }
    }
  }
  
  # Save the plot if requested
  if(save_pdf) {
    filename <- file.path(output_dir, paste0("distribution_GDP_per_capita_", year, ".pdf"))
    ggsave(filename, plot = p, width = 10, height = 6)
    cat("Plot saved as:", filename, "\n")
  }
  
  # Return the plot
  return(p)
}

###### Get a table of countries which today have higher GDP per capita than the 
#threshold
# Find the most recent year in the dataset
most_recent_year <- max(wb_6023$Year)

# Filter data for most recent year
recent_data <- wb_6023 %>% 
  filter(Year == most_recent_year)

# Calculate countries above Canada's 2018 GDP in the most recent year
canada_2018_gdp_threshold <- 49982.6
countries_above_threshold <- recent_data %>%
  filter(GDP_filled >= canada_2018_gdp_threshold) %>%
  select(Country, GDP_filled) %>%
  arrange(desc(GDP_filled))

# Print results
cat("Countries with GDP per capita ≥", canada_2018_gdp_threshold, 
    "in", most_recent_year, ":\n")
print(countries_above_threshold)

# Count of countries above threshold
cat("\nTotal number of countries above threshold:", nrow(countries_above_threshold),
    "out of", nrow(recent_data), "countries in the dataset for", most_recent_year, "\n")


####### Get the proportion and country names for the countries in 1990 which had a higher GDP per capita
#than 2018's Canada
## These turned out to be United Arab Emirates, Cayman Islands, Brunei Darussalam, and Qatar
# Filter data for year 1990
data_1990 <- wb_6023 %>% 
  filter(Year == 1990)

# Calculate proportion of countries above Canada's 2018 GDP in 1990
canada_2018_gdp_threshold <- 49982.6
countries_above_threshold <- data_1990 %>%
  filter(GDP_filled >= canada_2018_gdp_threshold)

# Get count and percentage
count_above <- nrow(countries_above_threshold)
total_countries_1990 <- nrow(data_1990)
percent_above <- count_above / total_countries_1990 * 100

# Print results
cat("In 1990, there were", count_above, "countries with GDP per capita ≥", 
    canada_2018_gdp_threshold, "\n")
cat("This represents", round(percent_above, 2), "% of all", total_countries_1990, 
    "countries in the dataset for 1990\n")

# Create a visualization to compare
ggplot(data_1990, aes(x = GDP_filled)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white", alpha = 0.7) +
  geom_vline(xintercept = canada_2018_gdp_threshold, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = canada_2018_gdp_threshold*1.1, y = 20, 
           label = paste("Canada's 2018 GDP =", round(canada_2018_gdp_threshold, 2)), 
           color = "red", hjust = 0) +
  annotate("label", x = canada_2018_gdp_threshold*3, y = max(hist(data_1990$GDP_filled, breaks = 30, plot = FALSE)$counts)*0.8, 
           label = paste0(round(percent_above, 2), "% of countries\n(", count_above, " countries)"), 
           color = "darkblue", alpha = 0.8) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Distribution of GDP per Capita (1990)",
       subtitle = paste("Proportion of countries with GDP ≥ Canada's 2018 level (", round(canada_2018_gdp_threshold, 2), ")"),
       x = "GDP per Capita (log scale)",
       y = "Count of Countries") +
  theme_minimal()

# Let's also get the list of these countries for reference
high_gdp_countries_1990 <- countries_above_threshold %>%
  select(Country, GDP_filled) %>%
  arrange(desc(GDP_filled))

print(high_gdp_countries_1990)

# Calculate aggregate total welfare using the 32-82 method
calculate_aggregate_welfare <- function(data, threshold) {
  # Create a new column with welfare value based on GDP threshold
  welfare_data <- data %>%
    mutate(welfare_points = ifelse(GDP_filled <= threshold, 32, 82)) %>%
    mutate(country_welfare = Population * welfare_points)
  
  # Aggregate by year
  total_welfare_by_year <- welfare_data %>%
    group_by(Year) %>%
    summarize(Total_welfare = sum(country_welfare, na.rm = TRUE),
              Total_population = sum(Population, na.rm = TRUE),
              Avg_welfare_per_capita = Total_welfare / Total_population) %>%
    ungroup()
  
  return(total_welfare_by_year)
}

# Calculate the aggregate welfare
human_wWL_3282 <- calculate_aggregate_welfare(wb_6023, threshold)

# Optional: Save the results to a CSV file
write.xlsx(human_wWL_3282, "first_pass/human_wWL_3282.xlsx", rowNames = FALSE)

#visualisations of total and average global welfare per capita over time
# Create a plot to visualize welfare over time
ggplot(human_wWL_3282, aes(x = Year, y = Total_welfare)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Aggregate Global Welfare Over Time (32-82 Method)",
       subtitle = paste("Using GDP threshold of", round(threshold, 2)),
       x = "Year",
       y = "Total Welfare (Population × Welfare Points)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
ggsave("human_welfare_level_visuals/total_human_welfare_over_time_3282.pdf")

# Also create a plot for average welfare per capita
ggplot(human_wWL_3282, aes(x = Year, y = Avg_welfare_per_capita)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Average Global Welfare Per Capita Over Time (32-82 Method)",
       subtitle = paste("Using GDP threshold of", round(threshold, 2)),
       x = "Year",
       y = "Average Welfare Per Capita") +
  theme_minimal()
ggsave("human_welfare_level_visuals/average_human_welfare_over_time_3282.pdf")










##################### ISOELASTIC UTILITY METHOD  #####################
######## calibrate isoelastic utility function. #####################
#results after using wolfram alpha on india and canada suggest #UTH recalibrating
ubar <- -22.1713
gamma <- 0.674252
#for a sanity check, this gives us u(753.4555) = 4 welfare score for Burundi, which 
#is actually a lot lower than what I might have thought, around 18. But it's ok.

######################## WELFARE WEIGHTED RESULTS ##############################
#calculate weflare scores for all
human_WL <- wb_6023 %>% 
  mutate(welfare_level = ubar + (GDP_filled^(1-gamma))/(1-gamma))

#aggregate to global level
human_wWL_isoelastic <- human_WL %>% 
  select(Year, Country, Population, welfare_level) %>% 
  group_by(Year) %>% 
  summarise(
    Avg_welfare_per_capita = sum(welfare_level * Population, na.rm = TRUE) / sum(Population, na.rm = TRUE),
    .groups = "drop"
  )

#save to file
write.xlsx(human_wWL_isoelastic, "first_pass/human_wWL_isoelastic.xlsx")


############### TESTS AND CHECKS ###############
#out of date
#sanity check india and canada welfare levels given calibration
india_2018_wl <- human_WL %>%
  filter(Country == "India", Year == 2018) %>%
  pull(welfare_level)
# 32

canada_2018_wl <- human_WL %>% 
  filter(Country == "Canada", Year == 2018) %>% 
  pull(welfare_level)
# 82