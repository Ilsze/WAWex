#this file loads swb data from the US, taken from IVS and appends it to NA population data.
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, foreign, broom, knitr, data.table) 

#load US SWB data
swb_us <- read_rds("./data/int_val_surv/swb_us.rds")

# Function to create plot with connected points and best fit line
create_plot <- function(data, y_var, title, y_label) {
  ggplot(data, aes(x = S020, y = !!sym(y_var))) +
    geom_line() +  # Connect points with a line
    geom_point() +  # Add points
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add best fit line
    theme_minimal() +
    labs(title = title,
         x = "Year",
         y = y_label) +
    scale_x_continuous(breaks = unique(data$S020))
}

# Create plots
plot_satisfaction <- create_plot(swb_us, "mean_life_satisfaction", 
                                 "Mean Life Satisfaction in the US Over Time", 
                                 "Mean Life Satisfaction")

plot_happiness <- create_plot(swb_us, "mean_happiness", 
                              "Mean Happiness in the US Over Time", 
                              "Mean Happiness")

# Display the plots
print(plot_satisfaction)
print(plot_happiness)
#display them side by side
grid.arrange(plot_satisfaction, plot_happiness, ncol = 2)

# If you want to save the plots, you can use ggsave:
# ggsave("life_satisfaction_plot.png", plot_satisfaction, width = 10, height = 6)
# ggsave("happiness_plot.png", plot_happiness, width = 10, height = 6)

#swb_us as it is, summarised, in latex format: 
kable(swb_us, format = "latex")

############################# EXTRAPOLATION
# Create linear models
lm_satisfaction <- lm(mean_life_satisfaction ~ S020, data = swb_us)
lm_happiness <- lm(mean_happiness ~ S020, data = swb_us)

# Create a data frame with all years from 1970 to 2024
all_years <- data.frame(S020 = 1970:2023)

# Predict values for all years
predictions <- all_years %>%
  mutate(
    mean_life_satisfaction = predict(lm_satisfaction, newdata = .),
    mean_happiness = predict(lm_happiness, newdata = .)
  )

# Combine predictions with actual data, preferring actual data where available
final_table <- predictions %>%
  left_join(swb_us, by = "S020") %>%
  mutate(
    mean_life_satisfaction = coalesce(mean_life_satisfaction.y, mean_life_satisfaction.x),
    mean_happiness = coalesce(mean_happiness.y, mean_happiness.x)
  ) %>%
  select(S020, mean_life_satisfaction, mean_happiness) %>% 
  #rename S020 column to Time
  rename(Time = S020)
#final_table is a dataset with three columns: Time, mean_life_satisfaction, and mean_happiness


########### POP DATA 
#load pop data 
#un_dat <- read_excel("./data/un/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx") #parsing issues. world_bank data also had parsing issues
#un_dat <- read_csv("./data/un/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv.gz")

#filter for north american region/subregion/country/area
## Bermuda is LocID 60. Greenland is LocID 304. Saint Pierre and Miquelon is 666. Each are excluded due to bird data probs not there
## Canada is locID 124
## USA is LocID 840
un_f <- un_dat %>% 
  select("LocID", "Location", "PopTotal", "Time") %>% 
  filter(LocID == 840 | LocID == 124) %>% 
  #at this point, still differentiated by age group
  group_by(Time) %>% 
  summarise(PopTot = sum(PopTotal)) %>% 
  ungroup()

#keep 1970 to 2023
un_7023 <- filter(un_f, Time >= 1970)
#save to avoid having to load un_dat again
write_rds(un_7023, "./data/un/un_7023.rds")
#note that population counts are in 1000s. The colnames are Time and PopTot

#merge un_7023 and final_table
na_pop_us_swb <- merge(final_table, un_7023, by = "Time", all = TRUE) %>% 
  #multiply PopTot by a thousand to it's now in individuals not thousands of individuals
  mutate(PopTot = PopTot*1000, 
         LSTot = PopTot*mean_life_satisfaction) 

#Plot human utils on the x axis and bird numbers on the y axis


  
  


