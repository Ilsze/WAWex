#This file conducts simple analysis produces correlations, elasticities, 
#factor changes, and other summary variables from using calc_tseries from
#prep_calc_tseries.R
## differs from analysis.R bc trying to write it to streamline with any pop, potential, and utility metrics

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm)  

# Read in the updated time series
ts <- read_excel("first_pass/calc_tseries.xlsx")


########### NET SERIES (total utility, potential-weighted pop) #################
# --- 1) Global window and net series ---
# Determine the common years across all categories for global summaries
year_bounds <- ts %>%
  group_by(Category) %>%
  summarize(
    min_y = min(Year, na.rm = TRUE),
    max_y = max(Year, na.rm = TRUE)
  )
common_window <- year_bounds %>%
  summarize(
    start = max(min_y),
    end   = min(max_y)
  )

# Trim to global window
ts_global <- ts %>%
  filter(Year >= common_window$start,
         Year <= common_window$end)

# Compute and save net series
net_series <- ts_global %>%
  group_by(Year) %>%
  summarize(
    net_WR_utility = sum(WR_utility,   na.rm = FALSE),
    net_NC_utility = sum(NC_utility, na.rm = FALSE),
    net_NC_pop     = sum(NC_pop,        na.rm = FALSE),
    net_WR_pop     = sum(WR_pop,        na.rm = FALSE)
  )

write.xlsx(net_series, "first_pass/net_series.xlsx")


########### NET SERIES NO HUMAN (total utility, potential-weighted pop) ########
# --- 1) Global window and net series ---
# Determine the common years across all categories for global summaries
ts_nh <- ts %>% 
  filter(!Category == "Humans")

year_bounds_nh <- ts_nh %>%
  group_by(Category) %>%
  summarize(
    min_y = min(Year, na.rm = TRUE),
    max_y = max(Year, na.rm = TRUE)
  )
common_window_nh <- year_bounds_nh %>%
  summarize(
    start = max(min_y),
    end   = min(max_y)
  )

# Trim to global window
ts_global_nh <- ts_nh %>%
  filter(Year >= common_window_nh$start,
         Year <= common_window_nh$end)

# Compute and save net series
net_series_nh <- ts_global_nh %>%
  group_by(Year) %>%
  summarize(
    net_WR_utility = sum(WR_utility,   na.rm = FALSE),
    net_NC_utility = sum(NC_utility, na.rm = FALSE),
    net_NC_pop     = sum(NC_pop,        na.rm = FALSE),
    net_WR_pop     = sum(WR_pop,        na.rm = FALSE)
  )

write.xlsx(net_series_nh, "first_pass/net_series_nh.xlsx")

################ CORRELATIONS AND ELASTICITIES ###################

# --- 2) Human baseline series ---
human_series <- ts %>%
  filter(Category == "Humans") %>%
  select(Year,
         human_pop = aliveatanytime,
         human_u   = NC_utility) #note that human NC_utility == WR_utility if all is going well

# --- 3) Category-specific correlations & elasticities ---
# Define target variables
regressand  <- c("aliveatanytime", "NC_pop", "WR_pop", "NC_utility", "WR_utility") #dependent
regressor <- c("human_pop", "human_u") #independent
## we're going to end up getting correlations + elasticities of all five of 
## these variables on the human pop and utility variables for each animal 
## category

# Initialize results
target_cats <- setdiff(unique(ts$Category), "Humans")
results <- tibble(
  Category = character(),
  Regressor = character(),
  Regressand = character(),
  Correlation = double(),
  Elasticity = double()
)

#for each non-human cateogry, pair each of the regressands on each of the 
#regressors
for(cat in target_cats) {
  for(ror in regressor) {
    for(rand in regressand) {
      
      cat_data <- ts %>% filter(Category == cat)
      
      # Determine non-NA ranges
      #note that .data[[ror]]: accesses the column named by the variable 
      #ror (not the literal string "ror").
      h_nonNA <- human_series %>% filter(!is.na(.data[[ror]]))
      c_nonNA <- cat_data     %>% filter(!is.na(.data[[rand]]))
      
      start_year <- max(min(h_nonNA$Year), min(c_nonNA$Year))
      end_year   <- min(max(h_nonNA$Year), max(c_nonNA$Year))
      
      # Filter vectors within range
      h_vec <- h_nonNA %>%
        filter(Year >= start_year, Year <= end_year) %>%
        pull(.data[[ror]])
      c_vec <- c_nonNA %>%
        filter(Year >= start_year, Year <= end_year) %>%
        pull(.data[[rand]])
      
      # Compute statistics
      cor_val  <- cor(h_vec, c_vec, use = "complete.obs")
      elas_val <- coef(lm(c_vec ~ h_vec))[2] #which of these should involve log regressions?
      
      # Append to results
      results <- results %>% add_row(
        Category    = cat,
        Regressor = ror,
        Regressand = rand,
        Correlation = cor_val,
        Elasticity  = elas_val
      )
    }
  }
}



# Save results
write.xlsx(results, "first_pass/cor_and_elas.xlsx")