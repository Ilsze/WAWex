#This file contains a helper function for the calculation of weighted 
#proportional change trends.

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, rlang)

################ PROPORTIONAL CHANGE CALCUALTION ###############################
compute_prop_change <- function(dat, 
                                period_col, # e.g year
                                value_col,
                                species_col = NULL, #UTH edit this! It fell out
                                ...) { # optional grouping columns to aggregate across, e.g country, area, environment
  
  # Capture optional grouping columns as a list of quosures
  group_vars <- enquos(...) # these are captured as expressions and may be empty if no grouping columns are provided
  
  #If there's grouping e.g at the country, area, environment level, 
  if (length(group_vars) > 0) {
    
    # 1. Compute proportional change at the specified grouping level. Call this level the "group".
    #    The lag will be computed within groups if provided
    dat_prop_change <- dat %>%
      arrange(!!!group_vars, {{ period_col }}) %>% #clarification: !!!group_syms turns e.g list(country, area) into country, area. And {{period_col}} additionally makes it country, area, time
      group_by(!!!group_vars) %>%
      mutate(prop_change = 1 + ({{ value_col }} - lag({{ value_col }})) / lag({{ value_col }})) %>%
      ungroup()
    
    # 2. Compute time-averaged value per group, if grouping columns were provided.
    #this will be used for weighting later.
    avg_value <- dat %>%
      group_by(!!!group_vars) %>%
      summarise(avg_VALUE = mean({{ value_col }}, na.rm = TRUE), .groups = "drop")
    
    # 3. Merge the average value into the proportional change data.
    dat_prop_change <- dat_prop_change %>%
      left_join(avg_value, by = sapply(group_vars, rlang::as_name)) # converting quosures to strings for the join
    
    # 4. Aggregate to compute the weighted geometric mean of the proportional change by period.
    #    Note: is.finite(log(...)) is used to filter out non-finite values.
    weighted_prop_change <- dat_prop_change %>%
      group_by({{ period_col }}) %>%
      summarise(
        avg_prop_change = exp(
          sum(
            log(prop_change[is.finite(log(prop_change))]) * 
              avg_VALUE[is.finite(log(prop_change))],
            na.rm = TRUE
          ) / sum(avg_VALUE[is.finite(log(prop_change))], na.rm = TRUE)
        ),
        .groups = "drop"
      )
    #Interpretation of code: weighted_prop_change is a dataset with the period 
    #column (e.g Year), and also the weighted_prop_change column, where each entry
    #is the geometric mean of the proportional changes across that period, 
    #weighted by the group (e.g at the country-area-environment level) that has
    #more individuals.
    
    return(weighted_prop_change)
    
  } else {
    # No grouping columns provided: calculate change over the entire dataset
    simple_prop_change <- dat %>%
      arrange({{ period_col }}) %>%
      mutate(prop_change = 1 + ({{ value_col }} - lag({{ value_col }})) / lag({{ value_col }})) %>%
      select({{ period_col }}, prop_change)
    
    return(simple_prop_change)
  }
}

# Example usage:
output_dat <- compute_prop_change(fao_aqua,
                                  PERIOD,
                                  VALUE,
                                  SPECIES.ALPHA_3_CODE,
                                  COUNTRY.UN_CODE,
                                  AREA.CODE,
                                  ENVIRONMENT.ALPHA_2_CODE)





############ TESTING ############
#Check that output_dat and fao_aqua_agg_prop_change are the same
output_dat == fao_aqua_agg_prop_change #yes

