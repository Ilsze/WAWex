#This file contains a helper function for the calculation of weighted 
#proportional change trends.

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, rlang)

################ PROPORTIONAL CHANGE CALCUALTION ###############################
compute_prop_change <- function(dat, 
                                species_col, 
                                country_col, 
                                area_col, 
                                environment_col, 
                                period_col, 
                                value_col) {
  # Convert string inputs to symbols for tidy evaluation
  species_sym <- sym(species_col)
  country_sym <- sym(country_col)
  area_sym <- sym(area_col)
  environment_sym <- sym(environment_col)
  period_sym <- sym(period_col)
  value_sym <- sym(value_col)
  
  # 1. Compute proportional change at the species-country-area-environment level.
  #    The lag is computed within groups defined by country, area, and environment.
  dat_prop_change <- dat %>%
    arrange(!!species_sym, !!country_sym, !!area_sym, !!environment_sym, !!period_sym) %>%
    group_by(!!species_sym, !!country_sym, !!area_sym, !!environment_sym) %>%
    mutate(ton_prop_change = 1 + ((!!value_sym) - lag(!!value_sym)) / lag(!!value_sym)) %>%
    ungroup()
  
  # 2. Compute time-averaged value per species-country-area-environment.
  avg_value <- dat %>%
    group_by(!!species_sym, !!country_sym, !!area_sym, !!environment_sym) %>%
    summarise(avg_VALUE = mean(!!value_sym, na.rm = TRUE), .groups = "drop")
  
  # 3. Merge the average value into the proportional change data.
  dat_prop_change <- dat_prop_change %>%
    left_join(avg_value, by = c(species_col, country_col, area_col, environment_col))
  
  # 4. Aggregate to compute the weighted geometric mean of the proportional change by period.
  #    Note: is.finite(log(...)) is used to filter out non-finite values.
  result <- dat_prop_change %>%
    group_by(!!period_sym) %>%
    summarise(
      avg_prop_change = exp(
        sum(
          log(ton_prop_change[is.finite(log(ton_prop_change))]) * 
            avg_VALUE[is.finite(log(ton_prop_change))],
          na.rm = TRUE
        ) / sum(avg_VALUE[is.finite(log(ton_prop_change))], na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  return(result)
}

# Example usage:
output_dat <- compute_prop_change(dat = fao_aqua,
                                  species_col = "SPECIES.ALPHA_3_CODE",
                                  country_col = "COUNTRY.UN_CODE",
                                  area_col = "AREA.CODE",
                                  environment_col = "ENVIRONMENT.ALPHA_2_CODE",
                                  period_col = "PERIOD",
                                  value_col = "VALUE")


############ TESTING ############
#Check that output_dat and fao_aqua_agg_prop_change are the same
output_dat == fao_aqua_agg_prop_change #yes

