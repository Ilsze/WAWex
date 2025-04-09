##scratch for helping write the script helper_prop_trend.R
##newer
##newer, more flexible, still-needs-to-be tested {{ }} version



##### draft of newer, more flexible, version
compute_prop_change <- function(dat, 
                                period_col, # e.g year
                                value_col,
                                ...) { # optional grouping columns, e.g species, country, area, environment
  
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
    
  } else {
    # No grouping columns provided: calculate change over the entire dataset
    dat_prop_change <- dat_prop_change %>%
      arrange({{ period_col }}) %>%
      mutate(prop_change = 1 + ({{ value_col }} - lag({{ value_col }})) / lag({{ value_col }}),
             avg_VALUE = mean({{ value_col }}, na.rm = TRUE))
  }
  
  # 4. Aggregate to compute the weighted geometric mean of the proportional change by period.
  #    Note: is.finite(log(...)) is used to filter out non-finite values.
  prop_change_vector <- dat_prop_change %>%
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
  #Interpretation of code: prop_change_vector is a vector with one entry per time period, where each entry
  #is the geometric mean of the proportional changes across that period, 
  #weighted by the group (e.g at the country-area-environment level) that has
  #more individuals.
  
  ##
  
  #return both the dataset with the prop_change calculated, as well as the geometric mean of the proportional change
  return(prop_change_vector)
}







##############Old, partially altered sym version
compute_prop_change <- function(dat, 
                                period_col, #e.g year
                                value_col,
                                species_col = NULL, 
                                country_col = NULL, 
                                area_col = NULL, 
                                environment_col = NULL) {
  # Convert string inputs to symbols for tidy evaluation (required)
  period_sym <- sym(period_col)
  value_sym <- sym(value_col)
  species_sym <- sym(species_col)
  country_sym <- sym(country_col)
  area_sym <- sym(area_col)
  environment_sym <- sym(environment_col)
  
  
  #build vector of optional grouping column names
  optional_cols <- c(species_col, country_col, area_col, environment_col) #vector elements either take the argument specified or "NULL"
  # Remove any NULL entries
  group_vars <- optional_cols[!sapply(optional_cols, is.null)] #vector may be shortened to exclude "NULL" entries
  
  # If group_vars exist, convert them to symbols for tidy evaluation
  if (length(group_vars) > 0) {
    group_syms <- syms(group_vars)
    
    # 1. Compute proportional change at up to the species-country-area-environment level. Call the relevant level the "group"
    #    The lag will be computed within groups
    dat_prop_change <- dat %>%
      arrange(!!!group_syms, !!period_sym) %>%  
      group_by(!!!group_syms) %>%
      mutate(ton_prop_change = 1 + ((!!value_sym) - lag(!!value_sym)) / lag(!!value_sym)) %>%
      ungroup()
    
    # 2. Compute time-averaged value per group
    avg_value <- dat %>%
      group_by(!!!group_syms) %>%
      summarise(avg_VALUE = mean(!!value_sym, na.rm = TRUE), .groups = "drop")
    
    # 3. Merge the average value into the proportional change data.
    dat_prop_change <- dat_prop_change %>%
      left_join(avg_value, by = group_vars)
    
  } else {
    #No grouping columns provided: calcualte change over the entire dataset [UTH checking this!]
    dat_prop_change <- dat %>%
      arrange(!!period_sym) %>%
      mutate(ton_prop_change = 1 + ((!!value_sym) - lag(!!value_sym)) / lag(!!value_sym)) %>%
      mutate(avg_VALUE = mean(!!value_sym, na.rm = TRUE))
  }
  
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
