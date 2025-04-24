#This file regresses forebrain neuron count on wholebrain neuron count to 
#predict fb from wholebrain

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm) 

NC_dat <- read_excel("first_pass/wb_vs_f_NC_count.xlsx")
#note that human is removed as outlier

lm_model <- lm(f_NC_count_million ~ wb_NC_count_million, data = NC_dat)
log_model <- lm(log(f_NC_count_million) ~ log(wb_NC_count_million), data = NC_dat)
#summary(model)

#want to predict fish forebrain from wholebrain
lm_predictions <- predict(lm_model, newdata = data.frame(wb_NC_count_million = c(74, 130)))
log_predictions <- exp(predict(log_model, newdata = data.frame(wb_NC_count_million = c(74, 130))))


#do i need to remove human outlier? 


#plot for viewing purposes
# Plot the data (linear)
plot(NC_dat$wb_NC_count_million, NC_dat$f_NC_count_million,
     xlab = "Wholebrain NC Count (million)",
     ylab = "Forebrain NC Count (million)",
     main = "Linear Fit: Forebrain vs Wholebrain NC Count",
     pch = 16, col = "blue")

# Add the regression line using the fitted model (uses slope and intercept from 'model')
abline(lm_model, col = "red", lwd = 2)

# Add a grid for readability
grid()

# Plot the log-transformed data
plot(log(NC_dat$wb_NC_count_million), log(NC_dat$f_NC_count_million),
     xlab = "log(Wholebrain NC Count)",
     ylab = "log(Forebrain NC Count)",
     main = "Log-Log Fit: Forebrain vs Wholebrain NC Count",
     pch = 16, col = "blue")

# Add the regression line (log-log)
abline(log_model, col = "red", lwd = 2)

# Add a grid for readability
grid()

