#This file regresses forebrain neuron count on wholebrain neuron count to 
#predict fb from wholebrain

library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm) 

NC_dat <- read_excel("first_pass/wb_vs_f_NC_count.xlsx")
#note that human is removed as outlier

lm_model <- lm(f_NC_count ~ wb_NC_count, data = NC_dat)
log_model <- lm(log(f_NC_count) ~ log(wb_NC_count), data = NC_dat)
#summary(log_model)

#want to predict fish forebrain from wholebrain
lm_predictions <- predict(lm_model, newdata = data.frame(wb_NC_count = c(74, 130)))
log_predictions <- exp(predict(log_model, newdata = data.frame(wb_NC_count = c(74, 130))))


#do i need to remove human outlier? 


#plot for viewing purposes
# Plot the data (linear)
plot(NC_dat$wb_NC_count, NC_dat$f_NC_count,
     xlab = "Wholebrain NC Count",
     ylab = "Forebrain NC Count",
     main = "Linear Fit: Forebrain vs Wholebrain NC Count",
     pch = 16, col = "blue")

# Add the regression line using the fitted model (uses slope and intercept from 'model')
abline(lm_model, col = "red", lwd = 2)

# Add a grid for readability
grid()

# Plot the log-transformed data
plot(log(NC_dat$wb_NC_count), log(NC_dat$f_NC_count),
     xlab = "log(Wholebrain NC Count)",
     ylab = "log(Forebrain NC Count)",
     main = "Log-Log Fit: Forebrain vs Wholebrain NC Count",
     pch = 16, col = "blue")

# Add the regression line (log-log)
abline(log_model, col = "red", lwd = 2)

# Add a grid for readability
grid()


################# MODEL THAT INCLUDES BODY SIZE #################
##VERDICT: BODY SIZE HAS P-VALUE > 0.07 . DON"T USE
animaltraits <- read_excel("first_pass/animaltraits.xlsx")

#merge animaltraits and NC_dat
# Merge datasets keeping only required columns
NC_dat_bs <- merge(
  x = NC_dat,                             # First dataset - keeping all columns
  y = animaltraits[, c("species",         # Second dataset - selecting only specific columns
                       "body_mass",
                       "body mass - units")],
  by = "species",                         # Merge by the common column "species"
  all.x = TRUE                            # Keep all rows from NC_dat (left join)
)

log_model_bs <- lm(log(f_NC_count) ~ log(wb_NC_count) + log(body_mass), data = NC_dat_bs)
#summary(log_model)

#PREDICT CARP AND SALMON
log_predictions_bs <- exp(predict(log_model_bs, 
                                  newdata = data.frame(wb_NC_count = c(74000000, 130000000), 
                                                       body_mass = c(4.75, 4.5)))) #unit: kg 

# Plot the log-transformed data
#regressive model for the two to visualise
f_v_body_mass_model <- lm(log(f_NC_count) ~ log(body_mass), data = NC_dat_bs)
#visualise
plot(log(NC_dat_bs$body_mass), log(NC_dat_bs$f_NC_count),
     xlab = "log(Body Mass in kg)",
     ylab = "log(Forebrain NC Count)",
     main = "Log-Log Fit: Body Mass vs Forebrain Neuron Count",
     pch = 16, col = "blue")

# Add the regression line (log-log)
abline(f_v_body_mass_model, col = "red", lwd = 2)

# Add a grid for readability
grid()





################### FOR BRISTLEMOUTH PREDICTION ##################
# Create a simpler model directly relating forebrain NC count to body mass
direct_model <- lm(log(f_NC_count) ~ log(body_mass), data = NC_dat_bs)

# Summary of the model to examine its performance
summary(direct_model)

# Predict using only body mass
new_data <- data.frame(body_mass = 0.004)  # 4g
predicted_log_f_NC <- predict(direct_model, newdata = new_data)
predicted_f_NC <- exp(predicted_log_f_NC)

# Print result
print(paste("Predicted forebrain neural cell count for a 4g animal:", 
            format(predicted_f_NC, digits = 3, scientific = TRUE)))

# Optional: Calculate prediction intervals for better understanding of uncertainty
pred_interval <- predict(direct_model, newdata = new_data, interval = "prediction")
lower_f_NC <- exp(pred_interval[, "lwr"])
upper_f_NC <- exp(pred_interval[, "upr"])

print(paste("95% prediction interval:", 
            format(lower_f_NC, digits = 3, scientific = TRUE), "to", 
            format(upper_f_NC, digits = 3, scientific = TRUE)))

