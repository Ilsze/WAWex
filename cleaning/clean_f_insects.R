##This file cleans Rethink Priorities global farmed insect for food and feed 
## point abundance estimates [Abraham Rowe], as well as RP forecasting and 
## backcasting trends for mealworms and black soldier flies in the west. 
##It also adds information, naively extending backcasting from 2015 back to 1961. 
##It also copies western time series trends to global point estimate data. 
library(pacman)
p_load(tidyverse, dplyr, readr, ggplot2, gridExtra, png, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, minpack.lm)  

#read in cast data
## note this dataset seems to have percentiles as a result of monte carlo estimates. 
## more on methods and results here: https://rethinkpriorities.org/research-area/investments-into-insect-farming/ 
insect_cast <- read_xlsx("dat/rp/insect_forecasts_byspecies & year.xlsx", sheet = "Data")
#keep aliveatanytime, species, years, median
insect_cast_clean <- insect_cast %>% 
  select(year, species, variable, median) %>% 
  filter(variable == "aliveatanytime")

#backcast further. First observe the trend. 
#get each group's median first, as vectors
cast_bsf_med <- filter(insect_cast_clean, species == "bsf") %>% 
  select(year, median)
cast_mw_med <- filter(insect_cast_clean, species == "mw") %>% 
  select(year, median)

## GENERALIZED LOGISTIC FUNCTION - works great and interpretation makes sense
##Begin with bsf
#rescale so medians are in billion units for model fitting
cast_bsf_med_rescaled <- cast_bsf_med %>% 
  mutate(median = median/1e9)
##fit nls model with logistic function
log_bsf <- nlsLM(median ~ L/((1 + exp(-k * (year - year0)))^p), #L is max, k is growth rate, year0 is median in logistic function
               data = cast_bsf_med_rescaled, 
               start = list(L = max(cast_bsf_med_rescaled$median), 
                            k = 0.05, 
                            year0 = mean(cast_bsf_med_rescaled$year),
                            p = 1.5), #parameter that generalised logistic. p > 1 makes growth steeper
               control = nls.control(maxiter = 1000)) #how many rounds you give the algorithim to converge on better values
#generate new predictions for the existing years and also further into the past
new_x <- seq(1961, 2035)
cast_bsf_hat <- data.frame(year = new_x)  
bsf_hat <- predict(log_bsf, newdata = cast_bsf_hat)
#plot results
plot(cast_bsf_med_rescaled$year, cast_bsf_med_rescaled$median, pch = 19, xlab = "Year", ylab = "Median BSFs alive at any time in billions", main = "Logistic Fit with Backcasting")
lines(new_x, bsf_hat, col = "blue", lwd = 2)
#append bsf_hat to cast_bsf_hat
bsf_hat <- data.frame(year = new_x, aliveatanytime_med = bsf_hat*1e9)

#use Sagar's actual values where available (not predicted by me), that is 2015 onwards. ugly ass code
bsf_hat$aliveatanytime_med[bsf_hat$year >= 2015] <- 
  cast_bsf_med$median[match(bsf_hat$year[bsf_hat$year >= 2015], cast_bsf_med$year)]

##Do the same for mw
cast_mw_med_rescaled <- cast_mw_med %>% 
  mutate(median = median/1e9)
##fit nls model with logistic function
log_mw <- nlsLM(median ~ L/((1 + exp(-k * (year - year0)))^p), #L is max, k is growth rate, year0 is median in logistic function
                 data = cast_mw_med_rescaled, 
                 start = list(L = max(cast_mw_med_rescaled$median), 
                              k = 0.05, 
                              year0 = mean(cast_mw_med_rescaled$year),
                              p = 1.5), #parameter that generalised logistic. p > 1 makes growth steeper
                 control = nls.control(maxiter = 1000)) #how many rounds you give the algorithim to converge on better values
#generate new predictions for the existing years and also further into the past
new_x <- seq(1961, 2035)
cast_mw_hat <- data.frame(year = new_x)  
mw_hat <- predict(log_mw, newdata = cast_mw_hat)
#plot results
plot(cast_mw_med_rescaled$year, cast_mw_med_rescaled$median, pch = 19, xlab = "Year", ylab = "Median mealworms alive at any time in billions", main = "Logistic Fit with Backcasting")
lines(new_x, mw_hat, col = "blue", lwd = 2)
#append mw_hat to cast_mw_hat
mw_hat <- data.frame(year = new_x, aliveatanytime_med = mw_hat*1e9)

#use Sagar's actual values where available (not predicted by me), that is 2015 onwards. ugly ass code
mw_hat$aliveatanytime_med[mw_hat$year >= 2015] <- 
  cast_mw_med$median[match(mw_hat$year[mw_hat$year >= 2015], cast_mw_med$year)]

#Small data summary
# bsf_hat and mw_hat says ~0 insects for farm and feed aliveatanytime in 2010
#But what matters is trend not number, since these will be mapped onto Abraham.

#We save log_bsf and log_mw models to global environment
save(log_bsf, file = "dat/rp/sagar_bsf_model.RData")
save(log_mw, file = "dat/rp/sagar_mw_model.RData")
