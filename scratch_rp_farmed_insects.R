#################### TRYING DIFFERENT MODELS TO BACKCAST INSECT FARMING #######

## TRY LOGISTIC FUNCTION  - this works but isn't quite steep enough
#rescale so medians are in billion units for model fitting
cast_bsf_med_rescaled <- cast_bsf_med %>% 
  mutate(median = median/1e9)
##fit nls model with logistic function
log_bsf <- nlsLM(median ~ L/(1 + exp(-k * (year - year0))), #L is max, k is growth rate, year0 is median in logistic function
                 data = cast_bsf_med_rescaled, 
                 start = list(L = max(cast_bsf_med_rescaled$median), 
                              k = 0.05, 
                              year0 = mean(cast_bsf_med_rescaled$year)), 
                 control = nls.control(maxiter = 1000)) #how many rounds you give the algorithim to converge on better values
#generate new predictions for the existing years and also further into the past
new_x <- seq(1961, 2035)
cast_bsf_hat <- data.frame(year = new_x)  
bsf_hat <- predict(log_bsf, newdata = cast_bsf_hat)
#plot results
plot(cast_bsf_med_rescaled$year, cast_bsf_med_rescaled$median, pch = 19, xlab = "Year", ylab = "Median alive at any time in billions", main = "Logistic Fit with Backcasting")
lines(new_x, bsf_hat, col = "blue", lwd = 2)





## Try GAM
#fit GAM with log link function for non-negativity. Visual inspection shows the 
# graph look logistic
gam_bsf <- gam(median ~ s(year, k = 5), data = cast_bsf_med, 
               family = Gamma(link = "log"))
gam_mw <- gam(median ~ s(year, k = 5), data = cast_mw_med, 
              family = Gamma(link = "log"))
#plot visually
plot(gam_bsf, shade = TRUE, main = "Smooth Term for BSF median")
plot(gam_mw, shade = TRUE, main = "Smooth Term for MW median")

