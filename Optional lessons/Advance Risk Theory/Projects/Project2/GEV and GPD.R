# Load necessary libraries
library(fitdistrplus)
library(ismev)       # For GEV functions
library(extRemes)    # Additional extreme value analysis tools
library(EnvStats)
  data("danishuni")

#GEV ON ALL DATA WITH MLE method
loss_data <- danishuni$Loss
gev_fit_all_data_mle <- fevd(loss_data, method = "MLE", type = "GEV")
summary(gev_fit_all_data_mle)
plot(gev_fit_all_data_mle)
return_level <- return.level(gev_fit_all_data_mle, return.period = 100)
print(return_level)
simulated_data1 <- rgev(1000, loc = gev_fit_all_data_mle$results$par["location"], 
                       scale = gev_fit_all_data_mle$results$par["scale"])
head(simulated_data1)

#GEV ON ALL DATA WITH PWM method
loss_data <- danishuni$Loss
gev_fit_all_data_pwm <-egevd(loss_data, method = "pwme")
# برازش توزیع Generalized Pareto به exceedances
gev_fit_all_data_pwm
simulated_data2 <- rgev(1000, loc = gev_fit_all_data_pwm$parameters["location"], 
                        scale = gev_fit_all_data_pwm$parameters["scale"], 
                        shape = gev_fit_all_data_pwm$parameters["shape"])
head(simulated_data2)


#GEV ON BLOCK MAX DATA with MLE method
library(dplyr)
danish_block_max <- danishuni %>%
  group_by(year = lubridate::year(Date)) %>%  # فرض کنید ستون Date دارد
  summarise(MaxLoss = max(Loss))
gev_fit_blockMax_data_mle <- fevd(danish_block_max$MaxLoss, method = "MLE"  , type = "GEV")
summary(gev_fit_blockMax_data_mle)
plot(gev_fit_blockMax_data_mle)
return_level <- return.level(gev_fit_blockMax_data_mle, return.period = 100)
print(return_level)
simulated_data3 <- rgev(1000, loc = gev_fit_blockMax_data_mle$results$par["location"], 
                       scale = gev_fit_blockMax_data_mle$results$par["scale"], 
                       shape = gev_fit_blockMax_data_mle$results$par["shape"])
head(simulated_data3)

#GEV ON BLOCK MAX DATA WITH PWM method
gev_fit_BlockMax_pwm <-egevd(danish_block_max$MaxLoss, method = "pwme")
# برازش توزیع Generalized Pareto به exceedances
gev_fit_BlockMax_pwm
simulated_data4 <- rgev(1000, loc = gev_fit_BlockMax_pwm$parameters["location"], 
                        scale = gev_fit_BlockMax_pwm$parameters["scale"], 
                        shape = gev_fit_BlockMax_pwm$parameters["shape"])
head(simulated_data4)


#GPD
# now we should set a threashold On All Data
threshold1 = quantile(danishuni$Loss , probs = 0.95)
gpd_fit_all_data <- fevd(loss_data, threshold = threshold1, type = "GP")
summary(gpd_fit_all_data)
plot(gpd_fit_all_data)
return_level <- return.level(gpd_fit_all_data, return.period = 100)
print(return_level)

simulated_data5 <- rgev(1000, 
                        scale = gpd_fit_all_data$results$par["scale"], 
                        shape = gpd_fit_all_data$results$par["shape"])
head(simulated_data5)
