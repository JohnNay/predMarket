#
#
#
source('prepare_data.R')
source('climate_model.R')

library(ggplot2)
library(dplyr)
library(tidyr)

theme_set(theme_bw(base_size=20))

data <- prepare_climate_data('rcp 4.5')

climate_data <- data$data
future_covars <- data$future

today <- which(climate_data$year == 1999)

mdl.co2 <- new('climate_model', climate = climate_data, covariates = list('log.co2'))
mdl.co2 <- init_model(mdl.co2, n_history = today, n_future = 100, covars = list('log.co2'), future_covars = future_covars)

future <- update_model(mdl.co2, n_today = today, n_horizon = 10)

p1 <- ggplot(future@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future@climate, color = 'blue')

plot(p1)

interval_prob(future, 10, c(0.5,0.7))

#' Total kludge. Because I don't have a good model yet, I just reverse the last century of TSI to 
#' simulate the next century.
mdl.tsi <- new('climate_model', climate = climate_data, covariates = list("slow.tsi"))
mdl.tsi <- init_model(mdl.tsi, n_history = today, n_future = 100, covars = list('slow.tsi'), future_covars = future_covars)

future_2 <- update_model(mdl.tsi, n_today = today, n_horizon = 10)
p2 <- ggplot(future_2@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future_2@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future_2@climate, color = 'blue')

plot(p2)

interval_prob(future_2, 10, c(0.5,0.7))

mdl.co2 <- init_model(mdl.co2, nrow(mdl.co2@climate), 0, list('co2'), NULL)
future <- update_model(mdl.co2, which(mdl.co2@climate$year == 1980), 20)

p3 <- ggplot(future@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future@climate, color = 'blue')

plot(p3)

interval_prob(future, 10, c(0.3, 0.7))

mdl.tsi <- init_model(mdl.tsi, nrow(mdl.tsi@climate), 0, list('slow.tsi'), NULL, p = 1, q = 0)
future_2 <- update_model(mdl.tsi, which(mdl.tsi@climate$year == 1980), 20, auto_arma = FALSE)

p4 <- ggplot(future_2@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future_2@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future_2@climate, color = 'blue')

plot(p4)

interval_prob(future_2, 10, c(0.3, 0.7))
