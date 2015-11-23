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
future_data <- data$future

today <- which(climate_data$year == 1999)

mdl.co2 <- new('climate_model', climate = climate_data)
mdl.co2 <- init_model(mdl.co2, n_history = today, n_future = 100, true_covar = 'log.co2', future_covars = future_data)

future.co2.co2 <- update_model(mdl.co2, n_today = today-20, n_horizon = 30, trader_covar = 'log.co2')

p1a <- ggplot(future.co2.co2@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future.co2.co2@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future.co2.co2@climate, color = 'blue') +
  labs(x = "Year", y = "T anomaly", title = "True = log(CO2), Trader = log(CO2)")

plot(p1a)


future.tsi.co2 <- update_model(mdl.co2, n_today = today, n_horizon = 10, trader_covar = 'slow.tsi')

p1b <- ggplot(future.tsi.co2@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future.tsi.co2@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future.tsi.co2@climate, color = 'blue') +
  labs(x = "Year", y = "T anomaly", title = "True = log(CO2), Trader = Slow TSI")

plot(p1b)

mdl.tsi <- new('climate_model', climate = climate_data)
mdl.tsi <- init_model(mdl.tsi, n_history = today, n_future = 100, true_covar = 'slow.tsi', future_covars = future_data)

future.tsi.tsi <- update_model(mdl.tsi, n_today = today, n_horizon = 10, trader_covar = 'slow.tsi')
p2a <- ggplot(future.tsi.tsi@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future.tsi.tsi@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future.tsi.tsi@climate, color = 'blue') +
  labs(x = "Year", y = "T anomaly", title = "True = Slow TSI, Trader = Slow TSI")

plot(p2a)


future.co2.tsi <- update_model(mdl.tsi, n_today = today, n_horizon = 10, trader_covar = 'log.co2')
p2b <- ggplot(future.co2.tsi@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future.co2.tsi@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future.co2.tsi@climate, color = 'blue') +
  labs(x = "Year", y = "T anomaly", title = "True = Slow TSI, Trader = log(CO2)")

plot(p2b)

mdl.co2 <- init_model(mdl.co2, nrow(mdl.co2@climate), 0, true_covar = 'log.co2', NULL)
future2.co2.co2 <- update_model(mdl.co2, which(mdl.co2@climate$year == 1980), 20, trader_covar = 'log.co2')

p3a <- ggplot(future2.co2.co2@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future2.co2.co2@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future2.co2.co2@climate, color = 'blue') +
  labs(x = "Year", y = "T anomaly", title = "True = log(CO2), Trader = log(CO2)")

plot(p3a)

future2.tsi.co2 <- update_model(mdl.co2, which(mdl.co2@climate$year == 1980), 20, trader_covar = 'slow.tsi')

p3b <- ggplot(future2.tsi.co2@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future2.tsi.co2@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future2.tsi.co2@climate, color = 'blue') +
  labs(x = "Year", y = "T anomaly", title = "True = log(CO2), Trader = Slow TSI")

plot(p3b)

mdl.tsi <- init_model(mdl.tsi, nrow(mdl.tsi@climate), 0, true_covar = 'slow.tsi', NULL, p = 1, q = 0)
future2.tsi.tsi <- update_model(mdl.tsi, which(mdl.tsi@climate$year == 1980), 20, trader_covar = 'slow.tsi', auto_arma = FALSE)

p4a <- ggplot(future2.tsi.tsi@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future2.tsi.tsi@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future2.tsi.tsi@climate, color = 'blue') +
  labs(x = "Year", y = "T anomaly", title = "True = Slow TSI, Trader = Slow TSI")

plot(p4a)

future2.co2.tsi <- update_model(mdl.tsi, which(mdl.tsi@climate$year == 1980), 20, trader_covar = 'log.co2', auto_arma = FALSE)

p4b <- ggplot(future2.co2.tsi@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future2.co2.tsi@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future2.co2.tsi@climate, color = 'blue') +
  labs(x = "Year", y = "T anomaly", title = "True = Slow TSI, Trader = log(CO2)")

plot(p4b)
