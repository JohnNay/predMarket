#
#
#
source('prepare_data.R')
source('climate_model.R')

library(ggplot2)
theme_set(theme_bw(base_size=20))

climate_data <- prepare_climate_data()

future_co2 <- data.frame(co2 = c(tail(climate_data$co2,-100), 
                                 seq(tail(climate_data$co2,1), 800, length.out = 201 - nrow(climate_data))[-1]))
future_co2 <- future_co2 %>% mutate(log.co2 = log(co2))

mdl.co2 <- new('climate_model', climate = climate_data, covariates = list('log.co2'))
mdl.co2 <- init_model(mdl.co2, n_history = 100, n_future = 100, covars = list('log.co2'), future_covars = future_co2)

future <- update_model(mdl.co2, n_today = 100, n_horizon = 10)

ggplot(future@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future@climate, color = 'blue')

interval_prob(future, 10, c(0.5,0.7))

#' Total kludge. Because I don't have a good model yet, I just reverse the last century of TSI to 
#' simulate the next century.
future_tsi <- data.frame(tsi = c(tail(climate_data$tsi,-100), 
                                 rev(tail(climate_data$tsi,200 - nrow(climate_data)))))
mdl.tsi <- new('climate_model', climate = climate_data, covariates = list("tsi"))
mdl.tsi <- init_model(mdl.tsi, n_history = 100, n_future = 100, covars = list('tsi'), future_covars = future_tsi)

future_2 <- update_model(mdl.tsi, n_today = 100, n_horizon = 10)
ggplot(future_2@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future_2@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future_2@climate, color = 'blue')

interval_prob(future_2, 10, c(0.5,0.7))

mdl.co2 <- init_model(mdl.co2, 115, 0, list('co2'), NULL)
future <- update_model(mdl.co2, 50, 20)

ggplot(future@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future@climate, color = 'blue')

interval_prob(future, 10, c(0.3, 0.7))

mdl.tsi <- init_model(mdl.tsi, 115, 0, list('tsi'), NULL, p = 1, q = 0)
future_2 <- update_model(mdl.tsi, 50, 20, auto_arma = FALSE)

ggplot(future_2@future, aes(x = year, y = t.anom)) + 
  # simulated future temperatures
  geom_point() + geom_line() + 
  # vertical bands: predictions
  geom_point(data = future_2@prediction %>% filter(sim <= 500), alpha = 0.02) + 
  # actual temperatures
  geom_point(data = future_2@climate, color = 'blue')

interval_prob(future_2, 10, c(0.3, 0.7))
