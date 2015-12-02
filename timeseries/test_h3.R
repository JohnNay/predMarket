#
#
#
# rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)

source('prepare_data.R')
source('climate_model.R')

TRACE_CLIMATE_MODEL <- TRUE
PARALLEL_STAN <- FALSE
WHICH_MODEL <- 'ar1'

theme_set(theme_bw(base_size=20))

data <- prepare_climate_data('rcp 4.5')

climate_data <- data$data
future_data <- data$future

today <- which(climate_data$year == 1999)

mdl.co2 <- new('climate_model', climate = climate_data)
mdl.co2 <- init_model(mdl.co2, n_history = today, n_future = 100, true_covar = 'log.co2', future_covars = future_data)

predictions <- list(co2 = list(), tsi = list())

for (n in 0:9) {
  pred.co2 <- update_model(mdl.co2, n_today = today + 10 * n, n_horizon = 10, trader_covar = 'log.co2')
  pred.tsi <- update_model(mdl.co2, n_today = today + 10 * n, n_horizon = 10, trader_covar = 'slow.tsi')
  predictions$co2 <- c(predictions$co2, list(pred.co2@prediction))
  predictions$tsi <- c(predictions$tsi, list(pred.tsi@prediction))
}

df <- data.frame()

for (n in 1:10) {
  x <- predictions$co2[[n]]
  y <- predictions$tsi[[n]]
  
  last_yr = max(x$year)
  t.true <- filter(mdl.co2@future, year == last_yr)$t.anom
  x <- x %>% filter(year == last_yr) %>% mutate(delta.t = t.anom - t.true, trader = 'log.co2') %>% select(year, delta.t, trader)
  y <- y %>% filter(year == last_yr) %>% mutate(delta.t = t.anom - t.true, trader = 'slow.tsi') %>% select(year, delta.t, trader)
  
  df <- rbind_all(list(df, x, y))
}


x <- predictions$co2[[10]] %>% mutate(trader = 'log.co2')
y <- predictions$tsi[[10]] %>% mutate(trader = 'slow.tsi')
x <- rbind(x,y)

ggplot(filter(x, year == 2099), aes(x = t.anom, fill = trader)) + geom_density() + geom_vline(xintercept = filter(mdl.co2@future, year == 2099)$t.anom, size = 1)


ggplot(df, aes(x = delta.t, fill = trader)) + geom_density()
