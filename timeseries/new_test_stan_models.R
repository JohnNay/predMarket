library(rstan)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(tseries)
library(ggplot2)
library(shinystan)

theme_set(theme_bw(base_size = 20))

source('prepare_data.R')

scale_data <- function(data) {
  sd.mean <- data %>% select(-year) %>% summarize_each(funs(mean(., na.rm=T))) %>% 
    gather(key = var, value = mean)
  sd.sd <- data %>% select(-year) %>% summarize_each(funs(sd(., na.rm=T))) %>%
    gather(key = var, value = sd)
  sdf <- inner_join(sd.mean, sd.sd, by = 'var')
  scaled_data <- data %>% mutate_each_(funs((. - mean(.,na.rm=T))/sd(.,na.rm=T)), 
                                       vars = list(~-year))
  invisible(list(data = scaled_data, scale = sdf))
}

unscale_projection <- function(proj, scale) {
  scale <- scale %>% filter(var == 't.anom')
  proj %>% mutate(t.anom = t.anom.s * scale$sd + scale$mean)
}

scale_covar <- function(data, covariate, scale) {
  scale <- scale %>% filter(var == covariate)
  data[,covariate] <- (data[,covariate] - scale$mean) / scale$sd
  data
}

apply_scale <- function(data, scale) {
  for (n in discard(names(data), ~ .x == 'year')) {
    data <- scale_covar(data, n, scale)
  }
  data
}

data <- prepare_climate_data('rcp 4.5')

climate_data <- data$data
future_data <- data$future

scd <- scale_data(climate_data)
sfd <- apply_scale(future_data, scd$scale)

climate_data$t.anom.s <- scd$data$t.anom
climate_data$log.co2.s <- scd$data$log.co2

future_data$log.co2.s <- sfd$log.co2

lin.mod <- lm(t.anom.s ~ log.co2.s, data = climate_data)
res <- residuals(lin.mod)

var_scale <- 10

b <- summary(lin.mod)$coef['(Intercept)','Estimate']
sb <- var_scale * summary(lin.mod)$coef['(Intercept)','Std. Error']

m <- summary(lin.mod)$coef['log.co2.s','Estimate']
sm <- var_scale * summary(lin.mod)$coef['log.co2.s','Std. Error']

arma.mod <- arma(res, c(1,0))
phi <- summary(arma.mod)$coef['ar1',' Estimate']
sphi <- var_scale * summary(arma.mod)$coef['ar1',' Std. Error']
#theta <- summary(arma.mod)$coef['ma1',' Estimate']
#stheta <- var_scale * summary(arma.mod)$coef['ma1',' Std. Error']

sigma <- sd(res)

future_data$t.anom.s <- b + m * future_data$log.co2.s + arima.sim(arma.mod, nrow(future_data), sd = sigma)

stan_pars <- list(T = nrow(future_data),
                  b0 = b, sb0 = sb, m0 = m, sm0 = sm,
                  ssig0 = var_scale * sigma,
                  phi0 <- phi, sphi0 = sphi 
                  #theta0 = theta, stheta0 = stheta
                  )

stan_data <- c(stan_pars, list(y = future_data$t.anom.s, x = future_data$log.co2.s))

mdl.stan <- stan_model('test_1.stan')

fit <- sampling(mdl.stan, data = stan_data, chains = 4, iter = 800, cores = 1, verbose = T, show_messages = TRUE)


