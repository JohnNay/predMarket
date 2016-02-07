library(rstan)
library(dplyr)
library(tidyr)
library(stringr)
library(loo)

source('prepare_data.R')

data <- prepare_climate_data('rcp 4.5')
climate_data <- data$data
future_data <- data$future

model <- stan_model('climate_model.stan', 'ARMA climate model', auto_write = TRUE, save_dso = TRUE)

cd <- climate_data
if (FALSE)
  cd <- climate_data %>% filter(year >= 1960)
if (TRUE) {
  scaled_data <- cd %>% mutate_each_(funs((. - mean(.)) / sd(.)), vars = list(~-year))
} else {
  scaled_data <- cd
}

m0 <- mean(cd$t.anom) - mean(scaled_data$t.anom)
s0 <- sd(cd$t.anom) / sd(scaled_data$t.anom)

mdl <- lm(t.anom ~ log.co2, data = scaled_data)
res <- residuals(mdl)
arma <- tseries::arma(res, c(1,1), include.intercept = 0)

parameters <- c('b', 'm', 'sigma', 'phi', 'theta', 'y_future', 'log_lik')

P <- 1
Q <- 1

stan_data <- list(T = nrow(scaled_data), T_future = nrow(scaled_data), P = P, Q = Q, 
                  y = scaled_data$t.anom, x = scaled_data$log.co2,
                  m0 = summary(mdl)$coef['log.co2', 'Estimate'], sm0 = 3 * summary(mdl)$coef['log.co2','Std. Error'], 
                  b0 = summary(mdl)$coef['(Intercept)','Estimate'], sb0 = 3 * summary(mdl)$coef['(Intercept)','Std. Error'], 
                  ssig0 = summary(mdl)$sigma, 
                  phi0 = summary(arma)$coef['ar1',' Estimate'], sphi0 = 3 * summary(arma)$coef['ar1',' Std. Error'], 
                  theta0 = summary(arma)$coef['ma1',' Estimate'], stheta0 = 3 * summary(arma)$coef['ma1',' Std. Error'], 
                  x_future = scaled_data$log.co2,
                  reps = 100)

stan_data_ar1 <- stan_data
stan_data_ar1$Q <- 0

stan_data_ma1 <- stan_data
stan_data_ma1$P <- 0

initf <- function(chain_id = 1) {
  clip <- function(x, lower = -1, upper = 1) pmin(pmax(x, lower),upper)
  list(b = clip(rnorm(1,0,0.25)), m = clip(rnorm(1,0,0.25)), 
                sigma = clip(abs(rnorm(1,0, 0.25)),1E-6,1),
                phi = list(clip(rnorm(P,0,0.25))), theta = list(clip(rnorm(Q,0,0.25))))
  if (P == 0) list$phi <- numeric(0)
  if (Q == 0) list$theta <- numeric(0)
}

n_sample <- 800

fit.1 <- sampling(model, data = stan_data, pars = parameters, 
                chains = 4, iter = n_sample, cores = 4)

fit.2 <- sampling(model, data = stan_data_ar1, 
                  pars = parameters[parameters != 'theta'], 
                chains = 4, iter = n_sample, cores = 4)

fit.3 <- sampling(model, data = stan_data_ma1, 
                  pars = parameters[parameters != 'eta'], 
                  chains = 4, iter = n_sample, cores = 4)

log_lik.1 <- extract_log_lik(fit.1)
log_lik.2 <- extract_log_lik(fit.2)
log_lik.3 <- extract_log_lik(fit.3)

compare(waic(log_lik.1), waic(log_lik.2), waic(log_lik.3))

df <- rstan::extract(fit.1, pars = 'y_future')[[1]] %>% as.data.frame() %>% 
  mutate(iteration = row_number()) %>%
  gather(key = key, value = t.anom, -iteration) %>% 
  separate(key, c('year','rep'), convert=T) %>%
  mutate(year = year + min(cd$year) - 1)
if (TRUE) {
 df <- df %>% mutate(t.anom = t.anom * s0 + m0)
}

s1 <- sample(max(df$iteration), 500, 500 > max(df$iteration))
s2 <- sample(max(df$rep), 500, 500 > max(df$rep))
sdf <- str_c(s1, s2, sep=':')

x <- df %>% filter(str_c(iteration, year, sep=':') %in% sdf)

