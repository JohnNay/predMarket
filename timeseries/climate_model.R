#
#
#
# library(nlme)
library(tseries)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(rstan)

options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

cm_debug <- FALSE

setClass("climate_model", 
         slots = list(
           climate="data.frame",     # data frame with columns year, t.anom, and covariates
           covariate="character",    # covariate affecting temperature
           burn_in = "integer",      # time-steps before starting
           future_len = "integer",   # time-steps after starting
           label = "character",
           scaled = "data.frame",
           scale.coefs = "data.frame",
           prior.coefs = "list",
           future = "data.frame",
           scaled.future = "data.frame",
           future.scale.coefs = "data.frame",
           today = "integer",
           horizon = "integer",
           prediction = "data.frame"
         ),
         prototype = list(
           climate=data.frame(),
           covariates=as.character(NA),
           burn_in = as.integer(NA),
           future_len = as.integer(NA),
           label=as.character(NA),
           scaled = data.frame(),
           scale_coefs = data.frame(),
           prior.coefs = list(),
           future = data.frame(), # true path for simulated temperature after burn-in,
           scaled.future = data.frame(),
           future.scale.coefs = data.frame(),
           today = as.integer(NA),
           horizon = as.integer(NA),
           prediction = data.frame()
         )
)

auto_arma <- function(res, max_p = 2, max_q = 2) {
  best.model <- NULL
  best.aic <- NA
  for(p in 0:max_p) {
    q_start = ifelse(p == 0, 1, 0)
    for (q in q_start:max_q) {
      model.arma <-  arma(res, order=c(p,q), include.intercept = FALSE, method="BFGS")
      model.aic <- summary(model.arma)$aic
      if(is.null(best.model) || model.aic < best.aic) {
        best.model <- model.arma
        best.aic <- model.aic
      }
    }
  }
  invisible(best.model)
}

extract_covar <- function(data, covariate) {
  data <- data %>% select_(~year, ~t.anom, covar = covariate)
}

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
  proj %>% mutate(t.anom = t.anom * scale$sd + scale$mean)
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

gen_prior_coefs <- function(scaled_data, covariate, p = 1, q = 1,
                            max_p = 2, max_q = 2, auto_arma = FALSE) {
  scaled_data <- extract_covar(scaled_data, covariate)
  lin.model <- lm(t.anom ~ covar, data = scaled_data)
  res <- residuals(lin.model)
  
  if (auto_arma) {
    arma.model <- auto_arma(res, max_p, max_q)
  } else {
    arma.model <- arma(res, order=c(p,q), include.intercept = FALSE, method="BFGS")
  }
  
  arma.s <- summary(arma.model)
  p <- arma.s$p
  q <- arma.s$q
  
  s <- summary(lin.model)
  
  prior.coefs <- c(
    P = p, Q = q,
    list(
      ssig0 = s$sigma, 
      m0 = s$coef['covar','Estimate'], sm0 = 3 * s$coef['covar','Std. Error'],
      b0 = s$coef['(Intercept)','Estimate'], sb0 = 3 * s$coef['(Intercept)','Std. Error']
    )
  )
  
  if (p > 0) {
    prior.coefs <- c(prior.coefs,
                     list(
                       phi0 = arma.s$coef['ar1', ' Estimate'], 
                       sphi0 = arma.s$coef['ar1', ' Std. Error']
                     ))
  } else {
    prior.coefs <- c(prior.coefs,
                     list(
                       phi0 = 0, 
                       sphi0 = 0
                     ))
  }
  
  if (q > 0) {
    prior.coefs <- c(prior.coefs,
                     list(
                       theta0 = arma.s$coef['ma1', ' Estimate'], 
                       stheta0 = arma.s$coef['ma1', ' Std. Error']
                     ))
  } else {
    prior.coefs <- c(prior.coefs,
                     list(
                       theta0 = 0,
                       stheta0 = 0
                     ))
  }
  prior.coefs
}

fit_model <- function(scaled_data, covariate, prior.coefs, 
                      scaled_future_covar  = NULL,
                      sim.reps = 1,
                      n_sample = 800, n_chains = 4) {
  scaled_data <- extract_covar(scaled_data, covariate)
  this.year <- max(scaled_data$year)
  message("Compiling Stan Model")
  model <- stan_model('climate_model.stan', 'ARMA climate model', auto_write = TRUE)
  parameters <- c('b', 'm', 'sigma', 'phi', 'theta', 'y_future')
  if (is.null(scaled_future_covar)) {
    t.future = 0
    reps = 0
    scaled_future_covar = numeric(0)
  } else {
    scaled_future_covar <- scaled_future_covar %>% select_(covar = covariate) %>% unlist()
    t.future <- length(scaled_future_covar)
    reps = ceiling(sim.reps * 2.0 / (n_sample * n_chains))
  }
  stan_data <- list(T = nrow(scaled_data), T_future = t.future, 
                    y = scaled_data$t.anom, x = scaled_data$covar,
                    x_future = scaled_future_covar, reps = reps)
  stan_data <- c(stan_data, prior.coefs[! names(prior.coefs) %in% names(stan_data)])
  message("Sampling from Stan Model")
  fit <- sampling(model, data = stan_data, pars = parameters, 
                  chains = n_chains, iter = n_sample)
  message("Done sampling")
  if (t.future > 0) {
    projection <- rstan::extract(fit, pars = 'y_future')$y_future %>% as.data.frame() %>% 
      mutate(iteration = row_number()) %>%
      gather(key = key, value = t.anom, -iteration) %>% 
      separate(key, c('year','rep'), convert=T) %>%
      mutate(year = year + this.year)
  } else {
    projection <- data.frame(year = numeric(0),
                             rep = numeric(0),
                             iteration = numeric(0),
                             t.anom = numeric(0))
  }
  invisible(list(fit = fit, projection = projection))
}

setMethod("initialize", "climate_model",
          function(.Object, ...) {
            .Object <- callNextMethod()
            .Object
          }
)

predict_future <- function(mdl, covariate, n_today, n_horizon,
                           p.samples = 10000, 
                           prior.coefs = NULL,
                           gen_priors = FALSE,
                           p = 1, q = 1, 
                           auto_arma = FALSE, max_p = 2, max_q = 2,
                           n_sample = 800, n_chains = 4,
                           trace = FALSE) {
  if (TRUE) 
    message("predict_future(", paste(c(mdl@covariate, n_today, n_horizon, covariate), 
                                     collapse = ", "), ")")
  
  past_data <- mdl@scaled.future %>% head(n_today)
  start.year <- max(past_data$year)
  if (is.null(prior.coefs)) {
    if (gen_priors) {
      prior.coefs<- gen_prior_coefs(past_data$data, covariate, 
                                    p = p, q = q,
                                    auto_arma = auto_arma, max_p = max_p, max_q = max_q)
    } else {
      prior.coefs = mdl@prior.coefs
    }
  }
  
  future_data <- mdl@scaled.future %>% slice(n_today + 1:n_horizon)
  
  fit <- fit_model(past_data, covariate, prior.coefs, future_data)
  
  invisible(fit)
}


#
# init_model
#
init_model <- function(mdl, n_history, n_future, true_covar, future_covars, 
                       max_p = 2, max_q = 2, p = NA, q = NA) {
  # all_covars = column names for all future covariates, not just the ones we're using for the true model.
  if (is.null(future_covars)) {
    all_covars <- names(mdl@climate)
  } else {
    all_covars <- names(future_covars)
  }
  all_covars <- discard(all_covars, ~ .x %in% c('year', 't.anom'))
  n_history <- as.integer(n_history)
  n_future <- as.integer(n_future)
  max_p <- as.integer(max_p)
  max_q <- as.integer(max_q)
  if (is.na(p)) {
    if (! is.na(q)) 
      p <- 0
  } else if (is.na(q)) {
    q <- 0
  }
  p <- as.integer(p)
  q <- as.integer(q)
  
  sdata <- scale_data(mdl@climate)
  mdl@scaled <- sdata$data
  mdl@scale.coefs <- sdata$scale
  
  mdl@burn_in <- as.integer(n_history)
  mdl@future_len <- as.integer(n_future)
  
  mdl@covariate <- true_covar
  
  future <- future_covars %>% mutate(t.anom = NA)
  future$t.anom[1:n_history] <- mdl@climate$t.anom[1:n_history]
  mdl@future <- future
  
  fsdata <- apply_scale(future, mdl@scale.coefs)
  mdl@scaled.future <- fsdata
  mdl@future.scale.coefs <- mdl@scale.coefs
  
  mdl@prior.coefs <- gen_prior_coefs(mdl@scaled.future[1:n_history,], true_covar,
                                     p = p, q = q, 
                                     max_p = max_p, max_q = max_q,
                                     auto_arma = any(is.na(c(p,q))))
  
  if (is.null(q) || is.na(q)) {
    mdl@label <- "NULL"
  } else if (q > 0) {
    if (p > 0) {
      mdl@label <- with(am, paste0("ARMA(", p, ',', q, ")"))
    } else {
      mdl@label <- with(am, paste0("MA(", q, ")"))
    }
  } else {
    mdl@label <- with(am, paste0("AR(", p, ")"))
  }
  
  true_future  <- predict_future(mdl, true_covar, 
                                 n_today = n_history, n_horizon = n_future, 
                                 covar = true_covar)
  
  if (n_future > 0) {
    tf <- rstan::extract(true_future$fit, pars = 'y_future', permuted = FALSE, inc_warmup = FALSE)
    sp <- get_sampler_params(true_future$fit, inc_warmup = FALSE)
    df <- data.frame()
    for(i in seq_along(sp)) {
      mtd <- attr(true_future$fit@sim$samples[[i]], 'args')$control$max_treedepth
      nd <- sp[[i]][,'n_divergent__'] > 0
      td <- sp[[i]][,'treedepth__'] >= mtd
      x <- as.data.frame(tf[,i,])[! any(nd,td),]
      df <- rbind(df, x)
    }
    df <- df %>% sample_n(1) %>%
      gather(key = key, value = t.anom) %>% 
      mutate(key = str_replace(key, 'y_future\\[([:digit:]+),([:digit:]+)\\]','\\1,\\2')) %>%
      separate(key, c('year','rep'), remove=T, convert=T)
    r = base::sample(max(df$rep), 1)
    df <- df %>% filter(rep == r)
    
    mdl@scaled.future$t.anom[n_history + seq_len(n_future)] <- df$t.anom
    mdl@future$t.anom[n_history + seq_len(n_future)] <-  
      unscale_projection(df, mdl@future.scale.coefs)$t.anom
    
  }
  mdl@today <- n_history
  mdl@horizon <- n_history
  mdl@prediction = data.frame(sim = 1, step = n_history, t.anom = mdl@future$t.anom[n_history])
  invisible(mdl)
}


update_model <- function(mdl, n_today, n_horizon, 
                         trader_covar = NULL, 
                         p.samples = 10000, 
                         p = NA, q = NA, 
                         auto_arma = FALSE,
                         max_p = 2, max_q = 2,
                         n_sample = 800, n_chains = 4) {
  if (is.null(trader_covars)) trader_covars <- mdl@covariates
  n_today <- as.integer(n_today)
  n_horizon <- as.integer(n_horizon)
  max_p <- as.integer(max_p)
  max_q <- as.integer(max_q)
  if (is.na(p)) {
    if (! is.na(q)) 
      p <- 0
  } else if (is.na(q)) {
    q <- 0
  }
  p <- as.integer(p)
  q <- as.integer(q)
  
  if (any(is.na(c(p,q))) && ! auto_arma) {
    p <- mdl@prior.coefs$P
    q <- mdl@prior.coefs$Q
  }
  message("update_model: ", n_today, ", ", n_horizon, ", ", trader_covars,
          ", ", p.samples, ", arma is ", 
          ifelse(is.na(p), paste0("(",p,",",q,")"), mdl@label), 
          ", auto_arma is ", auto_arma, ", ", max_p, ", ", max_q)
  
  prediction  <- predict_future(mdl, n_today = n_history, n_horizon = n_future, 
                                covar = trader_covar,
                                gen_priors = TRUE,
                                p = p, q = q, 
                                auto_arma = auto_arma, max_p = max_p, max_q = max_q,
                                p.samples = p.samples,
                                n_sample = n_sample, n_chains = n_chains)
  
  mdl@today <- n_today
  mdl@horizon <- n_today + n_horizon
  mdl@prediction <- prediction
  invisible(mdl)
}

#
# Note: because findInterval returns 0 to length(intervals), 
# The length of the binned output is length(intervals) + 1
#
bin_prob <- function(mdl, n_horizon, intervals) {
  if (n_horizon > mdl@horizon) stop("n_horizon(", n_horizon, ") > mdl@horizon(", mdl@horizon, ")")
  prediction <- mdl@prediction %>% filter(step == mdl@today + n_horizon)
  if (any(is.na(prediction$t.anom))) warning("NA values in temperature prediction")
  x <- findInterval(prediction$t.anom, intervals)
  levels <- seq(0, length(intervals))
  if (! all(x %in% levels)) stop("Error in bin_prob: unexpected values from findInterval")
  if (any(is.na(x))) warning("NA values in findIntervals result")
  x <- factor(x, levels = levels)
  t <- table(x)
  if (length(t) != length(intervals) + 1) warning("Length mismatch in climate_model::bin_prob")
  bins <- t / nrow(prediction)
  if (sum(bins) != 1) 
    message("Probability leakage in bin_prob: ", formatC(sum(bins), digits = 5, format='f'))
  bins  
}
