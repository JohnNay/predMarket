#
#
#
# library(nlme)
library(tseries)
library(dplyr)
library(tidyr)
library(stringr)
# library(purrr)

# This is so rstan:::rstan_load_time will get updated for the purpose of loading the stan model.
if ('package:rstan' %in% search())
  detach('package:rstan', unload=TRUE)
library(rstan)

rstan_options(auto_write = TRUE)

TRACE_CLIMATE_MODEL <- FALSE
PARALLEL_STAN <- FALSE
WHICH_MODEL <- 'default'
STAN_REFRESH <- NA

model_path = normalizePath('climate_model.stan')
model_path_arma11 = normalizePath('climate_model_arma11.stan')
model_path_ar1 = normalizePath('climate_model_ar1.stan')


climate_model <- NULL
climate_model_ts <- NA

climate_model_arma11 <- NULL
climate_model_arma11_ts <- NA

climate_model_ar1 <- NULL
climate_model_ar1_ts <- NA

cm_debug <- FALSE

paste_with_names <- function(x) {
  paste(names(x), x, sep = " = ", collapse = ", ")
}

get_model <- function() {
  if (is.null(climate_model) || file.info(model_path)$mtime > climate_model_ts) {
    if (TRACE_CLIMATE_MODEL) {
      message("Compiling ARMA(p,q) Model")
    }
    ptm <- proc.time()
    climate_model <<- stan_model(model_path, "ARMA(p,q) Climate Model", 
                                 auto_write = TRUE, save_dso = TRUE,
                                 verbose = FALSE)
    ptm <- head(proc.time() - ptm, 3)
    if (TRACE_CLIMATE_MODEL) {
      message("Done compiling: ", paste_with_names(round(ptm, 2)))
    }
    climate_model_ts <<- Sys.time()
  }
  invisible(climate_model)
}

get_model_arma11 <- function() {
  if (is.null(climate_model_arma11) || 
      file.info(model_path_arma11)$mtime > climate_model_arma11_ts) {
    if (TRACE_CLIMATE_MODEL) {
      message("Compiling ARMA(1,1) Model")
    }
    ptm <- proc.time()
    climate_model_arma11 <<- stan_model(model_path_arma11, "ARMA(1,1) Climate Model", 
                                 auto_write = TRUE, save_dso = TRUE,
                                 verbose = FALSE)
    ptm <- head(proc.time() - ptm, 3)
    if (TRACE_CLIMATE_MODEL) {
      message("Done compiling: ", paste_with_names(round(ptm, 2)))
    }
    climate_model_arma11_ts <<- Sys.time()
  }
  invisible(climate_model_arma11)
}

get_model_ar1 <- function() {
  if (is.null(climate_model_ar1) || 
      file.info(model_path_ar1)$mtime > climate_model_ar1_ts) {
    if (TRACE_CLIMATE_MODEL) {
      message("Compiling AR(1) Model")
    }
    ptm <- proc.time()
    climate_model_ar1 <<- stan_model(model_path_ar1, "AR(1) Climate Model", 
                                        auto_write = TRUE, save_dso = TRUE,
                                        verbose = FALSE)
    ptm <- head(proc.time() - ptm, 3)
    if (TRACE_CLIMATE_MODEL) {
      message("Done compiling: ", paste_with_names(round(ptm, 2)))
    }
    climate_model_ar1_ts <<- Sys.time()
  }
  invisible(climate_model_ar1)
}



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
  p_start <- ifelse(max_q > 0, 0, 1)
  for(p in p_start:max_p) {
    q_start = ifelse(p > 0, 0, 1)
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
  if ('t.anom' %in% names(data))
    data <- data %>% select_(~year, ~t.anom, covar = covariate)
  else
    data <- data %>% select_(~year, covar = covariate)
  invisible(data)
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
  proj %>% mutate(t.anom = t.anom.s * scale$sd + scale$mean)
}

scale_covar <- function(data, covariate, scale) {
  scale <- scale %>% filter(var == covariate)
  data[,covariate] <- (data[,covariate] - scale$mean) / scale$sd
  data
}

apply_scale <- function(data, scale) {
  nd <- names(data)
  nd <- nd[nd != 'year']
  for (n in nd) {
    data <- scale_covar(data, n, scale)
  }
  data
}

gen_prior_coefs <- function(scaled_data, covariate, p = 1, q = 1,
                            max_p = 2, max_q = 2, auto_arma = FALSE,
                            doubt = 10) {
  scaled_data <- extract_covar(scaled_data, covariate)
  lin.model <- lm(t.anom ~ covar, data = scaled_data)
  res <- residuals(lin.model)
  
  if (WHICH_MODEL == 'arma11') {
    p <- 1
    q <- 1
    auto_arma <- FALSE
  } else if (WHICH_MODEL == 'ar1') {
    p <- 1
    q <- 0
    auto_arma <- FALSE
  }
  
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
      m0 = s$coef['covar','Estimate'], 
      sm0 = doubt * s$coef['covar','Std. Error'],
      b0 = s$coef['(Intercept)','Estimate'], 
      sb0 = doubt * s$coef['(Intercept)','Std. Error']
    )
  )
  
  if (p > 0) {
    prior.coefs <- c(prior.coefs,
                     list(
                       phi0 = arma.s$coef['ar1', ' Estimate'], 
                       sphi0 = doubt * arma.s$coef['ar1', ' Std. Error']
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
                       stheta0 = doubt * arma.s$coef['ma1', ' Std. Error']
                     ))
  } else {
    prior.coefs <- c(prior.coefs,
                     list(
                       theta0 = 0,
                       stheta0 = 0
                     ))
  }
  # to ensure identifiability, -1 < theta < 1
  if (abs(prior.coefs$theta0) > 1) {
    prior.coefs$theta0 <- 1 / prior.coefs$theta0
  }
  # to enxure stationarity -1 < phi < 1
  # prior.coefs$phi0 <- min(0.999, max(-0.999, prior.coefs$phi0))
  if (TRACE_CLIMATE_MODEL) {
    message("Generating priors for ", covariate, ": ", 
            paste_with_names(signif(unlist(prior.coefs, use.names = TRUE), 2)))
  }
  prior.coefs
}

compare_priors <- function(fit, priors) {
  pars <- c('b','m','sigma','phi','theta')
  if (priors$P == 0) pars <- pars[pars != 'phi']
  if (priors$Q == 0) pars <- pars[pars != 'theta']
  means <- as.data.frame(t(get_posterior_mean(fit, pars=pars)))['mean-all chains',]
  means$b <- (means$b - priors$b0) / priors$sb0
  means$m <- (means$m - priors$m0) / priors$sm0
  means$sigma <- means$sigma / priors$ssig0
  if (priors$P > 0)
    for (i in 1:priors$P) {
      index <- paste0('phi[',i,']')
      means[,index] <- (means[,index] - priors$phi0) / priors$sphi0
    }
  if (priors$Q > 0) {
    index <- paste0('theta[',i,']')
    means[,index] <- (means[,index] - priors$theta0) / priors$stheta0
  }
  if (TRACE_CLIMATE_MODEL) {
    message("Prior z-scores: ", paste_with_names(signif(unlist(means, use.names = TRUE), 2)))
  }
}

fit_model <- function(scaled_data, covariate, prior.coefs, 
                      scaled_future_covar  = NULL,
                      sim.reps = 1,
                      n_sample = 800, n_chains = 4,
                      filter_results = TRUE,
                      threshold = 0.25) {
  scaled_data <- extract_covar(scaled_data, covariate)
  this.year <- max(scaled_data$year)
  if (WHICH_MODEL == 'arma11') {
    model = get_model_arma11()
  } else if (WHICH_MODEL == 'ar1') {
    model <- get_model_ar1()
  } else {
    model <- get_model()
  }
  if (is.null(scaled_future_covar)) {
    t.future = 0
    reps = 0
    scaled_future_covar = numeric(0)
  } else {
    scaled_future_covar <- scaled_future_covar %>% select_(covar = covariate) %>% 
      unlist()
    if (length(scaled_future_covar) == 1) {
      scaled_future_covar <- array(scaled_future_covar, dim = 1)
    }
    t.future <- length(scaled_future_covar)
    reps = ceiling(sim.reps * 2.0 / (n_sample * n_chains))
  }
  
  if (TRACE_CLIMATE_MODEL) {
    message("fit_model: history = ", nrow(scaled_data), " years, future = ", 
            length(scaled_future_covar), " years. sim.reps = ", sim.reps, 
            ", reps = ", reps, ".")
  }
  
  parameters <- c('b', 'm', 'sigma', 'phi', 'theta', 'y_future')
  if (WHICH_MODEL == 'ar1') {
    parameters <- parameters[parameters != 'theta']
  }
  stan_data <- list(T = nrow(scaled_data), T_future = t.future, 
                    y = scaled_data$t.anom, x = scaled_data$covar,
                    x_future = scaled_future_covar, reps = reps)
  stan_data <- c(stan_data, prior.coefs[! names(prior.coefs) %in% names(stan_data)])
  if (WHICH_MODEL == 'arma11') {
    stan_data$P <- 1
    stan_data$Q <- 1
  } else if (WHICH_MODEL == 'ar1') {
    stan_data$P <- 1
    stan_data$Q <- 0
  }
  
  if (PARALLEL_STAN) {
    n_cores <- min(n_chains, parallel::detectCores())
  } else {
    n_cores <- 1
  }
  
  if (TRACE_CLIMATE_MODEL) {
    message("Sampling from Stan Model")
    ptm <- proc.time()
  }
  if (is.na(STAN_REFRESH)) {
    refresh <- max(n_sample / 10, 1)
  } else {
    refresh <- STAN_REFRESH
  }
  # prevent stop for sampling warnings when warn = 2
  suppressWarnings(
    fit <- sampling(model, data = stan_data, pars = parameters, 
                    chains = n_chains, iter = n_sample,
                    cores = n_cores,
                    refresh = refresh)
  )
  compare_priors(fit, prior.coefs)
  sp <- get_sampler_params(fit, inc_warmup = FALSE)
  nd <- c()
  td <- c()
  mtd <- attr(fit@sim$samples[[1]], 'args')$control$max_treedepth
  td_max <- 0
  for(i in seq_along(sp)) {
    nd <- c(nd, sp[[i]][,'n_divergent__'] > 0)
    td <- c(td, sp[[i]][,'treedepth__'] > mtd)
    td_max <- max(td_max, sp[[i]][,'treedepth__'])
  }
  if (TRACE_CLIMATE_MODEL) {
    message("Excessive bad steps in stan simulation: ", sum(nd|td))
  }
  if (TRACE_CLIMATE_MODEL) {
    message(sum(nd|td), " bad samples: ", sum(nd), " divergent steps and ", sum(td), " steps exceeded max_treedepth")
    # message('Max treedepth = ', td_max, ', limit = ', mtd)
    ptm <- head(proc.time() - ptm, 3)
    message("Done sampling: ", paste_with_names(round(ptm, 2)))
  }
  if (t.future > 0) {
    if (filter_results) {
      df <- rstan::extract(fit, pars = 'y_future', permuted = FALSE, inc_warmup = FALSE)
      sp <- get_sampler_params(fit, inc_warmup = FALSE)
      projection <- data.frame()
      for(i in seq_along(sp)) {
        mtd <- attr(fit@sim$samples[[i]], 'args')$control$max_treedepth
        nd <- sp[[i]][,'n_divergent__'] > 0
        td <- sp[[i]][,'treedepth__'] > mtd
        x <- as.data.frame(df[,i,])[! (nd|td), , drop=F]
        if (nrow(x) > 0)
          projection <- rbind(projection, x)
      }
      noo <- names(projection)
      if (! any(grepl('^y_future', noo))) {
        browser()
      }
      projection <- projection %>%
        mutate(iteration = row_number()) %>%
        gather(key = key, value = t.anom.s, -iteration)
      foo <- projection$key
      projection <- projection %>%
        mutate(key = str_replace(key, 'y_future\\[([:digit:]+),([:digit:]+)\\]','\\1,\\2'))
      bar <- projection$key
      projection <- projection %>%
        separate(key, c('step','rep'), remove=T, convert=T)
      if (is.character(projection$step))
        warning("Projection$step is not numeric. ", noo[[1]], ", ", foo[[1]], " ", 
                bar[[1]], ", ", projection$step[[1]], ", ", projection$rep[[1]])
    } else {
      projection <- rstan::extract(fit, pars = 'y_future')$y_future %>% as.data.frame() %>% 
        mutate(iteration = row_number()) %>%
        gather(key = key, value = t.anom.s, -iteration) %>% 
        separate(key, c('step','rep'), convert=T)
    }
    if (TRACE_CLIMATE_MODEL && FALSE) {
      message(with(projection, paste0("Step: (", paste(range(step), collapse=", "),
                                      ") rep: (", paste(range(rep), collapse = ", "),
                                      ") iteration: (", paste(range(iteration), collapse = ", "),
                                      "), this.year = ", this.year)))
    }
    projection <- projection %>%
      mutate(year = step + this.year, sim = iteration * max(rep) + rep) %>%
      mutate(step = step + nrow(scaled_data)) %>%
      select(step, year, iteration, rep, sim, t.anom.s)
  } else {
    projection <- data.frame(step = numeric(0),
                             year = numeric(0),
                             iteration = numeric(0),
                             rep = numeric(0),
                             sim = numeric(0),
                             t.anom.s = numeric(0))
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
                           n_sample = 800, n_chains = 4) {
  if (TRACE_CLIMATE_MODEL) 
    message("predict_future(", paste(c(mdl@covariate, n_today, n_horizon, covariate), 
                                     collapse = ", "), ")")
  
  past_data <- mdl@scaled.future %>% head(n_today)
  start.year <- max(past_data$year)
  if (is.null(prior.coefs)) {
    if (gen_priors) {
      prior.coefs<- gen_prior_coefs(past_data, covariate, 
                                    p = p, q = q,
                                    auto_arma = auto_arma, max_p = max_p, max_q = max_q)
    } else {
      prior.coefs = mdl@prior.coefs
    }
  }
  
  future_data <- mdl@scaled.future %>% slice(n_today + 1:n_horizon)
  if (TRACE_CLIMATE_MODEL && FALSE) {
    message(" future data sliced from ", n_today + 1, " to ", n_today + n_horizon, " and has ", nrow(future_data), " rows.")
  }
  
  fit <- fit_model(past_data, covariate, prior.coefs, future_data,
                   sim.rep = p.samples)
  fit$projection <- unscale_projection(fit$projection, mdl@future.scale.coefs)
  
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
  all_covars <- all_covars[! all_covars %in% c('year', 't.anom')]
  n_history <- as.integer(n_history)
  n_future <- as.integer(n_future)
  max_p <- as.integer(max_p)
  max_q <- as.integer(max_q)
  auto_arma <- FALSE
  if (is.na(p) && is.na(q)) {
    auto_arma <- TRUE
  } else if (is.na(p)) {
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
  
  if (is.null(future_covars))
    future_covars <- mdl@climate %>% select_(~year, .dots = all_covars)
  future <- future_covars %>% slice(seq_len(n_history+n_future)) %>% mutate(t.anom = NA)
  future$t.anom[1:n_history] <- mdl@climate$t.anom[1:n_history]
  mdl@future <- future
  
  fsdata <- apply_scale(future, mdl@scale.coefs)
  mdl@scaled.future <- fsdata
  mdl@future.scale.coefs <- mdl@scale.coefs
  
  mdl@prior.coefs <- gen_prior_coefs(mdl@scaled.future[1:n_history,], true_covar,
                                     p = p, q = q, 
                                     max_p = max_p, max_q = max_q,
                                     auto_arma = auto_arma)
  
  p <- mdl@prior.coefs$P
  q <- mdl@prior.coefs$Q
  
  if (is.null(q) || is.na(q)) {
    mdl@label <- "NULL"
  } else if (q > 0) {
    if (p > 0) {
      mdl@label <- paste0("ARMA(", p, ',', q, ")")
    } else {
      mdl@label <- paste0("MA(", q, ")")
    }
  } else {
    mdl@label <- paste0("AR(", p, ")")
  }
  
  if (n_future > 0) {
    true_future  <- predict_future(mdl, covariate = true_covar, 
                                   n_today = n_history, n_horizon = n_future, 
                                   p.samples = 1)
    
    tf <- rstan::extract(true_future$fit, pars = 'y_future', permuted = FALSE, inc_warmup = FALSE)
    sp <- get_sampler_params(true_future$fit, inc_warmup = FALSE)
    df <- data.frame()
    for(i in seq_along(sp)) {
      mtd <- attr(true_future$fit@sim$samples[[i]], 'args')$control$max_treedepth
      nd <- sp[[i]][,'n_divergent__'] > 0
      td <- sp[[i]][,'treedepth__'] > mtd
      x <- as.data.frame(tf[,i,])[! any(nd,td),]
      df <- rbind(df, x)
    }
    df <- df %>% sample_n(1) %>%
      gather(key = key, value = t.anom.s) %>% 
      mutate(key = str_replace(key, 'y_future\\[([:digit:]+),([:digit:]+)\\]','\\1,\\2')) %>%
      separate(key, c('year','rep'), remove=T, convert=T)
    r = base::sample(max(df$rep), 1)
    df <- df %>% filter(rep == r) %>% unscale_projection(mdl@future.scale.coefs)
    
    mdl@scaled.future$t.anom[n_history + seq_len(n_future)] <- df$t.anom.s
    mdl@future$t.anom[n_history + seq_len(n_future)] <-  df$t.anom
    
    if (nrow(df) != n_future) {
      warning("Mismatched lengths: Simulation is ", nrow(df), " years, but future is",
              n_future, " years.")
    }
    if (nrow(mdl@scaled.future) != n_history + n_future)
      warning("Mismatched lengths: mdl@scaled.future is ", nrow(mdl@scaled.future), " years, but history is ",
              n_history, " years and future is ", n_future, " years.")
    if (nrow(mdl@future) != n_history + n_future)
      warning("Mismatched lengths: mdl@future is ", nrow(mdl@future), " years, but history is ",
              n_history, " years and future is ", n_future, " years.")
  }
  mdl@today <- n_history
  mdl@horizon <- n_history
  mdl@prediction = data.frame(sim = 1,  rep = 1, iteration = 1,
                              year = mdl@future$year[n_history],
                              step = n_history,
                              t.anom = mdl@future$t.anom[n_history])
  if (TRACE_CLIMATE_MODEL) {
    message("Model initialized. future is ", nrow(mdl@future), " years (",
            tail(mdl@future$year,1),"): Temp range is ",
            formatC(min(mdl@future$t.anom, na.rm=T), digits=2, format='g'), " -- ", 
            formatC(max(mdl@future$t.anom, na.rm=T), digits=2, format='g'),
            " (", 
            formatC(min(mdl@scaled.future$t.anom, na.rm=T), digits=2, format='g'), " -- ", 
            formatC(max(mdl@scaled.future$t.anom, na.rm=T), digits=2, format='g'),
            ") with ", sum(is.na(mdl@prediction$t.anom)), " NA's")
    if (any(is.na(mdl@future$t.anom))) {
      message("NAs at ", paste(which(is.na(mdl@future$t.anom)), collapse = ", "))
    }
    message("")
  }
  
  invisible(mdl)
}


update_model <- function(mdl, n_today, n_horizon, 
                         trader_covar = NULL, 
                         p.samples = 10000, 
                         p = NA, q = NA, 
                         auto_arma = FALSE,
                         max_p = 2, max_q = 2,
                         n_sample = 800, n_chains = 4) {
  if (is.null(trader_covar)) trader_covar <- mdl@covariates
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
  if (TRACE_CLIMATE_MODEL && FALSE) {
    message("update_model: ", n_today, ", ", n_horizon, ", ", trader_covar,
            ", ", p.samples, ", arma is ", 
            ifelse(is.na(p), paste0("(",p,",",q,")"), mdl@label), 
            ", auto_arma is ", auto_arma, ", ", max_p, ", ", max_q)
  }
  
  prediction  <- predict_future(mdl, n_today = n_today, n_horizon = n_horizon, 
                                covar = trader_covar,
                                gen_priors = TRUE,
                                p = p, q = q, 
                                auto_arma = auto_arma, max_p = max_p, max_q = max_q,
                                p.samples = p.samples,
                                n_sample = n_sample, n_chains = n_chains)
  
  mdl@today <- n_today
  mdl@horizon <- n_today + n_horizon
  mdl@prediction <- prediction$projection
  
  if (TRACE_CLIMATE_MODEL) {
    message("Model updated prediction from ", n_today, " to ", n_today + n_horizon, 
            " (", mdl@future$year[n_today + n_horizon], ") for ", mdl@covariate, ":", trader_covar, 
            ": Temp range is ",
            formatC(min(mdl@prediction$t.anom, na.rm=T), digits=2, format='g'), " -- ", 
            formatC(max(mdl@prediction$t.anom, na.rm=T), digits=2, format='g'),
            " (", 
            formatC(min(mdl@prediction$t.anom.s, na.rm=T), digits=2, format='g'), " -- ", 
            formatC(max(mdl@prediction$t.anom.s, na.rm=T), digits=2, format='g'),
            ") with ", sum(is.na(mdl@prediction$t.anom)), " NA's")
    if (any(is.na(mdl@prediction$t.anom))) {
      message("NAs at ", paste(which(is.na(mdl@prediction$t.anom)), collapse = ", "))
    }
    message("")
  }
  
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
  if (any(is.na(x))) warning(sum(is.na(x)), " NA values in findIntervals result")
  x <- factor(x, levels = levels)
  t <- table(x)
  if (any(is.na(t))) warning( sum(is.na(t)), " NA values in table: x = ", x, ", table = ", t)
  if (length(t) != length(intervals) + 1) warning("Length mismatch in climate_model::bin_prob")
  bins <- t / nrow(prediction)
  if (any(is.na(bins))) {
    warning( sum(is.na(bins)), " NA values in bins: ", bins, 
             " (table = ", t, ", nrow = ", nrow(prediction),")")
  }
  if (! isTRUE(all.equal(sum(bins, na.rm=T), 1, tolerance = 1E-6)))
    warning("Probability leakage in bin_prob: ", formatC(sum(bins), digits = 5, format='f'))
  bins  
}

plot_model <- function(mdl, trader.covar = NA) {
  require(ggplot2)
  
  if (mdl@covariate == 'log.co2') {
    cvr <- quote(log(CO[2]))
  } else if(mdl@covariate == 'slow.tsi') {
    cvr <- "Slow TSI"
  }
  else cvr <- ''
  
  if (trader.covar == 'log.co2') {
    cvr.t <- quote(log(CO[2]))
    lab.t <- ', Trader covar: '
  } else if(trader.covar == 'slow.tsi') {
    cvr.t <- "Slow TSI"
    lab.t <- ', Trader covar: '
  } else {
    cvr.t <- ''
    lab.t <- ''
  }
  
#  n5 <- fivenum(mdl@prediction$t.anom)[c(1,5)]
  n5 <- quantile(mdl@prediction$t.anom, c(0.02,0.98))
  predictions <- mdl@prediction %>% filter(t.anom >= n5[1] & t.anom <= n5[2] & sim %in% sample(sim, 500))
  
  p <- ggplot(mdl@future, aes(x = year, y = t.anom)) + 
    # simulated future temperatures
    geom_point() + geom_line() + 
    # vertical bands: predictions
    geom_point(data = predictions, alpha = 0.02) + 
    # actual temperatures
    geom_point(data = mdl@climate, color = 'blue') +
    labs(x = "Year", y = expression(T[anomaly]), 
         title = substitute(paste("Climate Model: ", x, lab.t, x.t), 
                            list(x = cvr, x.t = cvr.t, lab.t = lab.t)))
  plot(p)
  invisible(p)
}
