#
#
#
library(nlme)
library(tseries)
library(dplyr)


setClass("climate_model", 
         slots = list(
           climate="data.frame",     # data frame with columns year, t.anom, and covariates
           covariates="list",        # covariates affecting temperature
           burn_in = "integer",      # time-steps before starting
           future_len = "integer",   # time-steps after starting
           label = "character",
           p = "integer",
           q = "integer",
           arma="list",
           model="list",
           future = "data.frame",
           today = "integer",
           horizon = "integer",
           prediction = "data.frame"
         ),
         prototype = list(
           climate=data.frame(),
           covariates=list(),
           burn_in = as.integer(NA),
           future_len = as.integer(NA),
           label=as.character(NA),
           p = as.integer(NA),
           q = as.integer(NA),
           arma= as.list(NULL),
           model=as.list(NULL),
           future = data.frame(), # true path for simulated temperature after burn-in,
           today = as.integer(NA),
           horizon = as.integer(NA),
           prediction = data.frame()
         )
)

setGeneric("mdl_formula", 
           function(x) {
             standardGeneric("mdl_formula")
           }
)

setMethod("mdl_formula", "list", 
          function(x) {
            as.formula(paste0('t.anom ~ ', paste(x, collapse=" + ")))
          }
)

setMethod("mdl_formula", "character", 
          function(x) {
            as.formula(paste0('t.anom ~ ', paste(x, collapse=" + ")))
          }
)



setMethod("mdl_formula", signature = "climate_model",
          function(x) {
            as.formula(paste0('t.anom ~ ', paste(x@covariates, collapse=" + ")))
          }
)


setGeneric("fit_arma", function(p, q, res, covariates, data) {
  standardGeneric("fit_arma")
})

setMethod("fit_arma", signature(res = "numeric", covariates = "missing", data = "missing"),
          function(p, q, res, covariates, data) {
            invisible(arma(res, order=c(p,q), include.intercept = FALSE, method="BFGS"))
          }
)


setMethod("fit_arma", signature(res = "missing", covariates = "list", data = "data.frame"),
          function(p, q, res, covariates, data) {
            if (is.null(res)) {
              f <- mdl_formula(covariates)
              
              lin.model <- lm(f, data = data)
              res <- residuals(lin.model)
            }
            fit_arma(p, q, res)
          }
)

arma_model <- function(data, covariates, p, q, res = NULL) {
  if (is.null(res)) {
    f <- mdl_formula(covariates)
    lin.model <- lm(f, data = data)
    res <- residuals(lin.model)
  }
  model.arma <-  fit_arma(p, q, res)
  s <- summary(model.arma)
  invisible(list(p = s$p, q = s$q, aic = s$aic, model = model.arma))
}

auto_arma <- function(data, covariates, max_p = 2, max_q = 2) {
  f <- mdl_formula(covariates)
  lin.model <- lm(f, data = data)
  res <- residuals(lin.model)
  best.model <- NULL
  for(p in 1:max_p) {
    for (q in 0:max_q) {
      model.arma <-  arma_model(data, covariates, p, q, res)
      if(is.null(best.model) || model.arma$aic < best.model$aic) {
        best.model <- model.arma
      }
    }
  }
  invisible(best.model)
}

cor <- function(arma) {
  s <- summary(arma)
  p <- s$p
  q <- s$q
  if (p == 1 && q == 0) {
    cor <- corAR1(coef(arma), form = ~1)
  } else {
    cor <- corARMA(coef(arma), p = p, q = q, form = ~1)
  }
  cor
}

fit_model <- function(data, arma, covariates) {
  f <- mdl_formula(covariates)
  cols <- lapply(unlist(c('year', 't.anom', covariates)), as.name)
  select_cols <- function(...) { select(data, ...)}
  data <- do.call(select_cols, cols)
  cs <- Initialize(cor(arma), data)
  invisible(force(eval(substitute(gls(f, data = data, correlation = cs), list(f = eval(f))))))
}

setMethod("initialize", "climate_model",
          function(.Object, ...) {
            .Object <- callNextMethod()
            .Object
          }
)

predict_future <- function(mdl, n_today, n_horizon, covars, samples, arma_model = NULL, 
                           auto_arma = FALSE, max_p = 2, max_q = 2, 
                           trace = FALSE) {
  covars <- as.list(covars)
  past_data <- mdl@future[1:n_today,]
  if (is.null(arma_model)) {
    if (auto_arma)
      am <- auto_arma(past_data, covars, max_p = max_p, max_q = max_q)
    else
      am <- arma_model(past_data, covars, mdl@p, mdl@q)
    arma_model <- am$model
  } else {
    s <- summary(arma_model)
    am <- list(model = arma_model, p = s$p, q = s$q, aic = s$aic)
  }
  
  if (trace) message("About to fit gls_model: Best arma = (", am$p, ",", am$q, "), aic = ", am$aic)
  gls_model <- fit_model(past_data, arma_model, covars)
  
  if (trace) message("About to predict future temperature")
  future.temp <- predict(gls_model, newdata = mdl@future[n_today + seq_len(n_horizon), 
                                                         unlist(covars), drop=FALSE])
  past_res <- residuals(gls_model, "response")[seq_len(n_today)]
  res_sd <- sd(past_res)
  rgen <- function(n) rnorm(n, mean = 0.0, sd = res_sd)
  arma_coef <- as.data.frame(t(coef(arma_model)))
  model_coef <- list()
  arma_coef <- as.data.frame(t(coef(arma_model)))
  model_coef <- list(
    ar = unlist(arma_coef %>% select(starts_with('ar'))),
    ma = unlist(arma_coef %>% select(starts_with('ma')))
  )
  model_coef <- model_coef[! unlist(lapply(model_coef, is.null))]
  if (n_horizon > 0) {
    p_steps <- n_today + seq_len(n_horizon)
    if (trace) message("About to enter prediction loop: ", p_steps)
    prediction <- rep_len(data.frame(), samples)
    for (i in seq_len(samples)) {
      noise <- arima.sim(model = model_coef, n = n_horizon, n.start = n_today,
                         start.innov = past_res, rand.gen = rgen)
      prediction[[i]] <- data.frame(sim = i, step = p_steps, year = mdl@future$year[p_steps], t.anom = future.temp + noise)
    }
    prediction <- as.data.frame(rbind_all(prediction))
  } else {
    prediction <- data.frame()
  }
  invisible(list(arma = am, model = gls_model, prediction = prediction))
}

init_model <- function(mdl, n_history, n_future, true_covars, future_covars, 
                       max_p = 2, max_q = 2, p = NA, q = NA) {
  covars <- unlist(true_covars)
  # all_covars = column names for all future covariates, not just the ones we're using for the true model.
  if (is.null(future_covars)) {
    all_covars <- names(mdl@climate)
  } else {
  all_covars <- names(future_covars)
  }
  all_covars <- all_covars[! all_covars %in% c('year', 't.anom')]
  n_history <- as.integer(n_history)
  n_future <- as.integer(n_future)
  p <- as.integer(p)
  q <- as.integer(q)
  
  mdl@burn_in <- as.integer(n_history)
  mdl@future_len <- as.integer(n_future)

  mdl@covariates <- as.list(covars)
  mdl@future <- as.data.frame(matrix(nrow = n_history + n_future, ncol = 2 + length(all_covars)))

  colnames(mdl@future) <- c('year', 't.anom', all_covars)
  mdl@future[1:n_history,] <- mdl@climate[1:n_history, colnames(mdl@future)]
  if (n_future > 0) {
    future_indices <- n_history + seq_len(n_future)
    future_years <- mdl@future$year[n_history] + seq_len(n_future)
    missing_years <- setdiff(future_years, future_covars$year)
    if (length(missing_years) > 0) {
      future_covars <- rbind(mdl@climate[mdl@climate$year %in% missing_years, names(future_covars)], future_covars)
    }
    mdl@future$year[future_indices] <- future_years
    mdl@future[future_indices, all_covars] <- future_covars[future_covars$year %in% future_years,all_covars]
  }
  
  mdl@p <- p
  mdl@q <- q

  true_future  <- predict_future(mdl, n_history, n_future, mdl@covariates, samples = 1,
                                 auto_arma = all(is.na(c(p,q))))
  am <- true_future$arma
  arma_model <- am$model
  slot(mdl, "arma", check=FALSE) <- arma_model
  mdl@p <- as.integer(am$p)
  mdl@q <- as.integer(am$q)
  if (am$q > 0) {
    slot(mdl, "label") <- with(am, paste0("ARMA(", p, q, ")"))
  } else {
    slot(mdl, "label") <- with(am, paste0("AR(", p, ")"))
  }
  
  slot(mdl, "model", check=FALSE) <- true_future$model
  
  if (n_future > 0) {
    mdl@future$t.anom[n_history + seq_len(n_future)] <- true_future$prediction$t.anom
  }
  mdl@today <- n_history
  mdl@horizon <- n_history
  mdl@prediction = data.frame(sim = 1, step = n_history, t.anom = mdl@future$t.anom[n_history])
  invisible(mdl)
}

update_model <- function(mdl, n_today, n_horizon, trader_covars = NULL, samples = 10000, arma_model = NULL, auto_arma = FALSE) {
  if (is.null(trader_covars)) trader_covars <- mdl@covariates
  n_today <- as.integer(n_today)
  n_horizon <- as.integer(n_horizon)
  prediction <- predict_future(mdl, n_today, n_horizon, trader_covars, samples, arma_model, auto_arma)$prediction
  mdl@today <- n_today
  mdl@horizon <- n_today + n_horizon
  mdl@prediction <- prediction
  invisible(mdl)
}

interval_prob <- function(mdl, n_horizon, t.range) {
  if (n_horizon > mdl@horizon) stop("n_horizon(", n_horizon, ") > mdl@horizon(", mdl@horizon, ")")
  prediction <- mdl@prediction %>% filter(step == mdl@today + n_horizon)
  prob <- sum(findInterval(prediction$t.anom, t.range, rightmost.closed=TRUE) == 1) / nrow(prediction)
  prob
}
