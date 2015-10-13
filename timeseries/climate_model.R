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


setMethod("fit_arma", c(res = "missing", covariates = "list", data = "data.frame"),
          function(p, q, res, covariates, data) {
            if (is.null(res)) {
              f <- mdl_formula(covariates)
              
              lin.model <- lm(f, data = data)
              res <- residuals(lin.model)
            }
            fit_arma(mdl, p, q, res)
          }
)

auto_arma <- function(data, covariates, max_p = 2, max_q = 2) {
  f <- mdl_formula(covariates)
  lin.model <- lm(f, data = data)
  res <- residuals(lin.model)
  best.model <- NULL
  for(p in 1:max_p) {
    for (q in 0:max_q) {
      model.arma <-  fit_arma(p, q, res)
      if(is.null(best.model)) {
        best.model <- model.arma
      } else if (summary(model.arma)$aic < summary(best.model)$aic) {
        best.model <- model.arma
      }
    }
  }
  s <- summary(best.model)
  invisible(list(p = s$p, q = s$q, aic = s$aic, model = best.model))
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

predict_future <- function(mdl, n_today, n_horizon, samples, arma_model = NULL, trace = FALSE) {
  past_data <- mdl@future[1:n_today,]
  if (is.null(arma_model)) {
    am <- auto_arma(past_data, mdl@covariates)
    arma_model <- am$model
  } else {
    s <- summary(arma_model)
    am <- list(model = arma_model, p = s$p, q = s$q, aic = s$aic)
  }
  
  if (trace) message("About to fit gls_model: Best arma = (", am$p, ",", am$q, "), aic = ", am$aic)
  gls_model <- fit_model(past_data, arma_model, mdl@covariates)
  
  if (trace) message("About to predict future temperature")
  future.temp <- predict(gls_model, newdata = mdl@future[n_today + seq_len(n_horizon), 
                                                         unlist(mdl@covariates), drop=FALSE])
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
  p_steps <- n_today + seq_len(n_horizon)
  if (trace) message("About to enter prediction loop: ", p_steps)
  prediction <- rep_len(data.frame(), samples)
  for (i in seq_len(samples)) {
    noise <- arima.sim(model = model_coef, n = n_horizon, n.start = n_today,
                       start.innov = past_res, rand.gen = rgen)
    prediction[[i]] <- data.frame(sim = i, step = p_steps, year = mdl@future$year[p_steps], t.anom = future.temp + noise)
  }
  prediction <- as.data.frame(rbind_all(prediction))
  invisible(list(arma = am, model = gls_model, prediction = prediction))
}

init_model <- function(mdl, n_burn_in, n_future, covars, future_covars, ...) {
  covars <- unlist(covars)
  n_burn_in <- as.integer(n_burn_in)
  n_future <- as.integer(n_future)
  mdl@burn_in <- as.integer(n_burn_in)
  mdl@future_len <- as.integer(n_future)
  mdl@covariates <- as.list(covars)
  mdl@future <- as.data.frame(matrix(nrow = n_burn_in + n_future, ncol = 2 + length(covars)))
  colnames(mdl@future) <- c('year', 't.anom', covars)
  mdl@future[1:n_burn_in] <- mdl@climate[1:n_burn_in, colnames(mdl@future)]
  mdl@future$year[n_burn_in + seq_len(n_future)] <- mdl@future$year[n_burn_in] + seq_len(n_future)
  mdl@future[n_burn_in + seq_len(n_future), covars] <- future_covars[,covars]

  true_future  <- predict_future(mdl, n_burn_in, n_future, 1)
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

  mdl@future$t.anom[n_burn_in + seq_len(n_future)] <- true_future$prediction$t.anom
  mdl@today <- n_burn_in
  mdl@horizon <- n_burn_in
  mdl@prediction = data.frame(sim = 1, step = n_burn_in, t.anom = mdl@future[n_burn_in])
  invisible(mdl)
}

update_model <- function(mdl, n_today, n_horizon, samples = 10000, arma_model = NULL) {
  n_today <- as.integer(n_today)
  n_horizon <- as.integer(n_horizon)
  prediction <- predict_future(mdl, n_today, n_horizon, samples, arma_model)$prediction
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
