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
           future = "data.frame"
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
           future = data.frame() # true path for simulated temperature after burn-in
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
  p <- summary(arma)$p
  q <- summary(arma)$q
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
  invisible(gls(f, data = data, correlation = cs))
}

setMethod("initialize", "climate_model",
          function(.Object, ...) {
            .Object <- callNextMethod()
            .Object
          }
)

init_model <- function(mdl, n_burn_in, n_future, covars, future_covars, ...) {
  covars <- unlist(covars)
  mdl@burn_in <- as.integer(n_burn_in)
  mdl@future_len <- as.integer(n_future)
  mdl@covariates <- as.list(covars)
  mdl@future <- as.data.frame(matrix(nrow = n_burn_in + n_future, ncol = 2 + length(covars)))
  colnames(mdl@future) <- c('year', 't.anom', covars)
  mdl@future[1:n_burn_in] <- mdl@climate[1:n_burn_in, colnames(mdl@future)]
  mdl@future$year[n_burn_in + seq_len(n_future)] <- mdl@future$year[n_burn_in] + seq_len(n_future)
  mdl@future[n_burn_in + seq_len(n_future), covars] <- future_covars[,covars]
  
  am <- auto_arma(mdl@climate, covars)
  arma_model <- am$model
  slot(mdl, "arma", check=FALSE) <- arma_model
  mdl@p <- as.integer(am$p)
  mdl@q <- as.integer(am$q)
  if (am$q > 0) {
    slot(mdl, "label") <- with(am, paste0("ARMA(", p, q, ")"))
  } else {
    slot(mdl, "label") <- with(am, paste0("AR(", p, ")"))
  }
  
  gls_model <- fit_model(mdl@climate, arma_model, mdl@covariates)
  slot(mdl, "model", check=FALSE) <- gls_model

  future.temp <- predict(gls_model, 
                         newdata = mdl@future[n_burn_in + seq_len(n_future), covars, drop=FALSE])

  
  past_res <- residuals(gls_model, "response")[seq_len(n_burn_in)]
  arma_coef <- as.data.frame(t(coef(arma_model)))
  model_coef <- list(
    ar = unlist(arma_coef %>% select(starts_with('ar'))),
    ma = unlist(arma_coef %>% select(starts_with('ma')))
  )
  model_coef <- model_coef[! unlist(lapply(model_coef, is.null))]
  
  noise <- arima.sim(model = model_coef, n = n_future, n.start = n_burn_in,
                     start.innov = past_res)
  mdl@future$t.anom[n_burn_in + seq_len(n_future)] <- future.temp + noise
  invisible(mdl)
}

