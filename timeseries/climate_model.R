#
#
#
library(nlme)
library(tseries)
library(dplyr)

cm_debug <- FALSE

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
  for(p in 0:max_p) {
    q_start = ifelse(p == 0, 1, 0)
    for (q in q_start:max_q) {
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
  if (cm_debug) 
    message("COR (", paste(names(coef(arma)), coef(arma), collapse = ", ", sep = " = "), ")")
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
  fc_message <- fixed("false convergence (8)")
  if (cm_debug)
    message("Fitting model with arma(", 
          paste(names(coef(arma)), coef(arma), collapse = ", ", sep="="),  
          ")")
  fit_model_ <- function(f, data, correlation) {
    tryCatch(force(eval(substitute(gls(f, data = data, correlation = correlation),
                                   list(f = eval(f))))),
             error = function(e) {
               if (str_detect(as.character(e$message), fc_message)) {
                 warning("False convergence: retrying with optim")
                 tryCatch(force(eval(substitute(gls(f, data = data, 
                                                    correlation = correlation, 
                                                    control = glsControl(opt = "optim")),
                                                list(f = eval(f))))),
                          error = function(e) {
                            if (str_detect(as.character(e$message), fc_message)) {
                              warning("False convergence with optim: retrying with LS")
                              force(eval(substitute(gls(f, data = data, 
                                                        correlation = correlation, 
                                                        method="LS"),
                                                    list(f = eval(f)))))
                            } else {
                              stop("Unexpected error 2: ", e)
                            }
                          })
               } else {
                 stop("Unexpected error 1: ", e)
               }
             })
  }
  model <- fit_model_(f, data, cs)
  invisible(model)
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
  if (TRUE) 
    message("predict_future(", paste(c(mdl@covariates[[1]], n_today, n_horizon, covars[[1]]), 
                                      collapse = ", "), ")")
            
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
                                 auto_arma = all(is.na(c(p,q))), 
                                 max_p = max_p, max_q = max_q)
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

update_model <- function(mdl, n_today, n_horizon, trader_covars = NULL, 
                         samples = 10000, arma_model = NULL, auto_arma = FALSE,
                         max_p = 2, max_q = 2) {
  if (is.null(trader_covars)) trader_covars <- mdl@covariates
  n_today <- as.integer(n_today)
  n_horizon <- as.integer(n_horizon)
  message("update_model: ", n_today, ", ", n_horizon, ", ", trader_covars,
          ", ", samples, ", arma is ", ifelse(is.null(arma), "NULL", mdl@label), ", ",  
          auto_arma, ", ", max_p, ", ", max_q)
  prediction <- predict_future(mdl, n_today, n_horizon, trader_covars, 
                               samples, arma_model, auto_arma,
                               max_p, max_q)$prediction
  mdl@today <- n_today
  mdl@horizon <- n_today + n_horizon
  mdl@prediction <- prediction
  invisible(mdl)
}

range_check <- function(x, t.range, closed) {
  if (closed %in% c("closed", "left")) {
    if (is.na(t.range[1])) {
      within_lower <- TRUE
    } else {
      within_lower <- x >= t.range[1]
    }
  } else {
    if (is.na(t.range[1])) {
      within_lower <- TRUE
    } else {
      within_lower <- x > t.range[1]
    }
  }
  if (closed %in% c('closed', 'right')) {
    if (is.na(t.range[2])) {
      within_upper <- TRUE
    } else {
      within_upper <- x <= t.range[2]
    }
  } else {
    if (is.na(t.range[2])) {
      within_upper <- TRUE
    } else {
      within_upper <- x < t.range[2]
    }
  }
  prob <- sum(within_lower & within_upper) / length(x)
  prob
}

interval_prob <- function(mdl, n_horizon, t.range, closed = c('open', 'closed', 'left', 'right')) {
  closed = match.arg(closed)
  
  if (n_horizon > mdl@horizon) stop("n_horizon(", n_horizon, ") > mdl@horizon(", mdl@horizon, ")")
  prediction <- mdl@prediction %>% filter(step == mdl@today + n_horizon)
  range_check(prediction$t.anom, t.range, closed)
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
