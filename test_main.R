#
# test_main.R Test the main function
#
rm(list = ls())
options(warn = 2)
source('main.R')

SHOW_CLIMATE_PLOTS <- TRUE
TRACE_CLIMATE_MODEL <- TRUE
PARALLEL_STAN <- FALSE

if (TRUE) {
  burn.in <- 120
  n.seq <- 14
  horizon <- 6
} else {
  burn.in <- 120
  n.seq <- 2
  horizon <- 6
}

x <- main(parameters = c(runif(3, min = 0.0001, max = 0.9999), # seg, ideo, risk.tak
                         0, # true model: 0 = slow.tsi, 1 = log.co2
                         runif(2, min = 0.0001, max = 0.9999)), # n.edge and n.traders
          iterations = 1,
          burn.in = burn.in,
          n.seq = n.seq,
          horizon = horizon,
          nyears = burn.in + n.seq * horizon
)

y <- main(parameters = c(runif(3, min = 0.0001, max = 0.9999), # seg, ideo, risk.tak
                         1, # true model: 0 = slow.tsi, 1 = log.co2
                         runif(2, min = 0.0001, max = 0.9999)), # n.edge and n.traders
          iterations = 1,
          burn.in = burn.in,
          n.seq = n.seq,
          horizon = horizon,
          nyears = burn.in + n.seq * horizon
)
