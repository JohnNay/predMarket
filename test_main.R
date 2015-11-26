#
# test_main.R Test the main function
#
rm(list = ls())
options(warn = 2)
source('main.R')

SHOW_CLIMATE_PLOTS <- TRUE
TRACE_CLIMATE_MODEL <- TRUE
PARALLEL_STAN <- FALSE
WHICH_MODEL <- 'ar1'

max_p <- 1
max_q <- 0

if (TRUE) {
  burn.in <- 120
  n.seq <- 14
  horizon <- 6
} else {
  burn.in <- 120
  n.seq <- 2
  horizon <- 6
}

ptm0 <- proc.time()
x <- main(parameters = c(runif(3, min = 0.0001, max = 0.9999), # seg, ideo, risk.tak
                         0, # true model: 0 = slow.tsi, 1 = log.co2
                         runif(2, min = 0.0001, max = 0.9999)), # n.edge and n.traders
          iterations = 1,
          burn.in = burn.in,
          n.seq = n.seq,
          horizon = horizon,
          nyears = burn.in + n.seq * horizon
)
ptm1 <- proc.time()
cat("First process finished in", paste_with_names(prettyNum(head(ptm1 - ptm0,3), big.mark=',')), 'seconds')

y <- main(parameters = c(runif(3, min = 0.0001, max = 0.9999), # seg, ideo, risk.tak
                         1, # true model: 0 = slow.tsi, 1 = log.co2
                         runif(2, min = 0.0001, max = 0.9999)), # n.edge and n.traders
          iterations = 1,
          burn.in = burn.in,
          n.seq = n.seq,
          horizon = horizon,
          nyears = burn.in + n.seq * horizon
)
ptm2 <- proc.time()
cat("Second process finished in", paste_with_names(prettyNum(head(ptm2 - ptm1,3), big.mark=',')), 'seconds')
