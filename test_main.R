#
# test_main.R Test the main function
#
rm(list = ls())
options(warn = 1)
source('main.R')
if (TRUE) {
  burn.in <- 135
  n.seq <- 14
  horizon <- 6
} else {
  burn.in <- 135
  n.seq <- 2
  horizon <- 6
}
x <- main(c(runif(3, min = 0.0001, max = 0.9999), 0, 
            runif(2, min = 0.0001, max = 0.9999)), 
          iterations = 1,
          burn.in = burn.in,
          n.seq = n.seq,
          horizon = horizon,
          nyears = burn.in + n.seq * horizon
)
