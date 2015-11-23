#
# test_main.R Test the main function
#
rm(list = ls())
options(warn = 2)
source('main.R')
# x <- main(c(runif(3, min = 0.0001, max = 0.9999), 0, runif(2, min = 0.0001, max = 0.9999)), iterations = 1,
#           burn.in = 135,
#           n.seq = 14,
#           horizon = 6,
#           nyears = 219)
x <- main(c(runif(3, min = 0.0001, max = 0.9999), 0, runif(2, min = 0.0001, max = 0.9999)), iterations = 1,
     burn.in = 51,
     n.seq = 14,
     horizon = 6,
     nyears = 135,
     out = "converg", record = TRUE)