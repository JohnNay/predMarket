#
# test_main.R Test the main function
#
options(warn = 2)
source('main.R')
x <- main(c(runif(3, min = 0.0001, max = 0.9999), 0, runif(2, min = 0.0001, max = 0.9999)), iterations = 1)
