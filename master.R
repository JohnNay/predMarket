# master script for conducting ABM experiments
rm(list=ls())
source("main.R")
main(market.struct = "CDA")

# param is vec length 2 created by a stochastic metaheuristic optimization method
# and main() takes in param and returns a scalar quant measure of the outcome var
# so , TODO: decide what the important 
main(seg = param[1], ideo = param[2], outcome = "price")
