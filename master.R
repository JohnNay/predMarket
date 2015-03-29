# master script for conducting ABM experiments
rm(list=ls())
source("main.R")
main(market.struct = "CDA")

# param is vec length 2 created by either a stochastic metaheuristic optimization method
# or a sampling scheme for a sens analysis
# and main() takes in param and returns a scalar quant measure of the outcome var
# so , TODO: decide what the important 
# results <- main(seg = param[1], ideo = param[2], outcome = "segreg")




# # TODO: create a function to put in "abm" arg of sobol_sa() func 
# # This abm func will just be a slight modification of the existing main() func.
# # TODO: create a list to put in "input_values" arg of sobol_sa() func 
# # that looks like this: (just change param1 to the name of the actual param)
# 
# input_values <- lapply(list(param1 = NA, param2 = NA), 
#                        function(x) list(random_function = "qunif",
#                                         ARGS = list(min = 0, max = 1)))
# # if there are any params that are binary valued give them a binom prior distribution:
# input_values[["param2"]] <- list(random_function = "qbinom",
#                                  ARGS = list(size = 1, prob = 0.5))
# 
# 
# source("sobol_sa.R")
# sa_results <- sobol_sa(abm = , input_values = ,
#                        out = "segreg", 
#                        sample_count = 100, 
#                        sobol_nboot = 1000, 
#                        iterations = NULL,
#                        parallel = TRUE)
# 
