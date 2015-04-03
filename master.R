# master script for conducting ABM experiments
rm(list=ls()) # make sure to always run this line of code and see that the next two 
# lines of code work without error, ensuring that the working of the model is not 
# dependent on anything in your global workspace, if it is, then you need to create 
# whatever is in your global workpace in the code that is sourced in the next two lines of code.
source("main.R")
s <- rep(runif(1, min = 0.0001, max = 0.9999), 4)
main(parameters = s)

devtools::install_github("JohnNay/eat", 
                         auth_token = "08d34f040cbe8c95d89477741ceb450a9cfa42c4")
library(eat)
?sobol_sa

input_values <- lapply(list(seg = NA, ideo = NA, risk.tak = NA,
                            market.complet = NA), 
                       function(x) list(random_function = "qunif",
                                        ARGS = list(min = 0.0001, max = 0.9999)))
# # if there are any params that are binary valued give them a binom prior distribution:
# input_values[["param2"]] <- list(random_function = "qbinom",
#                                  ARGS = list(size = 1, prob = 0.5))

sa_results <- sobol_sa(abm = main, 
                       input_values = input_values,
                       out = "segreg", 
                       sample_count = 25, 
                       sobol_nboot = 1000, 
                       parallel = TRUE,
                       cores = 7)
plot_sobol_fo(sa_results)
plot_sobol_total(sa_results)

