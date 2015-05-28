# master script for conducting ABM experiments
rm(list=ls()) # make sure to always run this line of code and see that the next two 
# lines of code work without error, ensuring that the working of the model is not 
# dependent on anything in your global workspace, if it is, then you need to create 
# whatever is in your global workpace in the code that is sourced in the next two lines of code.
source("main.R")
# test
s <- append(runif(4, min = 0.0001, max = 0.9999), sample(0:1, 1))
main(parameters = s, out = "converg")


# ######
# ##   Record values of outcomes through time and plot average through iterations
# ######
# 
# s <- runif(4, min = 0.0001, max = 0.9999)
# outcome.evolution <- main(parameters = s, out = "converg", record = TRUE)
# 
# for (j in 1:99){
#   s <- runif(4, min = 0.0001, max = 0.9999)
#   outcome.evolution <- rbind(outcome.evolution, main(parameters = s, out = "converg", record = TRUE))
# }
# 
# average.convergence <- colMeans(outcome.evolution)
# 
# plot(average.convergence, type = "b", main = "Average convergence \nas a function of the number of trading sequences",
#                                       xlab = "Number of trading sequences",
#                                       ylab = "Average convergence (n = 100)")

#####
## Sensitivity analysis
#####

devtools::install_github("JohnNay/eat", 
                         auth_token = "08d34f040cbe8c95d89477741ceb450a9cfa42c4")
library(eat)
?sobol_sa

input_values <- lapply(list(seg = NA, ideo = NA, risk.tak = NA,
                            market.complet = NA), 
                       function(x) list(random_function = "qunif",
                                        ARGS = list(min = 0.0001, max = 0.9999)))
input_values$true.model <- list(random_function = "qbinom",
                                ARGS = list(size = 1, prob = 0.5))
# # if there are any params that are binary valued give them a binom prior distribution:
# input_values[["param2"]] <- list(random_function = "qbinom",
#                                  ARGS = list(size = 1, prob = 0.5))

sobol <- sobol_sa(abm = main, 
                       input_values = input_values,
                       out = "converg", 
                       sample_count = 3000, 
                       sobol_nboot = 1000, 
                       parallel = TRUE,
                       cores = 25)
save(sobol, file = "output/sobol.Rda")
plot_sobol(sobol, "Convergence of Beliefs", legend_pos = "bottomright")

pc <- eat::pc_sa(abm = main, 
                 input_values = input_values,
                 out = "converg", 
                 sample_count = 5000, 
                 nboot = 1000, 
                 parallel = TRUE,
                 cores = 25,
                 rank = TRUE, method = "pcc") 
save(pc, file = "output/pc.Rda")
plot_pc(pc, "Convergence of Beliefs")

##############################################################################
## collecting the value of outcome.converge for the same value of the parameters 
# we test on the sa, and plot them as an histogram
devtools::install_github("JohnNay/eat", 
                         auth_token = "08d34f040cbe8c95d89477741ceb450a9cfa42c4")
library(eat)
input_values <- lapply(list(seg = NA, ideo = NA, risk.tak = NA,
                            market.complet = NA), 
                       function(x) list(random_function = "qunif",
                                        ARGS = list(min = 0.0001, max = 0.9999)))
input_values$true.model <- list(random_function = "qbinom",
                                ARGS = list(size = 1, prob = 0.5))
# Get names of input factors:
input_names <- names(input_values)
cores <- parallel::detectCores() - 1
input.set <- create_set(input_values, input_names, sample_count=5000,
                        constraints = "none")
# Simulation runs with generated input factor sets:
doParallel::registerDoParallel(cores = cores)
sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input.set)), .combine='c'), {
  main(as.numeric(input.set[i, ]), out = "converg")
})
library(ggplot2)
d <- data.frame(convergence = sim)
ggplot2::ggplot(d, aes(x=convergence)) + geom_histogram(binwidth=0.04) +
  xlim(c(-0.6, 0.6)) +
  ylab("Count") + xlab("Convergence of Beliefs") + 
  ggtitle("5000 Convergence of Belief Outcomes with LHS Input Parameters") +
  theme_bw()





