# master script for conducting ABM experiments
rm(list=ls()) # make sure to always run this line of code and see that the next two 
# lines of code work without error, ensuring that the working of the model is not 
# dependent on anything in your global workspace, if it is, then you need to create 
# whatever is in your global workpace in the code that is sourced in the next two lines of code.
source("main.R")
# test
s <- append(runif(4, min = 0.0001, max = 0.9999), sample(0:1, 1))
main(parameters = s, out = "converg", visu = TRUE)

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
plot(sobol, "Convergence of Beliefs", legend_pos = "bottomright")

pc <- eat::pc_sa(abm = main, 
                 input_values = input_values,
                 out = "converg", 
                 sample_count = 5000, 
                 nboot = 1000, 
                 parallel = TRUE,
                 cores = 25,
                 rank = TRUE, method = "pcc") 
save(pc, file = "output/pc.Rda")
plot(pc, outcome_var = "Convergence of Beliefs")

##############################################################################
## collecting the value of outcome.converge for the same value of the parameters 
# we test on the sa, and plot them as an histogram
##############################################################################
if (!require(eat)) devtools::install_github("JohnNay/eat", 
                                            auth_token = "08d34f040cbe8c95d89477741ceb450a9cfa42c4")
input_values <- lapply(list(seg = NA, ideo = NA, risk.tak = NA,
                            market.complet = NA), 
                       function(x) list(random_function = "qunif",
                                        ARGS = list(min = 0.0001, max = 0.9999)))
input_values$true.model <- list(random_function = "qbinom",
                                ARGS = list(size = 1, prob = 0.5))
input_names <- names(input_values)
cores <- parallel::detectCores() - 1
sample_count <- 7000
input_set <- create_set(input_values, input_names, sample_count = sample_count,
                        constraints = "none")
# Simulation runs with generated input factor sets:
doParallel::registerDoParallel(cores = cores)
source("main.R")
sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input_set)), .combine='c'), {
  main(as.numeric(input_set[i, ]), out = "converg")
})
d <- data.frame(convergence = sim, input_set)
save(d, file = "output/d.Rda")

library(ggplot2)
ggplot2::ggplot(d, aes(x = convergence)) + geom_histogram(binwidth = 0.04) +
  xlim(c(-0.6, 0.6)) +
  ylab("Count") + xlab("Convergence of Beliefs") + 
  ggtitle(paste(sample_count, 
                 "Convergence of Belief Outcomes with LHS Input Parameters")) +
  theme_bw()

d$true.model <- factor(d$true.model)
library(plyr)
cdat <- plyr::ddply(d, "true.model", summarize, outcome_mean = mean(convergence))
ggplot2::ggplot(d, aes(x = convergence, fill = true.model)) + 
  geom_density(alpha = 0.3) +
  # xlim(c(-0.6, 0.6)) +
  ylab("Density") + xlab("Convergence of Beliefs") + 
  ggtitle(paste(sample_count, 
                "Convergence of Belief Outcomes with LHS Input Parameters")) +
  geom_vline(data=cdat, aes(xintercept= outcome_mean,  color = true.model),
             linetype="dashed", size=1) + 
  theme_bw()

##############################################################################
## Sensitivity analysis on all variables
##############################################################################
rm(list=ls())
source("main2.R")
# test once
s <- c(runif(4, min = 0.0001, max = 0.9999), sample(0:1, 1), runif(2, min = 0.0001, max = 0.9999))
outcome.evolution <- main2(parameters = s, out = "converg", visu = TRUE, record = TRUE)
# run 100
doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
runs <- 100
outcome.evolution <- foreach::`%dopar%`(foreach::foreach(i=seq(runs), .combine='rbind'), {
  main2(parameters = c(runif(4, min = 0.0001, max = 0.9999), sample(0:1, 1), runif(2, min = 0.0001, max = 0.9999)), 
        out = "converg", visu = FALSE, record = TRUE)
})
average_convergence <- data.frame(avg = colMeans(outcome.evolution), 
                                  trading_seq = seq(length(colMeans(outcome.evolution))))
save(average_convergence, file = "output/average_convergence.Rda")
library(ggplot2)
ggplot(data=average_convergence, aes(x= trading_seq, y=avg)) +
  geom_line() + ggtitle("Average convergence \nas a function of the number of trading sequences") +
  xlab("Number of trading sequences") + ylab(paste0("Average convergence (n = ", runs, ")"))

# SA
input_values <- lapply(list(seg = NA, ideo = NA, risk.tak = NA,
                            market.complet = NA), 
                       function(x) list(random_function = "qunif",
                                        ARGS = list(min = 0.0001, max = 0.9999)))
input_values$true.model <- list(random_function = "qbinom",
                                ARGS = list(size = 1, prob = 0.5))
input_values$n.edg <- list(random_function = "qunif",
                           ARGS = list(min = 0.0001, max = 0.9999))
input_values$n.traders <- list(random_function = "qunif",
                               ARGS = list(min = 0.0001, max = 0.19999))
library(eat)
pc2 <- eat::pc_sa(abm = main2, 
                  input_values = input_values,
                  out = "converg", 
                  sample_count = 10000, 
                  nboot = 1000, 
                  parallel = TRUE,
                  cores = 30,
                  rank = TRUE, method = "pcc") 
save(pc2, file = "output/pc2.Rda")
plot(pc2, outcome_var = "Convergence of Beliefs")

# Standardized Regression Coefficient
src2 <- eat::pc_sa(abm = main2, 
                  input_values = input_values,
                  out = "converg", 
                  sample_count = 10000, 
                  nboot = 1000, 
                  parallel = TRUE,
                  cores = 30,
                  rank = TRUE, method = "src") 
save(src2, file = "output/src2.Rda")
plot(src2, outcome_var = paste0("Convergence of Beliefs (R^2= ", src2@r_squared, ")"))
plot(src2, outcome_var = "Convergence of Beliefs")
