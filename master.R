# master script for conducting ABM experiments
rm(list=ls()) # make sure to always run this line of code and see that the next two 
# lines of code work without error, ensuring that the working of the model is not 
# dependent on anything in your global workspace, if it is, then you need to create 
# whatever is in your global workpace in the code that is sourced in the next two lines of code.

# #####
# ## Sensitivity analysis
# #####
# input_values <- lapply(list(seg = NA, ideo = NA, risk.tak = NA,
#                             market.complet = NA), 
#                        function(x) list(random_function = "qunif",
#                                         ARGS = list(min = 0.0001, max = 0.9999)))
# input_values$true.model <- list(random_function = "qbinom",
#                                 ARGS = list(size = 1, prob = 0.5))
# # # if there are any params that are binary valued give them a binom prior distribution:
# # input_values[["param2"]] <- list(random_function = "qbinom",
# #                                  ARGS = list(size = 1, prob = 0.5))
# 
# sobol <- sobol_sa(abm = main, 
#                        input_values = input_values,
#                        out = "converg", 
#                        sample_count = 3000, 
#                        sobol_nboot = 1000, 
#                        parallel = TRUE,
#                        cores = 25)
# save(sobol, file = "output/sobol.Rda")
# plot(sobol, "Convergence of Beliefs", legend_pos = "bottomright")
# 
# pc <- pc_sa(abm = main, 
#                  input_values = input_values,
#                  out = "converg", 
#                  sample_count = 5000, 
#                  nboot = 1000, 
#                  parallel = TRUE,
#                  cores = 25,
#                  rank = TRUE, method = "pcc") 
# save(pc, file = "output/pc.Rda")
# plot(pc, outcome_var = "Convergence of Beliefs")

source("main2.R")

# test once
# s <- c(runif(4, min = 0.0001, max = 0.9999), sample(0:1, 1), runif(2, min = 0.0001, max = 0.9999))
# outcome.evolution <- main2(parameters = s, out = "converg", visu = TRUE, record = TRUE)


##############################################################################
## Estimate number of replicates needed for each param set
##############################################################################
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
source("utilities/create_set.R")
source("utilities/compute_iters.R")
iters_res <- compute_iters(main2, input_values, "converg", 
                           initial_iters = 1,
                           max_iters = 20, 
                           sample_count = 30, parallel = TRUE, cores = 30,
                           measure = "sd", thresh = 0.05, repeats = 100)
plot(iters_res, ylab = "Standard Deviation")
summary(iters_res)

##############################################################################
## collecting the value of outcome.converge for the same value of the parameters 
# we test on the sa, and plot them as an histogram
##############################################################################

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
## random analysis on all variables
##############################################################################
set <- list()
set[[1]] <- list(burn.in = 4, n.seq = 28, horizon = 4)
set[[2]] <- list(burn.in = 6, n.seq = 22, horizon = 5)
set[[3]] <- list(burn.in = 16, n.seq = 20, horizon = 5)
stopifnot(all(sapply(set, function(x) (x$burn.in + x$n.seq * x$horizon) == 116)))
average_convergence <- data.frame()
for(j in set){
  sample_count <- 1000
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
  input_names <- names(input_values)
  # TRUE model is Human-induced
  input_values$true.model <- list(random_function = "qbinom",
                                  ARGS = list(size = 1, prob = 1))
  input_set <- create_set(input_values, input_names, sample_count = sample_count,
                               constraints = "none")
  doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
  outcome.evolution <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input_set)), .combine='rbind'), {
    main2(parameters = as.numeric(input_set[i, ]), 
          burn.in = j$burn.in, n.seq = j$n.seq, horizon = j$horizon,
          out = "converg", visu = FALSE, record = TRUE)
  })
  average_convergence <- rbind(average_convergence,
                               data.frame(avg = colMeans(outcome.evolution), 
                                          trading_seq = seq(length(colMeans(outcome.evolution))),
                                          true_mod = "Human-Induced",
                                          set = paste(as.character(j), collapse = ", ")))
  # TRUE model is Natural Change
  input_values$true.model <- list(random_function = "qbinom",
                                  ARGS = list(size = 1, prob = 0))
  input_set <- create_set(input_values, input_names, sample_count = sample_count,
                          constraints = "none")
  doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
  outcome.evolution <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input_set)), .combine='rbind'), {
    main2(parameters = as.numeric(input_set[i, ]), 
          burn.in = j$burn.in, n.seq = j$n.seq, horizon = j$horizon,
          out = "converg", visu = FALSE, record = TRUE)
  })
  average_convergence <- rbind(average_convergence,
                               data.frame(avg = colMeans(outcome.evolution), 
                                          trading_seq = seq(length(colMeans(outcome.evolution))),
                                          true_mod = "Natural",
                                          set = paste(as.character(j), collapse = ", ")))
}

save(average_convergence, file = "output/average_convergence.Rda")

library(ggplot2)
plot_data <- average_convergence
plot_data[ , "set"] <- factor(plot_data[ , "set"], levels = gtools::mixedsort(unique(plot_data[ , "set"])))
ggplot(data=plot_data, aes(x= trading_seq, y=avg, color = true_mod)) +
  geom_smooth(method = "loess") + #geom_line() +
  ggplot2::facet_wrap(~set, ncol = 1) +
  ggtitle("Average Convergence Over Trading Sequences") +
  xlab("Trading Sequences") + ylab(paste0("Average Convergence (n = ", sample_count, ")")) + 
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0)) + 
  scale_color_discrete(name="True Model")

##############################################################################
## Sensitivity analysis on all variables
##############################################################################
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

source("utilities/pc_sa.R") # Need to have package "sensitivity" installed.
pc2 <- pc_sa(abm = main2, 
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
src2 <- pc_sa(abm = main2, 
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
