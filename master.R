# master script for conducting ABM experiments
rm(list=ls()) # make sure to always run this line of code and see that the next two 
# lines of code work without error, ensuring that the working of the model is not 
# dependent on anything in your global workspace, if it is, then you need to create 
# whatever is in your global workpace in the code
source("main.R")
# devtools::install_github("JohnNay/eat", 
#                          auth_token = "08d34f040cbe8c95d89477741ceb450a9cfa42c4")
library(eat)

test <- FALSE
estimate_replicates <- FALSE

if(test){
  main(c(runif(3, min = 0.0001, max = 0.9999), 1, runif(2, min = 0.0001, max = 0.9999)))
  main(c(runif(3, min = 0.0001, max = 0.9999), 0, runif(2, min = 0.0001, max = 0.9999)))
}

##############################################################################
## Param distributions to draw from
##############################################################################
input_values <- lapply(list(seg = NA, ideo = NA, risk.tak = NA), 
                       function(x) list(random_function = "qunif",
                                        ARGS = list(min = 0.0001, max = 0.9999)))
input_values$true.model <- list(random_function = "qbinom",
                                ARGS = list(size = 1, prob = 0.5))
input_values$n.edg <- list(random_function = "qunif",
                           ARGS = list(min = 0.0001, max = 0.9999))
input_values$n.traders <- list(random_function = "qunif",
                               ARGS = list(min = 0.0001, max = 0.19999))

if(estimate_replicates){
  ##############################################################################
  ## Estimate number of replicates needed for each param set
  ##############################################################################
  iters_res <- compute_iters(main, input_values, "converg", 
                             initial_iters = 1,
                             max_iters = 20, 
                             sample_count = 60, parallel = TRUE, cores = 30,
                             measure = "sd", thresh = 0.05, repeats = 100)
  save(iters_res, file = "output/iters_res.Rda")
  plot(iters_res, ylab = "100 Standard Deviations", 
       outcome = "Samples of 60 Convergence of Beliefs Outcomes")
  summary(iters_res)
}

##############################################################################
## Main sens analysis
##############################################################################

# pc2 <- pc_sa(abm = main, 
#              input_values = input_values,
#              out = "converg", 
#              sample_count = 10000, 
#              nboot = 1000, 
#              parallel = TRUE,
#              cores = 30,
#              rank = TRUE, method = "pcc")
# save(pc2, file = "output/pc2.Rda")
# plot(pc2, outcome_var = "Convergence of Beliefs")
future <- TRUE

# Standardized Regression Coefficient
if(!future){
  main2 <- function(parameters,
                    iterations = 10,
                    burn.in = 51,
                    n.seq = 14,
                    horizon = 6,
                    nyears = 135,
                    out){
    main(parameters = parameters,
         iterations = iterations,
         burn.in = burn.in,
         n.seq = n.seq,
         horizon = horizon,
         nyears = nyears)
  }
  src <- pc_sa(abm = main2, 
               input_values = input_values,
               out = "converg", 
               iterations = 7,
               sample_count = 196,
               nboot = 1000, 
               parallel = TRUE,
               cores = 30,
               rank = TRUE, method = "src")
  save(src, file = "output/src.Rda")
  plot(src, outcome_var = paste0("Convergence of Beliefs \n (R^2= ", round(src@r_squared, 2), ")"))
  # src2 <- pc_sa(abm = main, 
  #              input_values = input_values,
  #              previous_pc_sa = list(src),
  #              out = "converg", 
  #              iterations = 7,
  #              sample_count = 30,
  #              nboot = 1000, 
  #              parallel = TRUE,
  #              cores = 30,
  #              rank = TRUE, method = "src")
  # save(src2, file = "output/src2.Rda")
}

if(future){
    main2 <- function(parameters,
                      iterations = 10,
                      burn.in = 135,
                      n.seq = 14,
                      horizon = 6,
                      nyears = 219,
                      out){
      main(parameters = parameters,
           iterations = iterations,
           burn.in = burn.in,
           n.seq = n.seq,
           horizon = horizon,
           nyears = nyears)
    }
  src_future <- pc_sa(abm = main2, 
               input_values = input_values,
               out = "converg", 
               iterations = 7,
               sample_count = 16,
               nboot = 1000, 
               parallel = TRUE,
               cores = 30,
               rank = TRUE, method = "src")
  save(src_future, file = "output/src_future.Rda")
}

# ##############################################################################
# ## collecting the value of outcome.converge for the same value of the parameters 
# # we test on the sa, and plot them as an histogram
# ##############################################################################
# 
# 
# cores <- parallel::detectCores() - 1
# sample_count <- 7000
# 
# input_set <- create_set(input_values, input_names, sample_count = sample_count,
#                         constraints = "none")
# # Simulation runs with generated input factor sets:
# doParallel::registerDoParallel(cores = cores)
# source("main.R")
# sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input_set)), .combine='c'), {
#   main(as.numeric(input_set[i, ]), out = "converg")
# })
# d <- data.frame(convergence = sim, input_set)
# save(d, file = "output/d.Rda")
# 
# library(ggplot2)
# ggplot2::ggplot(d, aes(x = convergence)) + geom_histogram(binwidth = 0.04) +
#   xlim(c(-0.6, 0.6)) +
#   ylab("Count") + xlab("Convergence of Beliefs") + 
#   ggtitle(paste(sample_count, 
#                  "Convergence of Belief Outcomes with LHS Input Parameters")) +
#   theme_bw()
# 
# d$true.model <- factor(d$true.model)
# library(plyr)
# cdat <- plyr::ddply(d, "true.model", summarize, outcome_mean = mean(convergence))
# ggplot2::ggplot(d, aes(x = convergence, fill = true.model)) + 
#   geom_density(alpha = 0.3) +
#   # xlim(c(-0.6, 0.6)) +
#   ylab("Density") + xlab("Convergence of Beliefs") + 
#   ggtitle(paste(sample_count, 
#                 "Convergence of Belief Outcomes with LHS Input Parameters")) +
#   geom_vline(data=cdat, aes(xintercept= outcome_mean,  color = true.model),
#              linetype="dashed", size=1) + 
#   theme_bw()
# 
# ##############################################################################
# ## random analysis on all variables
# ##############################################################################
# set <- list()
# set[[1]] <- list(burn.in = 4, n.seq = 28, horizon = 4)
# set[[2]] <- list(burn.in = 6, n.seq = 22, horizon = 5)
# set[[3]] <- list(burn.in = 16, n.seq = 20, horizon = 5)
# stopifnot(all(sapply(set, function(x) (x$burn.in + x$n.seq * x$horizon) == 116)))
# average_convergence <- data.frame()
# for(j in set){
#   sample_count <- 1000
#   input_values <- lapply(list(seg = NA, ideo = NA, risk.tak = NA,
#                               market.complet = NA), 
#                          function(x) list(random_function = "qunif",
#                                           ARGS = list(min = 0.0001, max = 0.9999)))
#   input_values$true.model <- list(random_function = "qbinom",
#                                   ARGS = list(size = 1, prob = 0.5))
#   input_values$n.edg <- list(random_function = "qunif",
#                              ARGS = list(min = 0.0001, max = 0.9999))
#   input_values$n.traders <- list(random_function = "qunif",
#                                  ARGS = list(min = 0.0001, max = 0.19999))
#   input_names <- names(input_values)
#   # TRUE model is Human-induced
#   input_values$true.model <- list(random_function = "qbinom",
#                                   ARGS = list(size = 1, prob = 1))
#   input_set <- create_set(input_values, input_names, sample_count = sample_count,
#                                constraints = "none")
#   doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
#   outcome.evolution <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input_set)), .combine='rbind'), {
#     main(parameters = as.numeric(input_set[i, ]), 
#           burn.in = j$burn.in, n.seq = j$n.seq, horizon = j$horizon,
#           out = "converg", visu = FALSE, record = TRUE)
#   })
#   average_convergence <- rbind(average_convergence,
#                                data.frame(avg = colMeans(outcome.evolution), 
#                                           trading_seq = seq(length(colMeans(outcome.evolution))),
#                                           true_mod = "Human-Induced",
#                                           set = paste(as.character(j), collapse = ", ")))
#   # TRUE model is Natural Change
#   input_values$true.model <- list(random_function = "qbinom",
#                                   ARGS = list(size = 1, prob = 0))
#   input_set <- create_set(input_values, input_names, sample_count = sample_count,
#                           constraints = "none")
#   doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
#   outcome.evolution <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input_set)), .combine='rbind'), {
#     main(parameters = as.numeric(input_set[i, ]), 
#           burn.in = j$burn.in, n.seq = j$n.seq, horizon = j$horizon,
#           out = "converg", visu = FALSE, record = TRUE)
#   })
#   average_convergence <- rbind(average_convergence,
#                                data.frame(avg = colMeans(outcome.evolution), 
#                                           trading_seq = seq(length(colMeans(outcome.evolution))),
#                                           true_mod = "Natural",
#                                           set = paste(as.character(j), collapse = ", ")))
# }
# 
# save(average_convergence, file = "output/average_convergence.Rda")
# 
# library(ggplot2)
# plot_data <- average_convergence
# plot_data[ , "set"] <- factor(plot_data[ , "set"], levels = gtools::mixedsort(unique(plot_data[ , "set"])))
# ggplot(data=plot_data, aes(x= trading_seq, y=avg, color = true_mod)) +
#   geom_smooth(method = "loess") + #geom_line() +
#   ggplot2::facet_wrap(~set, ncol = 1) +
#   ggtitle("Average Convergence Over Trading Sequences") +
#   xlab("Trading Sequences") + ylab(paste0("Average Convergence (n = ", sample_count, ")")) + 
#   theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0)) + 
#   scale_color_discrete(name="True Model")
# 
