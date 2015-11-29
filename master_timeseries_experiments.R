rm(list=ls())
args <- commandArgs(trailingOnly = TRUE)
numcores <- as.integer(args[1])
message(paste("Number of cores", numcores))

source("main.R")
library(eat)

##############################################################################
## Set up parameters for climate_model to control level of tracing to screen
##############################################################################

SHOW_CLIMATE_PLOTS <- TRUE  # Plot graph of temperature after each year?
TRACE_CLIMATE_MODEL <- FALSE # Show diagnostic traces for stan runs?
STAN_REFRESH <-  0           # Frequency to show stan chain progress. 0 for silent
PARALLEL_STAN <- FALSE       # Run chains in parallel?
WHICH_MODEL <- 'ar1'         # "default", "arma11", or "ar1". "ar1" is recommended for speed and stability.

max_p <- 1                   # Maximum order for AR when running auto_arma
max_q <- 0                   # Maximum order for MA when running auto_arma

plot_final <- FALSE

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

##############################################################################
## random analysis on all variables
##############################################################################
sample_count <- numcores
doParallel::registerDoParallel(cores = numcores)

set <- list()
set[[1]] <- list(n.edg = 0.95, ideo = 0.95)
set[[2]] <- list(n.edg = 0.95, ideo = 0.05)
set[[3]] <- list(n.edg = 0.05, ideo = 0.95)
set[[4]] <- list(n.edg = 0.05, ideo = 0.05)
burn.in = 51
n.seq = 14
horizon = 6
nyears = burn.in + n.seq * horizon
average_convergence <- data.frame(stringsAsFactors = FALSE)

for(j in set){
  input_values$n.edg <- list(random_function = "qunif",
                             ARGS = list(min = j$n.edg, max = j$n.edg))
  input_values$ideo <- list(random_function = "qunif",
                             ARGS = list(min = j$ideo, max = j$ideo))
  
  # TRUE model is Human-induced
  input_values$true.model <- list(random_function = "qbinom",
                                  ARGS = list(size = 1, prob = 1))
  input_set <- create_set(input_values = input_values, 
                          input_names = names(input_values), 
                          sample_count = sample_count,
                          constraints = "none",
                          model_data = NULL)
  outcome.evolution <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input_set)), .combine='c'), {
    tryCatch(
      main(parameters = as.numeric(input_set[i, ]), 
           burn.in = burn.in,
           n.seq = n.seq,
           horizon = horizon,
           nyears =  nyears,
           iterations = 1,
           record = TRUE),
      error = function(e) rep(NA, n.seq))
  })
  cat(paste("/n", n.seq, "outcome evolutions with parameters", paste(as.character(j), collapse = ", "), "/n", outcome.evolution))
  average_convergence <- rbind(average_convergence,
                               data.frame(convergence = outcome.evolution,
                                          trading_seq = rep(seq(n.seq), nrow(input_set)),
                                          true_mod = rep(rep("LogCo2", length(seq(n.seq)), nrow(input_set))),
                                          set = rep(rep(paste(as.character(j), collapse = ", "),  length(seq(n.seq))), nrow(input_set)),
                                          stringsAsFactors = FALSE)
  )
  
  # TRUE model is Sun Spots
  input_values$true.model <- list(random_function = "qbinom",
                                  ARGS = list(size = 1, prob = 0))
  input_set <- create_set(input_values = input_values, 
                          input_names = names(input_values), 
                          sample_count = sample_count,
                          constraints = "none",
                          model_data = NULL)
  outcome.evolution <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input_set)), .combine='c'), {
    tryCatch(
      main(parameters = as.numeric(input_set[i, ]), 
           burn.in = burn.in,
           n.seq = n.seq,
           horizon = horizon,
           nyears =  nyears,
           iterations = 1,
           record = TRUE),
      error = function(e) rep(NA, n.seq))
  })
  cat(paste("/n", n.seq, "outcome evolutions with parameters", paste(as.character(j), collapse = ", "), "/n", outcome.evolution))
  average_convergence <- rbind(average_convergence,
                               data.frame(convergence = outcome.evolution, 
                                          trading_seq = rep(seq(n.seq), nrow(input_set)),
                                          true_mod = rep(rep("SlowTSI", length(seq(n.seq)), nrow(input_set))),
                                          set = rep(rep(paste(as.character(j), collapse = ", "),  length(seq(n.seq))), nrow(input_set)),
                                          stringsAsFactors = FALSE)
  )
}
save(average_convergence, file = "output/average_convergence_past.Rda")

if(plot_final){
  library(ggplot2)
  plot_data <- average_convergence
  #plot_data$set <- factor(plot_data$set, levels = gtools::mixedsort(unique(plot_data$set)))
  # n.edg <- round(parameters[5]*100) + 100 # integer in (100, 200)
  plot_data$set <- factor(plot_data$set, levels = c("0.05, 0.05", "0.05, 0.95", "0.95, 0.05", "0.95, 0.95"), 
         labels = c("n.edge = 105, ideo = 0.05", "n.edge = 105, ideo = 0.95", "n.edge = 195, ideo = 0.05", "n.edge = 195, ideo = 0.95"))
  ggplot(data=plot_data, aes(x= trading_seq, y=convergence, color = true_mod)) +
    geom_point() + geom_smooth() +
    ggplot2::facet_wrap(~set, ncol = 1) +
    ggtitle("Convergence Over Trading Sequences") +
    xlab("Trading Sequences") + ylab(paste0("Trader Model Convergence (n = ", sample_count, ")")) + 
    theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0)) + 
    scale_color_discrete(name="True Model")
}