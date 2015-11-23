args <- commandArgs(trailingOnly = TRUE)
numcores <- as.integer(args[1])
message(paste("Number of cores", numcores))

source("main.R")
library(eat)

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
average_convergence <- data.frame(stringsAsFactors = FALSE)
n.seq <- 14

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
    main(parameters = as.numeric(input_set[i, ]), 
         burn.in = 51,
         n.seq = n.seq,
         horizon = 6,
         nyears = 135,
         iterations = 1,
         out = "converg", record = TRUE)
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
  outcome.evolution <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input_set)), .combine='rbind'), {
    main(parameters = as.numeric(input_set[i, ]), 
         burn.in = 51,
         n.seq = n.seq,
         horizon = 6,
         nyears = 135,
         iterations = 1,
         out = "converg", record = TRUE)
  })
  cat(paste("/n", n.seq, "outcome evolutions with parameters", paste(as.character(j), collapse = ", "), "/n", outcome.evolution))
  average_convergence <- rbind(average_convergence,
                               data.frame(convergence = outcome.evolution, 
                                          trading_seq = rep(seq(n.seq), nrow(input_set)),
                                          true_mod = rep(rep("SlowTSI", length(seq(n.seq)), nrow(input_set))),
                                          set = rep(rep(paste(as.character(j), collapse = ", "),  length(seq(n.seq))), nrow(input_set)),
                                          stringsAsFactors = FALSE)
  )
#   average_convergence <- rbind(average_convergence,
#                                data.frame(avg = colMeans(outcome.evolution), 
#                                           trading_seq = seq(length(colMeans(outcome.evolution))),
#                                           true_mod = rep("SlowTSI", length(colMeans(outcome.evolution))),
#                                           set = rep(paste(as.character(j), collapse = ", "), length(colMeans(outcome.evolution)))))
}
save(average_convergence, file = "output/average_convergence.Rda")

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
