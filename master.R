# master script for conducting ABM experiments
rm(list=ls()) # make sure to always run this line of code and see that the next two 
# lines of code work without error, ensuring that the working of the model is not 
# dependent on anything in your global workspace, if it is, then you need to create 
# whatever is in your global workpace in the code

args <- commandArgs(trailingOnly = TRUE)
cores <- as.integer(args[1])
if (length(args) < 2) {
  plotting = FALSE
} else {
  plotting <- as.logical(args[2])
}
message(paste("Number of cores", cores))

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
                               ARGS = list(min = 0.0001, max = 0.9999))

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
## Main sens analysis # Standardized Regression Coefficient
##############################################################################

future <- TRUE
past <- FALSE
sample_count <- 100

if(past){
  main2 <- function(parameters,
                    iterations = 10,
                    burn.in = 51,
                    n.seq = 14,
                    horizon = 6,
                    out){
    main(parameters = parameters,
         out = out,
         iterations = iterations,
         burn.in = burn.in,
         n.seq = n.seq,
         horizon = horizon,
         nyears = burn.in + n.seq * horizon)
  }
  
  if(file.exists("output/src.Rda")){
    load("output/src.Rda")
    src <- pc_sa(abm = main2, 
                 input_values = input_values,
                 previous_pc_sa = list(src),
                 out = "converge", 
                 iterations = 5,
                 sample_count = sample_count,
                 nboot = 1000, 
                 parallel = TRUE,
                 cores = cores,
                 rank = TRUE, method = "src")
  } else{
    src <- pc_sa(abm = main2, 
                 input_values = input_values,
                 out = "converge", 
                 iterations = 5,
                 sample_count = sample_count,
                 nboot = 1000, 
                 parallel = TRUE,
                 cores = cores,
                 rank = TRUE, method = "src") 
  }
  save(src, file = "output/src.Rda")
  if (plotting){
    pdf("output/sa_past.pdf", width=10, height=10)
    plot(src, outcome_var = paste0("Convergence of Beliefs in Past Scenario \n (R^2= ", 
                                   round(src@r_squared, 2), " , n=",  length(src@sims),")"))
    dev.off()
  }
}

if(future){
  main2 <- function(parameters,
                    iterations = 10,
                    burn.in = 135,
                    n.seq = 14,
                    horizon = 6,
                    out){
    main(parameters = parameters,
         iterations = iterations,
         out = out,
         burn.in = burn.in,
         n.seq = n.seq,
         horizon = horizon,
         nyears = burn.in + n.seq * horizon)
  }
  
  if(file.exists("output/src_future.Rda")){
    load("output/src_future.Rda")
    src_future <- pc_sa(abm = main2, 
                        input_values = input_values,
                        previous_pc_sa = list(src_future),
                        out = "converge", 
                        iterations = 5,
                        sample_count = sample_count,
                        nboot = 1000, 
                        parallel = TRUE,
                        cores = cores,
                        rank = TRUE, method = "src")
  } else {
    src_future <- pc_sa(abm = main2, 
                        input_values = input_values,
                        out = "converge", 
                        iterations = 5,
                        sample_count = sample_count,
                        nboot = 1000, 
                        parallel = TRUE,
                        cores = cores,
                        rank = TRUE, method = "src")
  }
  save(src_future, file = "output/src_future.Rda")
  if (plotting){
    pdf("output/sa_future.pdf", width=10, height=10)
    plot(src_future, outcome_var = paste0("Convergence of Beliefs in Future Scenario \n (R^2= ", 
                                          round(src_future@r_squared, 2), " , n=",  
                                          length(src_future@sims),")"))
    dev.off()
  }
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