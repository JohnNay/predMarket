
create_sample <- function(input_values, input_names, sample_count) {
  library(lhs)
  # will create values from 0 to 1 and must be transformed afterwards, if need be.
  
  # create a random sample of input factor sets with Latin Hypercube Sampling
  lhs_design <- lhs::improvedLHS(sample_count, length(input_values))
  
  # transform the standardized random values to the real input value range (if need be)
  # and apply the desired random distribution
  lhs_design <- lapply(seq(1,length(input_values)), function(i) {
    input_values[[i]]$ARGS$p <- as.vector(lhs_design[ ,i])
    do.call(input_values[[i]]$random_function, input_values[[i]]$ARGS) # input_values[[i]]$min, input_values[[i]]$max
  })
  names(lhs_design) <- input_names
  data.frame(lhs_design)
}

keep_satisfied <- function(sampled){
  # TODO: add any constraints
  constraints <- sampled
  data.frame(sampled[constraints, , drop=FALSE])
}

create_set <- function(input_values, input_names, sample_count){
  input.sets <- create_sample(input_values, input_names, sample_count)
  input.sets <- keep_satisfied(input.sets)
  while(nrow(input.sets) < sample_count) { 
    # Create input factor sets by latin hypercube sampling:
    input.sets <- rbind(input.sets,
                        create_sample(input_values, input_names, sample_count))  
    # Discard input factor sets that violate constraints:
    input.sets <- keep_satisfied(input.sets)
  }
  input.sets
}

sobol_sa <- function(abm, input_values,
                     out = c("segreg"), 
                     sample_count = 4000, 
                     sobol_nboot = 1000, 
                     iterations = NULL,
                     parallel = TRUE){
  out <- match.arg(out)
  
  if(out == "segreg") output_names <- "Segregation"
  
  # Get names of input factors:
  input_names <- names(input_values)
  
  # Create two samples, removing samples violating constraints, until you have enough:
  input.sets.1 <- create_set(input_values, input_names, sample_count)
  input.sets.2 <- create_set(input_values, input_names, sample_count)
  
  # Make sets the same size:
  rows <- min(nrow(input.sets.1), nrow(input.sets.2))
  input.sets.1  <- input.sets.1[seq(rows), ]
  input.sets.2  <- input.sets.2[seq(rows), ]
  stopifnot(nrow(input.sets.1) == nrow(input.sets.2) & nrow(input.sets.2) > 0)
  
  ##################################################
  # Simulation runs with generated input factor sets:
  library(sensitivity)
  # Create instance of sobol class:
  sobol_aggregate <- sensitivity::sobol2007(model = NULL, 
                                            X1 = input.sets.1, X2 = input.sets.2, 
                                            nboot = sobol_nboot)
  
  # simulation results for input factor sets (as matrix)
  if (parallel) {
    library(doParallel) # require(foreach) not needed bc do... loads it
    doParallel::registerDoParallel(cores = parallel::detectCores())
  } # without registering the backend the %dopar% should just run as %do%
  if (missing(iterations)){
    sobol_sim <- foreach(i=seq(nrow(sobol_aggregate$X)), .combine='c') %dopar% {
      abm(as.numeric(sobol_aggregate$X[i, ]), out = out) # out = c("avg", "firstlast")
    }
  } else {
    sobol_sim <- foreach(i=seq(nrow(sobol_aggregate$X)), .combine='c') %dopar% {
      abm(as.numeric(sobol_aggregate$X[i, ]), out = out, iterations = iterations)
    }
  }
  # hist(sobol_sim) # same as hist(so2007$y) after tell(so2007, sim.results.sobol2007)
  #max(so2007$X[sim.results.sobol2007=='NaN', 1]) # 0.052
  library(sensitivity)
  # add simulation results (as vector) to sobol object
  sensitivity::tell(sobol_aggregate, sobol_sim)
  sobol_aggregate
}

correct_bias <- function(x) {
  x_corr <- x[["original"]] - x[["bias"]]
  min_ci <- x[["min. c.i."]]
  max_ci <- x[["max. c.i."]]
  x <- cbind(x_corr = x_corr, min_ci = min_ci, max_ci = max_ci, x)
  x[x < 0] <- 0
  x[x > 1] <- 1
  cbind(var = row.names(x), x)
}

plot_sobol_fo <- function(x, outcome_var = "Outcome"){
  ss <- correct_bias(x$S)
  ggplot(ss, aes(x = var, y = x_corr)) + geom_point() +
    xlab("Variable") + ylab("Estimated Effect") +
    ggtitle(paste("First Order Effects of Variables on", outcome_var)) +
    geom_errorbar(aes(ymax = max_ci, ymin = min_ci), width=0.25) +
    ylim(c(0,1))
}

plot_sobol_total <- function(x, outcome_var = "Outcome"){
  tt <- correct_bias(x$T)
  ggplot(tt, aes(x = var, y = x_corr)) + geom_point() + 
    xlab("Variable") + ylab("Estimated Effect") +
    ggtitle(paste("Total Sensitivity Effects of Variables on", outcome_var)) +
    geom_errorbar(aes(ymax = max_ci, ymin = min_ci), width=0.25) + 
    ylim(c(0,1))
}

