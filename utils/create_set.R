# this code is from Nay's R package for data-driven modeling that is a work in progress
# I moved this source code directly into this prediction_market repo so everything
# needed to reproduce the prediction_market results is contained here in this repo.

#'Create set of samples by sampling with LHS and then checking constraints.
#'
#'\code{create_set} creates sample that stay within constraints.
#'
#'@param input_values List
#'@param input_names Character vector
#'@param sample_count Numeric vector length one.
#'@param constraints Character vector that is either "none" of is using only 
#'  variable names that are specified in the input_values List argument. This 
#'  character vector is evaluated in an environment created for the sampled data
#'  on the variables, and its evaluation results in a Logical vector that that 
#'  subsets sampled.
#'  
#'@return Returns a \code{data.frame} of samples.
#'  
#'@export
create_set <- function(input_values, input_names, sample_count, constraints){
  
  input.sets <- create_sample(input_values, input_names, sample_count)
  
  if(constraints == "none") {
    constrained <- rep(TRUE, nrow(input.sets))
  } else {
    constrained <- with(input.sets, eval(parse(text=constraints)))
  }
  input.sets <- keep_satisfied(input.sets, constrained)
  
  while(nrow(input.sets) < sample_count) { 
    # Create input factor sets by latin hypercube sampling:
    input.sets <- rbind(input.sets,
                        create_sample(input_values, input_names, sample_count))  
    # Discard input factor sets that violate constraints:
    constrained <- with(input.sets, eval(parse(text=constraints)))
    input.sets <- keep_satisfied(input.sets, constrained)
  }
  
  input.sets
}

################################################################################
#'@describeIn create_set Create a sample.
#'  
#'@return Returns a data.frame of samples.
#'  
#'@references B. Beachkofski, R. Grandhi, in 43rd AIAA/ASME/ASCE/AHS/ASC
#'Structures, Structural Dynamics, and Materials Conference (American Institute
#'of Aeronautics; Astronautics, 2002;
#'http://arc.aiaa.org/doi/abs/10.2514/6.2002-1274).
#'
#'R. Carnell, Lhs Latin Hypercube Samples (2012), (available at
#'http://cran.r-project.org/web/packages/lhs/index.html).
#'
#'@export

create_sample <- function(input_values, input_names, sample_count) {
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

################################################################################
#'@describeIn create_set Stay within constraints.
#'  
#'@param sampled Output of create sample_sample
#'  
#'@return Returns a data.frame of samples thats the same or less rows as input.
#'  
#'@examples
#'fake_constraints <- "param1 < 0.5 & param2 > 0.5"
#'fake_data <- data.frame(param1 = runif(100), param2 = runif(100))
#'fake_constraints <- with(fake_data, eval(parse(text=fake_constraints)))
#'keep_satisfied(fake_data, fake_constraints)
#'
#'@export

keep_satisfied <- function(sampled, constraints){
  result <- data.frame(sampled[constraints, , drop=FALSE])
  stopifnot(nrow(result) <= nrow(sampled))
  result
}

