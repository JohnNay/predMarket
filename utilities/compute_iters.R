

setClass(Class = "computeIters",
         slots = c(call = "language",
                   measure = "character",
                   result = "integer",
                   plot_data = "data.frame",
                   timing = "numeric",
                   session = "ANY")
)

setMethod("summary", "computeIters",
          function(object, digits = 3) {
            cat("\n\nThis process took", object@timing, "seconds (", 
                object@timing/60/60, "hours).")
            cat("Average number of iterations found to be sufficient is", object@result)
            invisible(object)
          }
)

setMethod("print", "computeIters",
          function(x, ...) str(x)
)


setMethod("show", "computeIters",
          function(object) {
            cat("An object of class \"computeIters\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          }
)


setMethod("plot", "computeIters",
          function(x, outcome_var = "Outcome", 
                   xlab = "Iterations", ylab = NULL){
            
            measure <- x@measure
            
            if (missing(ylab)) {
              ylab <- measure
            } else {
              measure <- ylab
            }
            
            x <- x@plot_data
            
            ggplot2::ggplot(x, 
                            ggplot2::aes(x = iters, y = measured)) + 
              ggplot2::geom_point() +
              ggplot2::geom_smooth(method = "loess") +
              ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
              ggplot2::ggtitle(paste("Iterations and", measure, "for", outcome_var)) +
              ggplot2::theme_bw()
          }
)


# Coefficient of variation
# @param na.rm Logical vector length one indicating whether 
# NA values should be stripped before the computation proceeds
coef_var <- function(x, na.rm = FALSE){
  sd(x, na.rm = na.rm)/
    mean(x, na.rm = na.rm)
}

compute_iters <- function(abm, 
                          input_values,
                          out, 
                          sample_count = 20,
                          repeats = 30,
                          thresh = 0.05,
                          initial_iters = 5,
                          max_iters = 100,
                          constraints = "none",
                          parallel = FALSE,
                          cores = NULL,
                          verbose = FALSE,
                          measure = c("coef_var", "var", "sd")){
  
  # Preparing: ###
  measure <- match.arg(measure)
  start_time <- as.numeric(proc.time()[[3]])
  call <- match.call()
  
  # Get names of input factors:
  input_names <- names(input_values)
  
  if(parallel & missing(cores)) cores <- parallel::detectCores() - 1
  
  # Main outer loop: ###
  res <- foreach::`%do%`(foreach::foreach(i=seq(repeats), .combine='rbind', .verbose=FALSE), {
    iters <- initial_iters
    measured <- Inf
    
    repeat{
      # Create sample, removing samples violating constraints, until you have one:
      input.set <- create_set(input_values, input_names, 1, constraints)
      if(verbose) cat("Done with input set creation.\n")
      
      if (parallel) {
        doParallel::registerDoParallel(cores = cores)
      } # without registering the backend the %dopar% should just run sequentially as %do%
      output <- foreach::`%dopar%`(foreach::foreach(i = seq(sample_count), .combine='c'), {
        abm(as.numeric(input.set), out = out, iterations = iters)
      })
      if(verbose) cat("Done with simulations.\n")
      
      if (measure == "coef_var"){
        measured2 <- coef_var(output)
      }
      if (measure == "var"){
        measured2 <- var(output)
      }
      if (measure == "sd"){
        measured2 <- sd(output)
      }
      
      if (iters >= max_iters || (measured - measured2) <= thresh){
        break
      } else {
        measured <- measured2
        iters <- iters + 1
      }
    }
    
    c(iters, measured)
  })
  
  if (dim(res)[1] > 1){
    res_int <- round(mean(res[ ,1]))
    plot_data <- data.frame(iters = res[ ,1], measured = res[ ,2])
  } else {
    res_int <- round(mean(res[1]))
    plot_data <- data.frame(iters = res[1], measured = res[2])
  }
  
  new("computeIters",
      call = call,
      measure = measure,
      result = as.integer(res_int),
      plot_data = plot_data,
      timing = as.numeric(proc.time()[[3]]) - start_time,
      session = sessionInfo())
}

