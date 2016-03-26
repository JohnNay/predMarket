# this code is from Nay's R package for data-driven modeling that is a work in progress
# I moved this source code directly into this prediction_market repo so everything
# needed to reproduce the prediction_market results is contained here in this repo.

setClassUnion("numericOrchar", members = c("numeric", "character"))

################################################################################
#' An S4 class to return the results of sensitivity analyses
#' 
#' @slot call Language from the call of the function \code{\link{cv_abm}}.
#' @slot result c("src", "pcc") s3 classes from \code{sensitivity} package.
#' @slot r_squared Numeric vector length one.
#' @slot timing Numeric vector length one with the total elapsed time it took 
#'   \code{\link{cv_abm}} to execute.
#' @slot session the results from calling \code{sessionInfo()} at end of
#'   \code{\link{pc_sa}} function.
#'   
#' @export

setClass(Class = "pcSA",
         slots = c(call = "language",
                   result = "ANY", # "pcsens"
                   r_squared = "numericOrchar",
                   timing = "numeric",
                   session = "ANY")
)

################################################################################
#' @describeIn pcSA An S4 method for printing a pcSA S4 object
#' @param ... ignored
#'  @export

setMethod("print", "pcSA",
          function(x, ...) str(x)
)

################################################################################
#' @describeIn pcSA An S4 method for showing a pcSA S4 object
#' @param object S4 pcSA object
#' 
#'  @export

setMethod("show", "pcSA",
          function(object) {
            cat("An object of class \"pcSA\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          }
)


correct_bias <- function(x) {
  x_corr <- x[["original"]] - x[["bias"]]
  min_ci <- x[["min. c.i."]]
  max_ci <- x[["max. c.i."]]
  x <- cbind(x_corr = x_corr, min_ci = min_ci, max_ci = max_ci, x)
  cbind(var = row.names(x), x)
}
################################################################################
#'Plots pcSA S4 object, a Partial Correlation Analysis of a Simulation Model
#'@describeIn pcSA
#'  
#'  This is function of the \strong{eat} package. \code{pc_sa} conducts a a 
#'  partial correlation analysis.
#'  
#'@param x The result slot of an object created by \code{pc_sa}.
#'@param outcome_var Optional character vector for labeling the outcome variable
#'  in the plot. Default is "Outcome".
#'@param xlab Optional character vector for labeling the variables.
#'@param ylab Optional character vector. Default is "Partial Rank Correlation
#'  Coefficient". Could also be "Partial Correlation Coefficient", "Standardized
#'  Rank Regression Coefficient", and "Standardized Regression Coefficient".
#'  
#'@return Returns a ggplot2 plot.
#'  
#' @examples
#' fake_abm <- function(params, out) {
#'   x1 <- params[1]
#'   x2 <- params[2]
#'   if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
#'   if (out=="ident") return(x1 + x2 + rnorm(1, 0))
#' }
#' inputs <- lapply(list(param1 = NA, param2 = NA), 
#'                  function(x) list(random_function = "qunif",
#'                                   ARGS = list(min = 0, max = 1)))
#' s <- pc_sa(fake_abm, inputs, "sq")
#' plot(s)
#' 
#'@references J. C. Thiele, W. Kurth, V. Grimm, Facilitating Parameter 
#'  Estimation and Sensitivity Analysis of Agent-Based Models: A Cookbook Using 
#'  NetLogo and R. Journal of Artificial Societies and Social Simulation. 17, 11
#'  (2014).
#'  
#'@export

setMethod("plot", "pcSA",
          function(x, outcome_var = "Outcome", 
                   xlab = "Variable", ylab = "Partial rank correlation coefficient"){
            x <- x@result
            ss <- correct_bias(x[[7]]) # $SRRC, $SRC, $PRCC, $PCC
            
            ggplot2::ggplot(ss, ggplot2::aes(x = var, y = x_corr)) + ggplot2::geom_point() +
              ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
              ggplot2::ggtitle(paste("Estimated effects on", outcome_var)) +
              ggplot2::geom_errorbar(ggplot2::aes(ymax = max_ci, ymin = min_ci), width=0.25) +
              ggplot2::geom_hline(yintercept = 0) +
              ggplot2::ylim(c(-1,1)) +
              ggplot2::theme_bw()
          }
)

# Calculation of R2 for the original data: this function is from:
# J. C. Thiele, W. Kurth, V. Grimm, Facilitating Parameter Estimation and Sensitivity Analysis of Agent-Based Models: 
# A Cookbook Using NetLogo and R. Journal of Artificial Societies and Social Simulation. 17, 11 (2014).
get_rsquare <- function(x, y, on.ranks) {
  data <- data.frame(Y = y, x)
  if (on.ranks) {
    for (i in 1:ncol(data)) {
      data[,i] <- rank(data[,i])
    }
  }
  i = 1:nrow(data)
  d <- data[i, ]
  lm.Y <- stats::lm(formula(paste(colnames(d)[1], "~", paste(colnames(d)[-1], collapse = "+"))), data = d)
  summary(lm.Y)$r.squared
}

#'Partial Correlation Analysis of a Simulation Model
#'
#'\code{pc_sa} conducts a partial correlation analysis.
#'
#'This is function of the \strong{eat} package. It takes an abm in function form
#'and a list of input values. Helper function for extracting R-sqaured is from 
#'Thiele et al. (2014).
#'
#'@param abm A function that takes as input values for each of the 
#'  \code{input_values}
#'@param input_values List
#'@param out Character vector length one to be passed an argument to the 
#'  \code{abm} function to specify what outcome measure to use.
#'@param sample_count  Optional Numeric vector length one. Default is 100.
#'@param constraints Optional Character vector that is either "none" or is using
#'  only variable names that are specified in the input_values List argument. 
#'  This character vector is evaluated in an environment created for the sampled
#'  data on the variables, and its evaluation results in a Logical vector that 
#'  that subsets sampled.
#'@param nboot Optional Numeric vector length one. Default is 1000.
#'@param iterations Optional numeric vector length one.
#'@param parallel Optional logical vector length one. Default is FALSE.
#'@param cores Optional Numeric vector length one. Default is 
#'  parallel::detectCores().
#'@param verbose Optional logical vector.
#'@param rank Optional logical vector.
#'@param method Optional character vector that is either "src" (Standardized 
#'  Regression Coefficient) or "pcc" (Partial Correlation Coefficient)
#'  
#'@return Returns a sensitivity object that can be plotted by functions.
#'  
#' @examples
#' # Unconstrained Analysis
#' fake_abm <- function(params, out) {
#'   x1 <- params[1]
#'   x2 <- params[2]
#'   if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
#'   if (out=="ident") return(x1 + x2 + rnorm(1, 0))
#' }
#' inputs <- lapply(list(param1 = NA, param2 = NA), 
#'                  function(x) list(random_function = "qunif",
#'                                   ARGS = list(min = 0, max = 1)))
#' pc_sa(fake_abm, inputs, "sq")
#' pc_sa(fake_abm, inputs, "sq", method = "pcc", rank = FALSE)
#' pc_sa(fake_abm, inputs, "sq", method = "src", rank = FALSE)
#' 
#' # Constrained Analysis
#' fake_abm <- function(params, out) {
#'   x1 <- params[1]
#'   x2 <- params[2]
#'   if (out=="sq") return(x1^2 + x2 + rnorm(1, 0))
#'   if (out=="ident") return(x1 + x2 + rnorm(1, 0))
#' }
#' inputs <- lapply(list(param1 = NA, param2 = NA), 
#'                  function(x) list(random_function = "qunif",
#'                                   ARGS = list(min = 0, max = 1)))
#' pc_sa(fake_abm, inputs, "sq", constraints = "param1 > 0.1 & param2 < 0.9")
#' 
#'@references G. Pujol et al., Sensitivity: Sensitivity Analysis (2014), 
#'  (available at
#'  http://cran.r-project.org/web/packages/sensitivity/index.html).
#'  
#'  A. Saltelli, K. Chan, E. M. Scott, Sensitivity Analysis (Wiley, Chichester, 
#'  2009).
#'  
#'  J. C. Thiele, W. Kurth, V. Grimm, Facilitating Parameter Estimation and 
#'  Sensitivity Analysis of Agent-Based Models: A Cookbook Using NetLogo and R. 
#'  Journal of Artificial Societies and Social Simulation. 17, 11 (2014).
#'  
#'@export

pc_sa <- function(abm, 
                  input_values,
                  out, 
                  sample_count = 100,
                  constraints = "none",
                  nboot = 1000, 
                  iterations = NULL,
                  parallel = FALSE,
                  cores = NULL,
                  verbose = TRUE,
                  rank = TRUE,
                  method = c("src", "pcc")){
  
  method <- match.arg(method)
  start_time <- as.numeric(proc.time()[[1]])
  call <- match.call()
  
  # Get names of input factors:
  input_names <- names(input_values)
  
  if(parallel & missing(cores)) cores <- parallel::detectCores() - 1
  
  # Create two samples, removing samples violating constraints, until you have enough:
  input.set <- create_set(input_values, input_names, sample_count, constraints)
  if(verbose) cat("Done with input set creation \n")
  
  ##################################################
  # Simulation runs with generated input factor sets:
  if(verbose) cat("Starting simulations \n")
  # simulation results for input factor sets (as matrix)
  if (parallel) {
    doParallel::registerDoParallel(cores = cores)
  } # without registering the backend the %dopar% should just run sequentially as %do%
  if (missing(iterations)){
    pc_sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input.set)), .combine='c'), {
      abm(as.numeric(input.set[i, ]), out = out)
    })
  } else {
    pc_sim <- foreach::`%dopar%`(foreach::foreach(i=seq(nrow(input.set)), .combine='c'), {
      abm(as.numeric(input.set[i, ]), out = out, iterations = iterations)
    })
  }
  if(verbose) cat("Done with simulations \n")
  
  if (method == "src"){
    result <- sensitivity::src(X = input.set, y = pc_sim, nboot = nboot, rank = rank)
    r_squared <- get_rsquare(x = input.set, y = pc_sim, 
                             on.ranks = rank)
  }
  
  if (method == "pcc"){
    result <- sensitivity::pcc(X = input.set, y = pc_sim, nboot = nboot, rank = rank)
    r_squared <- "Not relevant to this method. Only relevant to the 'src' method."
  }
  
  new("pcSA",
      call = call,
      result = result, 
      r_squared = r_squared,
      timing = as.numeric(proc.time()[[1]]) - start_time,
      session = sessionInfo())
}
