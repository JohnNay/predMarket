#
# test_main.R Test the main function
#
rm(list = ls())
options(warn = 2)
source('main.R')

SHOW_CLIMATE_PLOTS <- TRUE
TRACE_CLIMATE_MODEL <- FALSE
STAN_REFRESH <-  0
PARALLEL_STAN <- FALSE
WHICH_MODEL <- 'ar1'

max_p <- 1
max_q <- 0

debug <- FALSE

if(!debug){
  future <- TRUE
  adaptation <- TRUE
  full_history <- FALSE
  
  if (future) {
    burn.in <- 135
    n.seq <- 14
    horizon <- 6
  } else {
    burn.in = 51
    n.seq = 14
    horizon = 6
    #nyears = burn.in + n.seq * horizon, #135,
  }
  
  ptm0 <- proc.time()
  x <- main(parameters = c(runif(3, min = 0.0001, max = 0.9999), # seg, ideo, risk.tak
                           0, # true model: 0 = slow.tsi, 1 = log.co2
                           runif(2, min = 0.0001, max = 0.9999)), # n.edge and n.traders
            out = "fraction_converge",
            safeNprint=TRUE,
            iterations = 1,
            burn.in = burn.in,
            n.seq = n.seq,
            horizon = horizon,
            nyears = burn.in + n.seq * horizon,
            record = TRUE,
            full_history = full_history,
            adaptation = adaptation,
            # saving and loading climate data
            load_previous = TRUE,
            saving = TRUE
  )
  ptm1 <- proc.time()
  cat("First process finished in", paste_with_names(prettyNum(head(ptm1 - ptm0,3), big.mark=',')), 'seconds')
  
  y <- main(parameters = c(runif(3, min = 0.0001, max = 0.9999), # seg, ideo, risk.tak
                           1, # true model: 0 = slow.tsi, 1 = log.co2
                           runif(2, min = 0.0001, max = 0.9999)), # n.edge and n.traders
            out = "fraction_converge",
            safeNprint=TRUE,
            iterations = 1,
            burn.in = burn.in,
            n.seq = n.seq,
            horizon = horizon,
            nyears = burn.in + n.seq * horizon,
            record = TRUE,
            full_history = full_history,
            adaptation = adaptation,
            # saving and loading climate data
            load_previous = TRUE,
            saving = TRUE
  )
  ptm2 <- proc.time()
  cat("Second process finished in", paste_with_names(prettyNum(head(ptm2 - ptm1,3), big.mark=',')), 'seconds')
} else{
  ##############################################################################
  ## Debug
  ##############################################################################
  
  future <- TRUE
  adaptation <- FALSE
  load_previous <- TRUE
  saving <- TRUE
  full_history <- TRUE
  
  if (future) {
    burn.in <- 135
    n.seq <- 14
    horizon <- 6
  } else {
    burn.in = 51
    n.seq = 14
    horizon = 6
    #nyears = burn.in + n.seq * horizon, #135,
  }
  
  ptm0 <- proc.time()
  
  iteration <- 1
  
  id <- c()
  true.model <- c()
  mean.true <- c()
  mean.false <- c()
  
  
  for (iteration in seq(iteration)){
    
    x <- main(parameters = c(runif(3, min = 0.0001, max = 0.9999), # seg, ideo, risk.tak
                             1, # true model: 0 = slow.tsi, 1 = log.co2
                             runif(2, min = 0.0001, max = 0.9999)), # n.edge and n.traders
              out = "fraction_converge",
              safeNprint=FALSE,
              iterations = 1,
              burn.in = burn.in,
              n.seq = n.seq,
              horizon = horizon,
              nyears = burn.in + n.seq * horizon,
              record = TRUE,
              full_history = full_history,
              adaptation = adaptation,
              # saving and loading climate data
              load_previous = load_previous,
              saving = saving,
              perfect = TRUE
    )
    
    id <- append(id,iteration)
    true.model <- append(true.model, x[[length(x)]]$true.model)
    mean.true <- append(mean.true, mean(V(x[[length(x)]])$money[V(x[[length(x)]])$approx == x[[length(x)]]$true.model]))
    mean.false <- append(mean.false, mean(V(x[[length(x)]])$money[V(x[[length(x)]])$approx != x[[length(x)]]$true.model]))
    
    x <- main(parameters = c(runif(3, min = 0.0001, max = 0.9999), # seg, ideo, risk.tak
                             0, # true model: 0 = slow.tsi, 1 = log.co2
                             runif(2, min = 0.0001, max = 0.9999)), # n.edge and n.traders
              out = "fraction_converge",
              safeNprint=FALSE,
              iterations = 1,
              burn.in = burn.in,
              n.seq = n.seq,
              horizon = horizon,
              nyears = burn.in + n.seq * horizon,
              record = TRUE,
              full_history = full_history,
              adaptation = adaptation,
              # saving and loading climate data
              load_previous = load_previous,
              saving = saving,
              perfect = TRUE
    )
    
    id <- append(id,iteration)
    true.model <- append(true.model, x[[length(x)]]$true.model)
    mean.true <- append(mean.true, mean(V(x[[length(x)]])$money[V(x[[length(x)]])$approx == x[[length(x)]]$true.model]))
    mean.false <- append(mean.false, mean(V(x[[length(x)]])$money[V(x[[length(x)]])$approx != x[[length(x)]]$true.model]))
    
  }
  
  money.data <- data.frame(id,true.model,mean.true, mean.false)

}
