#' @title  Populate Prediction Market Agent-Based Model (ABM)
#'   
#' @description Adapts Adam Laiacano's comment on http://www.babelgraph.org/wp/?p=237's to populate
#'  a prediction market ABM
#'   
#' @param ntraders integer indicating the number of traders.
#' @param nsecu number of securities the agents can trade.
#'   
#' @return \code{populate} returns a dataframe, each line of which corresponds to the 
#'  data relevant to a single agent

populate <- function( 
                        ntraders   = 100,
                        nsecus      = 10,
                      ) 
  {  
  
  # define some factors
  levels.behav  <- factor(c('ZI')) 
  levels.approx <- factor(c('true','myth','ineff','policy','engineer','eng.and.pol'))  
  
#   # initialize vectors by ntraders 
#   p.id    <- numeric(ntraders) 
#   p.money <- numeric(ntraders) 
#   p.behav <- character(ntraders) 
#   p.approx <- character(ntraders) 
#   p.secu  <- numeric(ntraders)
# 
#   
#   for (cc in 1:ntraders) {
#     
#     # generate attribute for each person in cohort 
#     p.id[cc] <- cc 
#     p.money[cc] <- 100
#     p.behav[cc] <- as.vector(sample(levels.behav,size=1)) 
#     p.approx[cc] <- as.vector(sample(levels.approx,size=1))
#     p.secu[cc] <- rep(1,nsecu)
#   }
  
  # construct data frame from vectors 
  
#   df <- data.frame(
#                    id=1:ntraders, 
#                    money=rep(100,ntraders),
#                    behav=sample(levels.behav, size=ntraders, replace = TRUE), 
#                    approx = sample(levels.approx, size=ntraders, replace = TRUE),
#                    s = matrix (1, ntraders, nsecus)
#                   )   

df <- cbind(
  
  matrix(rbinom(25, 1, 0.5), nr = ntraders, nc = ntraders)   # initial social network
  rep(100,ntraders),                                         # initial amount of money
  
  
#   behav=sample(levels.behav, size=ntraders, replace = TRUE), 
#   approx = sample(levels.approx, size=ntraders, replace = TRUE),
#   s = matrix (1, ntraders, nsecus)
)   
  


  return(df)  } 

df <- populate(100)
