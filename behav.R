 
Behav <- function (
                    g     = net,
                    data  = D
                  )
{

  # NOTE : In the preliminary model, there are only ZI agents, so that everyone
  # decides of buy and sell orders according to the same procedure.
  
  # package requirement
  require(igraph)
  
  ######
  ## Useful variables
  ######
  
  n.secu    <- length((V(g)$secu)[[1]])
  n.traders <- length(V(g))
  n.approx  <- length(unique(V(g)$approx))
  
  ######
  ### Determine bounds for securities
  ######
  
  min.trad <- 1.0001 * min(D[,"T"])  # every temperature below 1.1 * min.temp gets  
                                # included in a single security
  max.trad <- 0.9999 * max(D[,"T"])  # every temperature above 0.9 * max.temp gets  
                                # included in a single security
  
  #########
  ### Evaluate reservation price for securities as a function of approximate model
  #########
  
  reserv <- matrix(1,nrow = n.secu, ncol = n.approx) # an empty matrix to store
  # reservation prices for every security and every approximate model
  n.rand <- dim(g$pred)[1]  # The number of values in the empirical distributions
  
  ### Store expected values for "Boundary" securities 
  
  # Lower security
  
  for (k in 1:n.approx){
    reserv[1,k] <- length(g$pred[,k][g$pred[,k] < min.trad])/n.rand
  }
  # For a given approximate model k , the expected value of the lowest
  # security is simply the number of observations in the empirical approximate distribution
  # which are lower than min.trad, divided by the number of observation in the
  # empirical approximate distribution n.rand
  
  # Upper security
  
  for (k in 1:n.approx){
    reserv[n.secu,k] <- length(g$pred[,k][g$pred[,k] >= max.trad])/n.rand
  }
  
  #### Store expected values for "non-Boundary" securities 
  
  from <- 2
  to <- (n.secu - 1)
  
  # Determine size of intervals corresponding to the intermediate securitis
  
  inter <- (max.trad - min.trad)/(to - 1)
  
  # Store expected value for each interval
  
  for (j in from:to){
    for (k in 1:n.approx){
      reserv[j,k] <- length( g$pred[,k] [min.trad + (j-2)*inter < g$pred[,k] 
                                         & g$pred[,k] < min.trad + (j-1)*inter] )/n.rand
    }
  } 
  
  # Add row "n.secu+1" containing NA's for agent who sell nothing
  
  reserv <- rbind(reserv,rep(NA,n.approx))
  
  #######
  ## Traders pick a security at random that will try to buy once on the market
  #######
  
  V(g)$buy.which <- sample(n.secu, n.traders, replace = TRUE)
  
  #######
  ## Traders set a price to place a buy order (must be lower than traders's money stock)
  #######
  
  V(g)$buy.price <- pmin(
    reserv[cbind(V(g)$buy.which,V(g)$approx)]*(rep(1,n.traders) + V(g)$risk.tak),
    V(g)$money)
    

  #######
  ## Traders pick a security they have positive amount of at random to place sell offer 
  #######
  
  secu.mat <- do.call(rbind, V(g)$secu)
  
  # draw at random for each trader among the securities she has positive amount of
  
  sell <- rep (n.secu + 1,n.traders)  # the value n.secu +1 is chosen so that traders
  # who do not sell will be assigned a selling price of "NA" based on the reserv matrix,
  # see below
  
  
for (i in 1:n.traders)
{ 
  if (length(secu.mat[i,][secu.mat[i,] >= 1]) ==1) { 
    # if the traders own a positive amount of a single security
 sell[i] <- which(secu.mat[i,] >= 1) 
}
  if (length(secu.mat[i,][secu.mat[i,] >= 1]) >1) { 
    # if the traders own a positive amount of some security
    sell[i] <- sample(which(secu.mat[i,] >= 1),1) 
  }
}
  
  V(g)$sell.which <- sell
  
  #######
  ## Traders set a price to place a buy order
  #######
  
  V(g)$sell.price <- reserv[cbind(V(g)$sell.which,V(g)$approx)]*(rep(1,n.traders) - V(g)$risk.tak)
  
return(g)

}