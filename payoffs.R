Payoffs <- function ( g,
                      ct # current time
){
  require(igraph)
  
  ######
  ## Useful variables
  ######
  
  n.secu    <- length((V(g)$secu)[[1]])
  n.traders <- length(V(g))
  n.approx  <- length(unique(V(g)$approx))
  
  ######
  ## Actual temperature
  ######
  
  time      <- g$burn.in + g$horizon
  anom.star <- g$t.anom[ct,]
  min.trad  <- min(g$secu.inter)
  max.trad  <- max(g$secu.inter)

  
  
  ### Pay lower security if actual temperature is in lower security range
  
  if (anom.star < min.trad){
    for (i in 1:n.traders){
      V(g)$money[i] <- V(g)$money[i] + V(g)$secu[[i]][1]
    }
  }
  
  ### Pay upper security if actual temperature is in upper security range
  
  if (anom.star >= max.trad){
    for (i in 1:n.traders){
      V(g)$money[i] <- V(g)$money[i] + V(g)$secu[[i]][n.secu]
    }
  }
  
  #### Pay intermediate securities
  
  #
  # TODO: Check that indexing is correct. Check for edge conditions and off-by-one. 
  #
  which.payoff <- findInterval(anom.star, g$secu.inter) + 1
  if(length(g$secu.inter) != n.secu - 1) warning("Mismatch between n.secu and length of secu.inter")
  if (! which.payoff %in% 1:n.secu) warning("Index out of range in Payoffs")
  message("Payoff for ", ct, " = ", which.payoff, ": temperature = ", anom.star)
  ## Check for accuracy
  for (i in 1:n.secu) {
    if ((i == 1 || g$secu.inter[i-1] <= anom.star) && (i == n.secu || anom.star < g$secu.inter[i])) {
      if (which.payoff != i) warning("Internal inconsistency in Payoffs")
    }
  }
  
  # Make payoffs
  for (i in 1:n.traders) {
    V(g)$money[i] <- V(g)$money[i] + V(g)$secu[[i]][which.payoff]
  }

  #######
  ## Return the network
  #######
  
  g
  
}