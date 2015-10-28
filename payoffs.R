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
  
  from <- 1
  to <- (n.secu - 1)
  
  
  #
  # TODO: Check that indexing is correct. Check for edge conditions and off-by-one. 
  #
  for (j in from:to){
    if( g$secu.inter[j] <= anom.star & anom.star < g$secu.inter[j + 1])
      for (i in 1:n.traders){
        V(g)$money[i] <- V(g)$money[i] + V(g)$secu[[i]][j]
      }
  } 
  
  #######
  ## Return the network
  #######
  
  g
  
}