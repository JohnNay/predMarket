Payoffs <- function ( g     = net,
                      data  = D
){
  # package requirement
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
  
  time <- g$burn.in + g$horizon
  temp.star <- D[time,"temp"]
  
  ######
  ### Determine bounds for securities
  ######
  
  min.trad <- 1.0001 * min(D[,"temp"])  # every temperature below 1.1 * min.temp gets  
  # included in a single security
  max.trad <- 0.9999 * max(D[,"temp"])  # every temperature above 0.9 * max.temp gets  
  # included in a single security
  
  
  # Pay lower security if actual temperature is in lower security range
  
  if (temp.star < min.trad){
    g <- set.graph.attribute(g,"winner","min")   # keep track of the winning security
    for (i in 1:n.traders){
      V(g)$money[i] <- V(g)$money[i] + V(g)$secu[[i]][1]
    }
  }
  
  # Pay upper security if actual temperature is in upper security range
  
  if (temp.star >= max.trad){
    g <- set.graph.attribute(g,"winner","max")   # keep track of the winning security
    for (i in 1:n.traders){
      V(g)$money[i] <- V(g)$money[i] + V(g)$secu[[i]][n.secu]
    }
  }
  
  #### Pay intermediate securities
  
  from <- 2
  to <- (n.secu - 1)
  
  # Determine size of intervals corresponding to the intermediate securitis
  
  inter <- (max.trad - min.trad)/(to - 1)
  
  # Store expected value for each interval
  
  for (j in from:to){
    g <- set.graph.attribute(g,"winner",j)   # keep track of the winning security
    if( min.trad + (j-2)*inter <= temp.star & temp.star < min.trad + (j-1)*inter)
      for (i in 1:n.traders){
        V(g)$money[i] <- V(g)$money[i] + V(g)$secu[[i]][j]
      }
  } 
  g
}