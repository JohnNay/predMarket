ShapeNet <- function(
                      g,
                      n.edg = vcount(net)*2,
                      seg  = 0.5
                    )
  {
  
  # package requirement
  require(igraph)
  
  # Generate links between agents as a function of whether they share the same approximate model
  
  # In the current procedure below "n.edg" are selected among all possible links between agents in the network
  # The probability that a link be selected depends on whether the agents share the same approximate model
  # In the currect procedure, the probability that a link be formed is
  #            - (1-seg)/(number of possible nodes), if the two traders have different approximate models
  #            - 1/(number of possible nodes)      , if the two traders have the same approximate model     
  
  c <- combn(V(g),2)
  samp <- matrix(1,dim(c)[1]+1,dim(c)[2])
  samp[1:2,] <- c
  diff <- get.vertex.attribute(g,"approx",c[1,]) - get.vertex.attribute(g,"approx",c[2,])
  homoph <- as.numeric(diff == 0)
  select <- sample(1:dim(c)[2], n.edg , prob = rep((1-seg),dim(c)[2]) + homoph*seg) 
  new.edg <- c[1:2,select]
  new.edg.igraph <- c(new.edg)
  g <- g + edges(new.edg.igraph)
  
  
### Draft of alternative if we were to look at a notion of "distance" between approximate models
  
#   c <- combn(V(net),2)
#   samp <- matrix(1,dim(c)[1]+1,dim(c)[2])
#   samp[1:2,] <- c
#   diff <- abs(get.vertex.attribute(net,"approx",c[1,]) - get.vertex.attribute(net,"approx",c[2,]))
#   diff.max <- max(diff)
#   diff <- rep(1,dim(c)[2]) - diff/diff.max
#   sample(1:dim(c)[2], n.edg , prob = diff)
  
  return(g)
  
  }


