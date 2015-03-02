
shapenet <- function(
                      g = net,
                      nedg = vcount(net)*2,
                      seg  = 0.5
                    )
  {
  
  # package requirement
  require(igraph)
  
  c <- combn(V(g),2)
  samp <- matrix(1,dim(c)[1]+1,dim(c)[2])
  samp[1:2,] <- c
  diff <- get.vertex.attribute(g,"approx",c[1,]) - get.vertex.attribute(g,"approx",c[2,])
  homoph <- as.numeric(diff == 0)
  select <- sample(1:dim(c)[2], nedg , prob = rep((1-seg),dim(c)[2]) + homoph*seg) 
  newedg <- c[1:2,select]
  newedg.igraph <- c(newedg)
  g <- g + edges(newedg.igraph)
  
  
### Draft of alternative if we were to look at a notion of "distance" between approximate models
  
#   c <- combn(V(net),2)
#   samp <- matrix(1,dim(c)[1]+1,dim(c)[2])
#   samp[1:2,] <- c
#   diff <- abs(get.vertex.attribute(net,"approx",c[1,]) - get.vertex.attribute(net,"approx",c[2,]))
#   diff.max <- max(diff)
#   diff <- rep(1,dim(c)[2]) - diff/diff.max
#   sample(1:dim(c)[2], nedg , prob = diff)
  
  return(g)
  
  }


