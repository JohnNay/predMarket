library(igraph)
library(purrr)


ShapeNet <- function(
                      g,
                      n.edg = vcount(net)*4,
                      seg  = 0.5,
                      min_edg = 2
)
  {
  
  # package requirement

  # Generate links between agents as a function of whether they share the same approximate model

  # Ensure every trader has at least min_edg connections
  for (i in 1:min_edg) {
    nv <- length(V(g))
    x <- V(g)[[1]]
    homoph <- (get.vertex.attribute(g, "approx", x) == get.vertex.attribute(g, "approx", V(g))) %>%
      as.numeric()
    p <- (1 - seg) + seg * homoph
    # break the traders into two groups, which we call "red" and "blue"
    # We use th esame homophily logic as below to segment the two groups
    red <- sample(V(g), nv %/% 2, replace = FALSE, prob = p)
    blue <- V(g) %>% discard(~.x %in% red)
    red <- matrix(red, nrow = 2, byrow = TRUE)
    ## What to do if there's an odd number of traders?
    x <- NULL
    if (length(blue) %% 2 > 0) {
      x <- blue[[1]]
      blue <- blue[[-1]]
    }
    blue <- matrix(blue, nrow = 2, byrow = TRUE)
    new.edg <- cbind(red, blue)
    new.edg.igraph <- c(new.edg)
    g <- g + edges(new.edg.igraph)
  }

  n_edges <- lapply(V(g), function(v) length(incident(g,v))) %>% unlist()
  message("min edges = ", min(n_edges), ", max edges = ", max(n_edges))
  singletons <- V(g)[n_edges < min_edg]
  message(length(singletons), " singletons")
  for (v in singletons) {
    needed <- min_edg - incident(g, v)
    message("Needed = ", needed, ", min_edg = ", min_edg, ", incident = ", incident(g,v))
    others <- V(g)[-v]
    homoph <- (get.vertex.attribute(g, "approx", v) == get.vertex.attribute(g, "approx", others)) %>%
      as.numeric()
    p <- (1 - seg) + seg * homoph
    new.edg <- matrix(c(rep(v, needed), sample(others, needed, replace = F, prob = p)),
                      nrow=2, byrow = TRUE)
    message("new.edg = ", dim(new.edg))
    new.edg.igraph <- c(new.edg)
    g <- g + edges(new.edg.igraph)
  }

  n.edg <- n.edg - length(E(g))
  
  

  # In the current procedure below "n.edg" are selected among all possible links between agents in the network
  # The probability that a link be selected depends on whether the agents share the same approximate model
  # In the currect procedure, the probability that a link be formed is
  #            - (1-seg)/(number of possible nodes), if the two traders have different approximate models
  #            - 1/(number of possible nodes)      , if the two traders have the same approximate model     

  
  
  c <- combn(V(g),2)
  # Avoid making the same connection twice
  connected <- lapply(1:ncol(c), function(i) are_adjacent(g, c[1,i], c[2,i])) %>% unlist()
  c <- c[,!connected]
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


