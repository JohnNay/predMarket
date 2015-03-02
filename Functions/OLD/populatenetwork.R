
populatenet <- function( 
                        ntraders  = 10,
                        mondis    = rep(1,ntraders),
                        approxdis = sample(1:6, ntraders, replace = TRUE),
                        behavdis  = sample(1:1, ntraders, replace = TRUE),
                        secudis   = list(rep(1,10))
                        
                      ) 
  {  

  # Input consistency checks (to be added)
  
  # package requirement
  require(igraph)
  
  # construct empty graph
  g = graph.empty(n=ntraders, directed = FALSE)
  
  # Assign attributes accros agents in graph

  V(g)$money  = mondis
  V(g)$approx = approxdis 
  V(g)$behav  = behavdis
  V(g)$secu   = secudis
  
  return(g)  } 


