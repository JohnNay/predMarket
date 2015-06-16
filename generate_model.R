generate_model <- function ( 
  # Network parameters
  n.traders  = 100,
  n.edg      = 100,
  seg        = 0.9, # determine initial segregation of the 
  # network. The higher seg, the higher the initial segregation 
  # In the currect procedure, the probability that a link be formed is
  #            - (1-seg)/(number of possible nodes), if the two traders have different approximate models
  #            - 1/(number of possible nodes)      , if the two traders have the same approximate model
  
  # Market structure parameters
  market.struct   = c("CDA", "LMSR"), # chose between CDA and LMSR
  market.complet  = 10,   # number of securities which
  # are traded. With higher securities, traders can trade on
  # more precise temperature intervals
  
  # Behavior parameters
  risk.tak = 1/2, # Determine the distribution of risk
  # taking behavior. The higher risk.taking, the more agent
  # will try to buy (resp. sell) lower (resp. higher) than
  # their reservation price.
  ideo = 0.5, # Determines the degree of "ideology" embedded
  # in the approximate models. If ideo is high, traders will not
  # revise their approximate models easily, even when faced with
  # strong evidence (conversely if low).
  
  # Timing parameters
  burn.in    = 16,
  horizon    = 5,
  n.seq      = 20
){
  
  # BC of the empirical data: (There are 116 data points in the data set.)
  stopifnot((burn.in + n.seq * horizon) == 116)
  # Otherwise, agents will start to try to access inexistent data
  
  market.struct <- match.arg(market.struct)
  
  #####
  ## Set model's parameters and create corresponding network
  #####
  
  net <- PopulateNet( n.traders = n.traders,
                      risk.tak = risk.tak,
                      market.complet  = market.complet,
                      ideo = ideo, 
                      burn.in = burn.in
  )
  
  
  #####
  ## Create links in the network according to homophily in terms of approximate model
  #####
  net <- ShapeNet(g = net,
                  seg = seg,
                  n.edg = n.edg)
  
#   #####
#   ## Record initial of outcomes 
#   #####

## Record convergence of the initial network

net$init.converg.util <- length(V(net)$approx[V(net)$approx == 1])/length(V(net))

### Assortativity ###

#   # calculate the mixing matrix
#   m <- mixmat(net, 'approx')
#   
#   # now calculate the assortativity coefficient
#   ac <- assortcoeff(m)
#   
#   net <- set.graph.attribute(net,"ac.init",ac)
  
  #####
  ## Set additional models parameter
  #####
  net <- set.graph.attribute(net,"burn.in",burn.in)
  
  net <- set.graph.attribute(net,"horizon",horizon)
  
  net <- set.graph.attribute(net,"market.struct",market.struct)
  
  net <- set.graph.attribute(net,"n.seq",n.seq)
  
  net
}
