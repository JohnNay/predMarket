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
  burn.in    = 100,
  horizon    = 50,
  n.seq      = 1
){
  market.struct <- match.arg(market.struct)
  ####
  ## Sub functions, 
  # these are sourced()/created in main.R, we dont need to recreate these functions everytime that generate_model() is called
  ####
    source("populate_net.R")
    source("shape_net.R")
    source("set_add_param.R")
    source("mixing_matrix.R")
    source("assortativity_coefficient.R")
  
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
  #####
  ## Measure and record initial assorativity
  #####
  
  # calculate the mixing matrix
  m <- mixmat(net,'approx')
  
  # now calculate the assortativity coefficient
  ac <- assortcoeff(m)
  
  net <- set.graph.attribute(net,"ac.init",ac)
  
  #####
  ## Set additional models parameter
  #####
  net <- SetAddParam(   g = net,
                        burn.in = burn.in,
                        horizon = horizon,
                        n.seq   = n.seq,
                        market.struct = market.struct) 
  
  net
}
