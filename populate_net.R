
PopulateNet <- function( 
                        n.traders    = 100,
                        mon.dis      = rep(1,n.traders),
                        approx.dis   = sample(1:5, n.traders, replace = TRUE),
                        behav.dis    = sample(1:1, n.traders, replace = TRUE),
                        risk.tak     = 1/2,
                        ideo         = 0.5,
                        market.complet = 10,
                        burn.in
                      ) 
  {  

  # Input consistency checks (to be added)
  
  # package requirement
  library(igraph)
  
  ## construct empty graph
  g <- graph.empty(n=n.traders, directed = FALSE)
  
  ## Assign attributes to agents in graph

  V(g)$money  <- mon.dis
  V(g)$approx <- approx.dis 
    #NOTE : in the preliminary version, approximate models are taken from Sumner and Jackson (2008) 
            # approx = 1  ->  Full model : ACC true, both geoingeneering and policy effective (aka full)
            # approx = 2  ->  ACC is a myth (aka myth)
            # approx = 3  ->  ACC is true but all measures are ineffective (aka ineff)
            # approx = 4  ->  ACC is true and policy are effective, but not geoingeneering (aka policy)
            # approx = 5  ->  ACC is true, geoingeneering is effective, but policy is not (aka engineer)
  
  ## Assign values for the distance between approximate models. 
  
  g <- set.graph.attribute(g, "burn.in", burn.in)
  
  g$dist <- matrix (c (0,3,2,1,1,
                   3,0,1,1,2,
                   2,3,0,1,1,
                   1,2,3,0,0,
                   1,2,3,0,0),
                nrow=5,
                ncol=5)

  
  # Will be used for
  # convergence analysis in order to compute some social utility associated
  # with a distribution of distances to the true model emerging from the 
  # distribution of approximate models.
  
  ## Record convergence utility of the initial network
  
  g$init.converg.util <- converg.util(g)
  
  V(g)$behav  <- behav.dis  # Market behavior of the traders, only ZI in preliminary model
  
  
  
  risk.tak.dis <- sample(0:(risk.tak*10000)/10000, n.traders, replace = TRUE)
  V(g)$risk.tak <- risk.tak.dis # Tendency of a player to take risk on the market by 
                                    # - placing buy orders way below reservation price
                                    # - placing sell orders way above reservation price
                                # each period a risk parameter 'r' is drawn 
                                # from 0:risk.tak.m for each player, where risk.tak \in [0,1]
                                # each trader then place 
                                    # - buy orders r percents below reservetation price
                                    # - sell orders r percents above reservation price
  
  ideo.dis <- sample(0:((1-ideo)*100)/100, n.traders, replace = TRUE)
  V(g)$ideo <- ideo.dis
  
  secu.dis     = list(rep(1,market.complet))
  V(g)$secu   <- secu.dis   # Securities held by the traders
  
    
  return(g)  } 


