
PopulateNet <- function( 
                        n.traders    = 100,
                        mon.dis      = rep(1,n.traders),
                        approx.dis   = sample(1:5, n.traders, replace = TRUE),
                        behav.dis    = sample(1:1, n.traders, replace = TRUE),
                        risk.tak.dis = sample(0:50/100, n.traders, replace = TRUE),
                        secu.dis     = list(rep(1,10))              
                      ) 
  {  

  # Input consistency checks (to be added)
  
  # package requirement
  require(igraph)
  
  # construct empty graph
  g <- graph.empty(n=n.traders, directed = FALSE)
  
  # Assign attributes to agents in graph

  V(g)$money  <- mon.dis
  V(g)$approx <- approx.dis 
    #NOTE : in the preliminary version, approximate models are taken from Sumner and Jackson (2008) 
            # approx = 1  ->  true model (aka true)
            # approx = 2  ->  ACC is a myth (aka myth)
            # approx = 3  ->  ACC is true but all measures are ineffective (aka ineff)
            # approx = 4  ->  ACC is true and policy are effective, but not geoingeneering (aka policy)
            # approx = 5  ->  ACC is true, geoingeneering is effective, but policy is not (aka engineer)
  V(g)$behav  <- behav.dis  # Market behavior of the traders, only ZI in preliminary model
  V(g)$risk.tak <- risk.tak.dis # Tendency of a player to take risk on the market by 
                                    # - placing buy orders way below reservation price
                                    # - placing sell orders way above reservation price
                                # each period a risk parameter 'r' is drawn 
                                # from 0:risk.tak.m for each player, where risk.tak \in [0,1]
                                # each trader then place 
                                    # - buy orders r percents below reservetation price
                                    # - sell orders r percents above reservation price
  
  V(g)$secu   <- secu.dis   # Securities held by the traders
  
    
  return(g)  } 


