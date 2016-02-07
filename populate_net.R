
PopulateNet <- function( 
                        n.traders    = 100,
                        mon.dis      = rep(1, n.traders),
                        approx.dis   = 
                          sample ( 
                            append( 
                              rep(1, n.traders %/% 2),
                              append(rep(2, n.traders %/% 2), sample(1:2, n.traders %% 2))
                                  )
                                 ),
                        behav.dis    = sample(1:1, n.traders, replace = TRUE),
                        risk.tak     = 0.5,
                        ideo         = 0.5,
                        market.complet = 10,
                        burn.in
                      ) 
  {  

  # Input consistency checks (to be added)
  
  
  ## construct empty graph
  g <- graph.empty(n=n.traders, directed = FALSE)
  
  ## Assign attributes to agents in graph

  V(g)$money  <- mon.dis
  V(g)$approx <- approx.dis 
            # approx = 1  ->  tsi
            # approx = 2  ->  log.co2
           
  g <- set.graph.attribute(g,"n.approx",length(unique(approx.dis)))

  # Assign behavioral parameters to traders
  
  V(g)$risk.tak <- runif(n.traders,0,risk.tak) # Tendency of a player to take risk on the market by 
                                    # - placing buy orders way below reservation price
                                    # - placing sell orders way above reservation price
                                # each period a risk parameter 'r' is drawn 
                                # from 0:risk.tak.m for each player, where risk.tak \in [0,1]
                                # each trader then place 
                                    # - buy orders r percents below reservetation price
                                    # - sell orders r percents above reservation price
  
  V(g)$ideo <- runif(n.traders,0,ideo)
  
  secu.dis     = list(rep(1,market.complet + 2))    # The "+2" accounts for the fact
                  # that in the market implementation in behav.r, we have 2 default
                  # securities that cover temperature below 1.0001 * min.temp (empirical min)
                  # and above 0.9999 * max.temp (empirical max)
  V(g)$secu   <- secu.dis   # Securities held by the traders
  
    
  g
} 


