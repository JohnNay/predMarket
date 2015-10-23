
library(igraph)

# Construct model:
source("generate_model.R")
# Sub functions needed inside of generate_model():
source("populate_net.R")
source("shape_net.R")
source("mixing_matrix.R")
source("assortativity_coefficient.R")
# Construct data for model:
library(pracma)
source("generate_data.R")
# Operate on model:
source("form_expect.R")
source("behav.R")
source("interact.R")
source("payoffs.R")
source("adapt.R")
source("mixing_matrix.R")
source("assortativity_coefficient.R")

main2 <- function(parameters,
                  iterations = 10,
                  burn.in = 4,
                  n.seq = 28,
                  horizon = 4,
                 out = c("segreg", "converg"),
                 visu = FALSE,
                 record = FALSE) {
  
  nyears <- 135
  # TODO: set nyears to adapt to whether there is future or not.
  # TODO: burn.in + n.seq * horizon all need to adapt
  
  stopifnot((burn.in + n.seq * horizon) == nyears)
  
  ### Market structure parameters:
  # market.complet is number of securities which
  # are traded. With higher securities, traders can trade on
  # more precise temperature intervals
  # TODO: market.complet needs an upper bound if its varied
  # or we can keep it fixed, for now we keep it at 10
  market.complet <- 10 #ifelse(parameters[4]*1000 < 1, 1, round(parameters[4]*1000)) # integer in (1, 1000)
  
  # SA creates everything in 0-1 interval, so im scaling things to the interval we want inside here
  seg <- parameters[1] # continuous value in (0,1)
  ideo <- parameters[2] # continuous value in (0,1)
  risk.tak <- parameters[3] # continuous value in (0,1)
  true.model <- parameters[4] + 1 # the true model, 1 for ACC is a myth, 2 for ACC is fction of GHG
  n.edg <- round(parameters[5]*100) + 100 # integer in (100, 200)
  n.traders <- round(parameters[6]*100) + 50 # integer in (50, 250)
  
  cat("seg", seg, "ideo", ideo, "risk.tak", risk.tak, "market.complet", market.complet,
            "true.model", true.model, "\n")
  
  out <- match.arg(out)
  
  if (visu){
    source("colored.R")
  }
  
  # Making this a list rather than a vector, because if record==TRUE, each result is itself a vector > length 1
  result_final <- as.list(rep(NA, iterations))
  
  for(iteration in seq(iterations)){
    #####
    ## Set model's parameters and create corresponding network
    #####
    # TODO: run jonathans code and create climate and covariates data
    #conditional on which is TRUE model, only generate that data to save time
    # doing this here inside the loop of iterations allows us to average over the randomness in this
    # TODO: using that data, then create predictions for all agents, they can switch between the two model
    # we create both the predictions and the actual reservation prices all before
    # number of predictions depends on the number of securities, we want this flexibility
    # 1 data.frame where rows are time and columns are the securities and the entries are reservation prices for TSI
    # 1 data.frame where rows are time and columns are the securities and the entries are reservation prices for log.co2
    # they need to be attached to the network after generate model and then left alone until this place in next iteration.
    
    net <- generate_model( ### Network parameters
      n.traders  = n.traders,
      n.edg      = n.edg,
      seg        = seg, # determine initial segregation of the
      # network. The higher seg, the higher the initial segregation
      ### Behavior parameters:
      risk.tak = risk.tak, # IN PERCENT. Determines the distribution of risk
      # taking behavior. The higher risk.taking, the more agent
      # will try to buy (resp. sell) lower (resp. higher) than
      # their reservation price. Agent i tries to buy at (resrv_i * (1 - risk.tak_i))
      # and tries to sell at (resrv_i * (1 + risk.tak_i))
      ideo = ideo,     # IN PERCENT. Determines the degree of "ideology" embedded
      # in the approximate models. If ideo is high, traders will not
      # revise their approximate models easily, even when faced with
      # strong evidence (conversely if low). In practice, each traders'
      # ideological parameter will be drawn from [0,ideo] and is the
      # probability that a trader adopt the approximate model of her
      # most succesfull neighbour in the social network, where success
      # is measured by cumulative monetary gains on the market (over
      # all past trading sequences and periods)
      
      market.complet = market.complet,
      ### Timing parameters:
      
      # Number of periods burned for initial calibration
      burn.in    = burn.in, # 16
      # Number of trading sequences
      n.seq       = n.seq, # 20
      # The number of periods in each sequence (= number of trading periods + 1)
      horizon    = horizon # 5
      
      ###############
      ### WARNING ### timing parameters must match empirical data used in generate_data
      ###############
    )
    
#     #####
#     ## Construct "true" time series (random at this point, to be later calibrated with actual data)
#     #####
#     D <- generate_data(t.mod = true.model,
#                        t.fin = (net$burn.in + (net$horizon * net$n.seq )))
    
    
    # Visualize network (optional)
    if (visu){
      net <- Colored(net)
      igraph::plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size = 7)
    }
    #############################################
    #############################################
    #########           RUN             #########
    #############################################
    #############################################
    
    ########                      ############
    ######   First trading sequence   ########
    ########                      ############
    from <- net$burn.in + 1 # the period at which the model starts
    to <- net$burn.in + net$horizon - 1 # the period at which trading stops and securities
    # are realized. The -1 accounts for the fact that no trade occurs in the last
    # periods. Securities are just realized and payoffs distributed.
    for (t in from:to){
      #####
      ## Traders calibrate their approximate model and determine their
      ## expected distribution for the future out
      #####
      net <- FormExpect(net, ct = t, D)
      #####
      ## Traders chose their buy and sell orders
      #####
      net <- Behav( net, D)
      #####
      ## Traders exchange on the market
      #####
      net <- Interact(g = net)
    }
    #####
    ## Pay the winning securities
    #####
    net <- Payoffs(g=net, D)
    #####
    ## Adapt approximate model
    #####
    net <- Adapt(g = net)
    
    if (visu) plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size = 7)
    
    if(record){
      result <- (length(V(net)$approx[V(net)$approx == 1])/length(V(net))) - net$init.converg.util
    }
    
    ########                            ############
    ######   Subsequent trading sequences   ########
    ########                            ############
    
    if (net$n.seq>1){
      toto <- net$n.seq  # the number of trading sequences
      for (ts in 2:toto){
        #TODO: insert initialization of the securities so at beg of each round they dont have leftovers
        
        from <- net$burn.in + (net$horizon * ts) + 1 # the period at which the sequence starts
        to <- net$burn.in + (net$horizon * ts) - 1 # the period at which trading stops and securities
        # are realized. The -1 accounts for the fact that no trade occurs in the last
        # periods. Securities are just realized and payoffs distributed.
        for (t in from:to){
          #####
          ## Traders calibrate their approximate model and determine their
          ## expected distribution for the future outcome
          #####
          net <- FormExpect( net, ct = t, D)
          #####
          ## Traders form their buy and sell orders
          #####
          net <- Behav(net, D)
          #####
          ## Traders exchange on the market
          #####
          net <- Interact(g = net)
        }
        #####
        ## Pay the winning securities
        #####
        net <- Payoffs(g=net, D)
        #####
        ## Adapt approximate model
        #####
        net <- Adapt( g = net)
        
        if (visu) plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size = 7)
        
        if(record){
          result <- append(result, (length(V(net)$approx[V(net)$approx == 1])/length(V(net))) - net$init.converg.util)
        }
      }
    }
    
    if (record == FALSE){
      # return difference in utility of network convergence
      result <- (length(V(net)$approx[V(net)$approx == 1])/length(V(net))) - net$init.converg.util
    }
    
    result_final[[iteration]] <- result
  }
  
  if (record==FALSE){
    return(mean(sapply(result_final, function(x) x)))
  } else {
    return(colMeans(sapply(result_final, function(x) x)))
  }
}


