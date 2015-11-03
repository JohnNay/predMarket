library(igraph)
# Construct model:
source("generate_model.R")
# Sub functions needed inside of generate_model():
source("populate_net.R")
source("shape_net.R")
source("mixing_matrix.R")
# Data and predictions
source("data_and_reservation_prices.R")
# Operate on model:
source("form_expect.R")
source("behav.R")
source("interact.R")
source("payoffs.R")
source("adapt.R")
source("mixing_matrix.R")
source("assortativity_coefficient.R")

main <- function(parameters,
                 iterations = 10,
                 burn.in = 51,
                 n.seq = 14,
                 horizon = 6,
                 nyears = 135,
                 visu = FALSE,
                 record = FALSE,
                 safeNprint=FALSE) {
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
  true.model <- parameters[4] + 1 # the true model, 1 for slow.tsi, 2 for log.co2
  n.edg <- round(parameters[5]*100) + 100 # integer in (100, 200)
  n.traders <- round(parameters[6]*100) + 50 # integer in (50, 250)
  
  cat("seg", seg, "ideo", ideo, "risk.tak", risk.tak, "market.complet", market.complet,
      "true.model", true.model, "n.edg", n.edg, "n.traders", n.traders,  "\n")
  
  if (visu){
    source("colored.R")
  }
  
  # Making this a list rather than a vector, because if record==TRUE, each result is itself a vector > length 1
  result_final <- as.list(rep(NA, iterations))
  
  for(iteration in seq(iterations)){
    #####
    ## Set model's parameters and create corresponding network
    ## with attached value of parameters
    #####
    net <- generate_model( 
      ### Data parameters
      true.model = true.model,
      ### Timing parameters:
      # Number of periods burned for initial calibration
      burn.in    = burn.in, # 16
      # Number of trading sequences
      n.seq       = n.seq, # 20
      # The number of periods in each sequence (= number of trading periods + 1)
      horizon    = horizon, # 5
      ###############
      ### WARNING ### timing parameters must match empirical data used in generate_data
      ###############
      ### Network parameters
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
      ### Market parameters
      market.complet = market.complet
    )
    
    # TODO: run jonathans code and create climate and covariates data
    #conditional on which is TRUE model, only generate that data to save time
    # doing this here inside the loop of iterations allows us to average over the randomness in this
    # TODO: using that data, then create predictions for all agents, they can switch between the two model
    # we create both the predictions and the actual reservation prices all before
    # number of predictions depends on the number of securities, we want this flexibility
    # 1 data.frame where rows are time and columns are the securities and the entries are reservation prices for TSI
    # 1 data.frame where rows are time and columns are the securities and the entries are reservation prices for log.co2
    # they need to be attached to the network after generate model and then left alone until this place in next iteration.
    
    #####
    ## Generate data and reservation prices, and attach to network
    #####
    
    net <- DataPrediction(net, scenario = 'rcp26', true.model = true.model)
    
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
    
    ## Initialize money 
    
    V(net)$money <- rep(1,length(V(net)))
    
    message("sequence 1")
    for (t in from:to){
      message(paste0("period ",t))
      #####
      ## Traders chose their buy and sell orders
      #####
      
      ### BEHAVE
      net <- Behav(net, ct = t)
      
      #####
      ## Traders exchange on the market
      #####
      
      ### Safeguards & prints
      if(safeNprint){
        money_pre_interact <- V(net)$money
        print("V(net)$money before interact")
        print(V(net)$money)
        print("V(net)$secu before interact")
        print(unlist(V(net)$secu))
      }
      
      ### TRADE
      net <- Interact(g = net)
      
      ### Safeguards & prints
      if(safeNprint){
        print("Change in V(net)$money from interact")
        print(V(net)$money - money_pre_interact)
        print("V(net)$money after interact")
        print(V(net)$money)
        print("V(net)$secu after interact")
        print(unlist(V(net)$secu))
        message(paste0("sum of money equals ", sum(V(net)$money)))
        if(sum(V(net)$money)!=n.traders){
          stop(paste0("In the first sequence, some money is create at t =", t))
        } 
      }
      if(any(unlist(V(net)$secu)<0)){
        stop(paste0("Some securities fell below zero in period ",t))
      } 
      if(any(V(net)$money<0)){
        stop(paste0("Some securities fell below zero in period ",t))
      } 
    }
    #####
    ## Pay the winning securities
    #####
    
    ### PAY    
    
    net <- Payoffs(g=net, ct = t)
    
    ### Safeguards & prints
    if(safeNprint){
      message(paste0("n.traders is ",n.traders))
              message(paste0("sum of money is ", sum(V(net)$money)))
    }
    
    if(abs(sum(V(net)$money)!= 2*n.traders) > 0.1){
      stop(paste0("After payoffs of the first sequence, some money is create at t =", t,
                  "The money created is ", sum(V(net)$money) - 2*n.traders))
    } 
    #####
    ## Adapt approximate model
    #####
    net <- Adapt(g = net)
    
    if (visu) plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size = 7)
    
    if(record){
      result <- (length(V(net)$approx[V(net)$approx == true.model])/length(V(net))) - net$init.converg.util
    }
    
    ########                            ############
    ######   Subsequent trading sequences   ########
    ########                            ############
    
    if (net$n.seq>1){
      toto <- net$n.seq  # the number of trading sequences
      for (ts in 2:toto){
        message(paste0("sequence ",ts))
        #TODO: insert initialization of the securities so at beg of each round they dont have leftovers
        
        ### INITIALIZE SECURITIES###
        V(net)$secu <- list(rep(1,market.complet + 2))
        ### Start trading periods in the sequence ###  
        from <- net$burn.in + (net$horizon * (ts-1)) + 1 # the period at which the sequence starts
        to <- net$burn.in + (net$horizon * ts) - 1 # the period at which trading stops and securities
        # are realized. The -1 accounts for the fact that no trade occurs in the last
        # periods. Securities are just realized and payoffs distributed.
        for (t in from:to){
          message(paste0("period",t))
          #####
          ## Traders chose their buy and sell orders
          #####
          net <- Behav(net, ct = t)
          
          
          
          #####
          ## Traders exchange on the market
          #####
          
          ###       Safeguards & prints
          if(safeNprint){
            money_pre_interact <- V(net)$money
            print("V(net)$money before interact")
            print(V(net)$money)
            print("V(net)$secu before interact")
            print(unlist(V(net)$secu))
          }
          
          ### TRADE
          
          net <- Interact(g = net)
          
          ###       Safeguards & prints
          if(safeNprint){
            print("Change in V(net)$money from interact")
            print(V(net)$money - money_pre_interact)
            print("V(net)$money after interact")
            print(V(net)$money)
            print("V(net)$secu after interact")
            print(unlist(V(net)$secu))
            message(sum(V(net)$money))
            message((ts)*n.traders)
          }
          if(abs(sum(V(net)$money)!=(ts)*n.traders)>0.1){
            stop(paste0("In a sequence after the first sequence,
                        some money is create at t =", t,
                        "The money created is ", sum(V(net)$money) - (ts +1)*n.traders))  
          } 
          if(any(unlist(V(net)$secu)<0)){
            stop(paste0("Some securities fell below zero in period ",t))
          } 
          if(any(V(net)$money<0)){
            stop(paste0("Some securities fell below zero in period ",t))
          } 
        }
        #####
        ## Pay the winning securities
        #####
        
        ### Safeguards & prints
        money_pre_payoff <- V(net)$money # record money pre-payoff for
        # comparison after payoff
        
        if(safeNprint){
          print("V(net)$money before payoff")
          print(V(net)$money)
          print("V(net)$secu before payoff")
          print(unlist(V(net)$secu))
        }
        ### PAY
        
        net <- Payoffs(g=net, ct = t)
        
        ### Safeguards & prints
        if(safeNprint){
          print("Change in V(net)$money from payoff")
          print(V(net)$money - money_pre_payoff)
          print("V(net)$money after payoff")
          print(V(net)$money)
          
          message(sum(V(net)$money))
          message((ts +1)*n.traders)
        }
        if(any(V(net)$money < money_pre_payoff)){
          stop(paste0("In period t = ", t, "some trader got a negative payoff"))
        }
        
        if(abs(sum(V(net)$money)- (ts +1)*n.traders)>0.1){
          stop(paste0("After payoffs of some sequence past the first sequence,
                      some money is create at t =", t,
                      "The money created is ", sum(V(net)$money) - (ts +1)*n.traders))
        } 
        
        #####
        ## Adapt approximate model
        #####
        net <- Adapt(g = net)
        
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
