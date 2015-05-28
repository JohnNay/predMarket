
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
                 market.struct = c("CDA", "LMSR"), 
                 out = c("segreg", "converg"),
                 visu = FALSE,
                 record = FALSE) {
  ### Market structure parameters:
  # market.struct, choses between CDA and LMSR
  # market.complet is number of securities which
  # are traded. With higher securities, traders can trade on
  # more precise temperature intervals
  
  
  # SA creates everything in 0-1 interval, so im scaling things to the interval we want inside here
  seg <- parameters[1] # continuous value in (0,1)
  ideo <- parameters[2] # continuous value in (0,1)
  risk.tak <- parameters[3] # continuous value in (0,1)
  market.complet <- ifelse(parameters[4]*1000 < 1, 1, round(parameters[4]*1000)) # integer in (1, 1000)
  true.model <- parameters[5] + 1 # the true model, 1 for ACC is a myth, 2 for ACC is fction of GHG
  n.edg <- round(parameters[6]*100) + 100 # integer in (100, 200)
  n.traders <- round(parameters[7]*100) + 50 # integer in (50, 250)
  
  
  cat("seg", seg, "ideo", ideo, "risk.tak", risk.tak, "market.complet", market.complet,
            "true.model", true.model, "\n")
  
  market.struct <- match.arg(market.struct)
  out <- match.arg(out)
  
  library(igraph)
  if (visu){
    source("colored.R")
  }
  #####
  ## Set model's parameters and create corresponding network
  #####
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
    
    ### Timing parameters:
    
    # Number of periods burned for initial calibration
    burn.in    = 16,
    # Number of trading sequences
    n.seq       = 20,
    # The number of periods in each sequence (= number of trading periods + 1)
    horizon    = 5
    
    ###############
    ### WARNING ### timing parameters must match empirical data used in generate_data
    ###############
  )
  
  #####
  ## Construct "true" time series (random at this point, to be later calibrated with actual data)
  #####
  D <- generate_data(t.mod = true.model,
                     t.fin = (net$burn.in + (net$horizon * net$n.seq )))
  
  
  # Visualize network (optional)
  if (visu){
    net <- Colored(net)
    plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size = 7)
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
  
  if (visu){
    plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size = 7)}
  
  if(record){
    if (out == "segreg"){
      
      # calculate the mixing matrix
      m <- mixmat(net,'approx')
      
      # now calculate the assortativity coefficient
      result <- assortcoeff(m)
      
    }
    
    if(out == "converg"){
      
      result <- length(V(net)$approx[V(net)$approx == 1])/length(V(net))
      
    }
  }
  
  ########                            ############
  ######   Subsequent trading sequences   ########
  ########                            ############
  
  if (net$n.seq>1){
    toto <- net$n.seq  # the number of trading sequences
    for (ts in 2:toto){
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
      
      
      if (visu){
        plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size = 7)}
      
      if(record){
        if (out == "segreg"){
          
          # calculate the mixing matrix
          m <- mixmat(net,'approx')
          
          # now calculate the assortativity coefficient
          result <- append(result,assortcoeff(m))

        }
        
        if(out == "converg"){
          
          result <- append(result,length(V(net)$approx[V(net)$approx == 1])/length(V(net)))
          
        }
      }
      
    }
  }
  
  # TODO:
  # need to compute some set of outcome measures here, like prices of a particular indices
  # and then return one index, the one that matches the arg of main() called "outcome" 
  # outcome
  
  if (out == "segreg" && record == FALSE){
    
    # calculate the mixing matrix
    m <- mixmat(net,'approx')
    
    # now calculate the assortativity coefficient
    ac.final <- assortcoeff(m)
    
    # report difference in assotativity with respect to initial network. 
    # less assortative network are "better", so ac.final<net$ac.init is "good"
    # and the more positive the reported difference, the more powerful the market
    # is at breaking assortativity
    result <- net$ac.init - ac.final  
    
    #See more at: http://www.babelgraph.org/wp/?p=351#sthash.yWfBpOhv.dpuf
  }
  
  if (out == "converg" && record == FALSE){
    
    # return difference in utility of network convergence
    
    result <- (length(V(net)$approx[V(net)$approx == 1])/length(V(net))) - net$init.converg.util
    
  }
  
  result
  
}


