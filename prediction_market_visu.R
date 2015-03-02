####
## Options
####

visu <- 1   # plots the network if 1

####
## Functions
####

# Construct model

source("generate_model.R")
source("generate_data.R")

# Operate on model

source("form_expect.R")
source("behav.R")
source("interact.R")
source("payoffs.R")
source("adapt.R")


if (visu == 1){
source("colored.R")
}

####
## Libaries
####

library(igraph)

#####
## Set model's parameters and create corresponding network
#####

net <- GenerateModel ( ### Network parameters
  
                       n.traders  = 100,
                       n.edg      = 150,
                       seg        = 0.95, # determine initial segregation of the 
                       # network. The higher seg, the higher the initial segregation
                                              
                       ### Market structure parameters
                       
                       market.struct   = "CDA", # chose between CDA and LMSR
                       market.complet  = 4,   # number of securities which
                       # are traded. With higher securities, traders can trade on
                       # more precise temperature intervals
                      
                       ### Behavior parameters
                       
                       risk.tak = 0.0001, # IN PER/10000 Determine the distribution of risk
                       # taking behavior. The higher risk.taking, the more agent
                       # will try to buy (resp. sell) lower (resp. higher) than
                       # their reservation price.
                       ideo = 0.1,     # IN PERCENT. Determines the degree of "ideology" embedded
                       # in the approximate models. If ideo is high, traders will not
                       # revise their approximate models easily, even when faced with
                       # strong evidence (conversely if low). In practice, each traders'
                       # ideological parameter will be drawn from [0,ideo] and is the
                       # probability that a trader adopt the approximate model of her
                       # most succesfull neighbour in the social network, where success
                       # is measured by cumulative monetary gains on the market (over
                       # all past trading sequences and periods)
                       
                       ### Timing parameters
                       
                       # Number of burnin periods for initial calibration
                       
                       burn.in    = 10,
                       
                       # Number of trading sequences
                       
                       n.seq       = 20,
                      
                       # Number of trading periods in each sequence
                       
                       horizon    = 10
                       
                       )

#####
## Construct "true" time series (random at this point, to be later calibrated with actual data)
#####

D <- GenerateData(h = (net$burn.in + (net$horizon * net$n.seq )))


# Visualize network (optional)

if (visu == 1){
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
## expected distribution for the future outcome
#####

net <- FormExpect(g = net, ct = t, data = D) 

#####
## Traders form their buy and sell orders
#####

net <- Behav( g = net, data = D)

#####
## Traders exchange on the market
#####

net <- Interact(g = net)

}

#####
## Pay the winning securities
#####

net <- Payoffs(g=net, data = D)

#####
## Adapt approximate model
#####

net <- Adapt(g = net)

plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size = 7)

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
    
    net <- FormExpect(g = net, ct = t, data = D) 
    
    #####
    ## Traders form their buy and sell orders
    #####
    
    net <- Behav( g = net, data = D)
    
    #####
    ## Traders exchange on the market
    #####
    
    net <- Interact(g = net)
    
    }

#####
## Pay the winning securities
#####

net <- Payoffs(g=net, data = D)

#####
## Adapt approximate model
#####

net <- Adapt( g = net)

plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size = 7)
  }
}
