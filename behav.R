
Behav <- function (
  g,
  ct #current time period
)
{
  
  # package requirement
  require(igraph)
  
  ######
  ## Useful variables and traders' variable initializations
  ######

  n.secu    <- length((V(g)$secu)[[1]])
  n.traders <- length(V(g))
  n.approx  <- g$n.approx
    
  #######
  ## Traders pick at random a security that they will try to BUY once on the market
  #######
  
  V(g)$buy.which <- sample(n.secu, n.traders, replace = TRUE)
  
  #######
  ## Traders set a PRICE to place a BUY order (must be lower than traders's money stock)
  #######
  
  for (i in 1:n.traders){
    if (V(g)$approx[i] == 1){
        V(g)$buy.price[i] <-min(
                            g$reserv.tsi[ct,V(g)$buy.which[i]]*(1+V(g)$risk.tak[i]),
                            V(g)$money[i]      
        )
    }
    if (V(g)$approx[i] == 2){
        V(g)$buy.price[i] <-min(
                            g$reserv.co2[ct,V(g)$buy.which[i]]*(1+V(g)$risk.tak[i]),
                            V(g)$money[i]      
      )
    }
  }
  
  
  #######
  ## Traders pick at random a security they have positive amount of to place a SELL offer 
  #######
  
  secu.mat <- do.call(rbind, V(g)$secu)
  
  # for each trader, draw at random among the securities she has positive amount of
  
  sell <- rep (n.secu + 1,n.traders)  # the value n.secu +1 is chosen so that traders
  # who do not sell will be assigned a selling price of "NA" based on the reserv matrix,
  # see below
  
  
  for (i in 1:n.traders)
  { 
    if (length(secu.mat[i,][secu.mat[i,] >= 1]) ==1) { 
      # if the traders own a positive amount of a single security
      sell[i] <- which(secu.mat[i,] >= 1) 
    }
    if (length(secu.mat[i,][secu.mat[i,] >= 1]) >1) { 
      # if the traders own a positive amount of some security
      sell[i] <- sample(which(secu.mat[i,] >= 1),1) 
    }
  }
  
  V(g)$sell.which <- sell
  
  #######
  ## Traders set a PRICE to place a SELL order
  #######
  
    for (i in 1:n.traders){
      if (V(g)$approx[i] == 1){
        V(g)$sell.price[i] <-g$reserv.tsi[ct,V(g)$sell.which[i]]*(1+V(g)$risk.tak[i])
      }
      if (V(g)$approx[i] == 2){
        V(g)$sell.price[i] <-g$reserv.co2[ct,V(g)$buy.which[i]]*(1+V(g)$risk.tak[i])
      }
    }

#######
## Return the network
#######

  g
  
}