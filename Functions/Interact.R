
Interact <- function (
                        g     = net
  ){
  
  ######
  ## Useful variables
  ######
  
  n.secu    <- length((V(g)$secu)[[1]])
  n.traders <- length(V(g))
#   n.approx  <- length(unique(V(g)$approx))
  
  ############
  ######
  ## Market interaction for CDA
  ######
  ############

  # Reset market interactions

  V(g)$buy.order <- rep(0,n.traders)
  V(g)$sell.order <- rep(0,n.traders)
  

###########
### CDA ###
###########

  if (g$market.struct == "CDA"){
    
    # Draw a random order in which traders will come to the market
    
    turns <- sample(1:n.traders, n.traders, replace = FALSE)
    
    for (i in turns){ 
      
      # Player i places a buy order on the order book
      V(g)$buy.order[i] <- 1
      
      # Player i places a sell order on the order book, if she owns 
      # a positive amount of some security
      
      if (V(g)$sell.which[i] != n.secu + 1){  # n.secu+1 is the code for sell.which
      V(g)$sell.order[i] <- 1                 # corresponding to a trader not selling
      }
    
      ########
      ### Complete deals if possible
      ########
      
      ### Buying ###
      
      # Determine who the trader can buy from
      
      potential.sellers <- which ( V(g)$sell.order == 1 
                                   # who has a pending sell order
                                   & V(g)$sell.which == V(g)$buy.which[i]
                                   # and sells the security i wants to buy
                                   & V(g)$sell.price < V(g)$buy.price[i]
                                   # at a price lower than max i is willing to pay
                                   )
      
      potential.sellers <- potential.sellers[potential.sellers != i]
            # i cannot sell to herself
    
      # Realize trade if possible
      
      if(length(potential.sellers) > 0){
        
                  # determine minimum selling price
        
                  min.price <- min(V(g)$sell.price[potential.sellers])
        
                  # pick a potential seller who sells at min price. The "if else"
                  # statement is needed because when there is only one
                  # potential.sellers "x" who sells at the minimum price,
                  #  the sample function will pick at random in the
                  # interval 1:x instead of picking x itself
                  
                  if (length(potential.sellers[V(g)$sell.price[potential.sellers] == min.price]) == 1){
                    seller <- potential.sellers[V(g)$sell.price[potential.sellers] == min.price]
                  }
                  else {
                    seller <- sample(
                    potential.sellers[V(g)$sell.price[potential.sellers] == min.price],1)
                  }
      
                  # pay for the security
                  
                  V(g)$money[[seller]] <- V(g)$money[[seller]] + min.price
                  V(g)$money[[i]] <- V(g)$money[[i]] - min.price
                  
                  # transfer the security
                  
                  V(g)$secu[[seller]][V(g)$buy.which[i]] <- V(g)$secu[[seller]][V(g)$buy.which[i]] - 1
                  V(g)$secu[[i]][V(g)$buy.which[i]] <- V(g)$secu[[i]][V(g)$buy.which[i]] + 1
                  
                  # remove orders
                  
                  V(g)$buy.order[[i]] <- 0
                  V(g)$sell.order[[seller]] <- 0
        
      }
    
     ### Selling ###
     
     # Determine who the trader can sell to
     
     potential.buyers <- which ( V(g)$buy.order == 1 
                                  # who has a pending buy order
                                  & V(g)$buy.which == V(g)$sell.which[i]
                                  # and buys the security i wants to sell
                                  & V(g)$buy.price > V(g)$sell.price[i]
                                  # at a price lower than max i is willing to pay
                               )
     
     potential.buyers <- potential.buyers[potential.buyers != i]
     # i cannot buy from herself
     
     # Realize trade if possible
     
     if(length(potential.buyers) > 0){
       
       # determine max buying price
       
       max.price <- max(V(g)$buy.price[potential.buyers])
       
       # pick a potential buyer who buys at max price The "if else"
       # statement is needed as otherwise, when potential.buy is 
       # a singleton "x", the sample function will pick at random in the
       # interval 1:x instead of picking x itself
       
       if (length(potential.buyers[V(g)$buy.price[potential.buyers] == max.price])==1){
         buyer <- potential.buyers[V(g)$buy.price[potential.buyers] == max.price]
       }
      else {
        buyer <- sample(
         potential.buyers[V(g)$buy.price[potential.buyers] == max.price],1)
         }
     
     # pay for the security
     
     V(g)$money[[buyer]] <- V(g)$money[[buyer]] - max.price
     V(g)$money[[i]] <- V(g)$money[[i]] + max.price
     
     # transfer the security
     
     V(g)$secu[[buyer]][V(g)$sell.which[i]] <- V(g)$secu[[buyer]][V(g)$sell.which[i]] + 1
     V(g)$secu[[i]][V(g)$sell.which[i]] <- V(g)$secu[[i]][V(g)$sell.which[i]] - 1
     
     # remove orders
     
     V(g)$sell.order[[i]] <- 0
     V(g)$buy.order[[buyer]] <- 0
     
     }
    }
# Reset buying and selling choices

V(g)$buy.which <- rep(0,n.traders)
V(g)$sell.which <- rep(0,n.traders)
V(g)$buy.price <- rep(0,n.traders)
V(g)$sell.price <- rep(0,n.traders)
 
    

  }

return(g)

}
  
