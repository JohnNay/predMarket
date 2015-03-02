
Interact.deb <- function (
  g     = net,
  i     = 1
){
  
    
    # Draw a random order in which traders will come to the market
      
      # Player i places a buy order on the order book
      V(g)$buy.order[i] <- 1
      
      # Player i places a sell order on the order book, if she owns 
      # a positive amount of some security
      
      if (V(g)$sell.which[i] !=0){
        V(g)$sell.order[i] <- 1
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
      
      g <- set.graph.attribute(g,"pot.sel",potential.sellers)
      
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
        
        g <- set.graph.attribute(g,"sel",seller)
        
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
      
  
  return(g)
  
}


