Adapt <- function( g                  
){
  
  ######
  ## Useful variables
  ######
  
  n.secu    <- length((V(g)$secu)[[1]])
  n.traders <- length(V(g))
  #   n.approx  <- length(unique(V(g)$approx))
  
  ######
  ## Adapt approximate model
  ######
  
  # Draw random order of the traders
  
  turns <- sample(1:n.traders, n.traders, replace = FALSE)
  
  ### Adapt approximate model 
  
  for (i in turns){
    
    # If the trader has neighbours
    
    if (length(neighbors(g,i)>=1)){
      
      # Determine richest neighbor
      
      richest <- neighbors(g,i)[V(g)$money[neighbors(g,i)] == max(V(g)$money[neighbors(g,i)])]
      
      ## If there is more than one richest neighbour (unlikely but you never know)
      
      if (length(richest)>1){
        richest <- sample(richest,1)
        
        # if the most successful neighbor does better than i
        
        if (V(g)$money[richest] > V(g)$money[i]) {
          
          # With proba V(g)$ideo[i], adopt the model of the most successful neighbor
          
          if (sample(1:100,1) >= V(g)$ideo[i]*100){
            
            V(g)$approx[i] <- V(g)$approx[richest]
          }
        }
      }
      
      ## If there is only one richest neighbour 
      
      if (length(richest)==1){
        
        # if the most successful neighbor does better than i
        
        if (V(g)$money[richest] > V(g)$money[i]) {
          
          # With proba V(g)$ideo[i], adopt the model of the most successful neighbor
          
          if (sample(1:100,1) >= V(g)$ideo[i]*100){
            
            V(g)$approx[i] <- V(g)$approx[richest]
          }
        }
      }   
    }
  }
  
  g
}