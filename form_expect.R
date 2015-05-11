
FormExpect <- function(
                        g,
                        ct,  # the current time period
                         D
                      )
  
  # In the preliminary model, agents simply calibrate their approximate model via pooled-OLS
  
{
  
  # package requirement
  require(igraph)
  
  ######
  ## Storing variable
  ######
  
  nonrandom <- rep(0,2)        # store the nonrandom component of traders' prediction
  random <- matrix(0,10000,2)  # store the random component of traders' prediction
  pred<- matrix(0, 10000, 2)   # store the predicted distribution of temperature 
  
  ######
  ## Make "left" the number of remaining periods until the end of the sequence
  ######
  
  if(ct<g$horizon){
    left <- g$horizon - (ct - g$burn.in)
  }
  
  if(ct > g$horizon){
    left <- g$horizon - ((ct - g$burn.in) %% 5)
  }
  
  ########                                          #######
  ### Model 1 : ACC is a myth, temp is autoregresive    ###
  ########                                          #######
  
  fit <- lm (temp ~ 0 + temp.lag, data = D[1:(ct-1), ])

  ### Expected value at the end of the trading sequence
  
  nonrandom[1] <- (coef(fit)[[1]])^(left)*D["temp"][ct-1,]
  
  ### Bootstrap past errors to determine random component of future temperature
  ### and construct recusive sum of error terms from bootstrap
  
  for (t.left in 1:left){
    random[,1] <- random[,1] +
      coef(fit)[[1]]^(left - t.left)*
      sample(as.vector(resid(fit)),10000, replace = TRUE)
  }
  
  ###  Predicted distribution
  
  pred[,1] <- rep(nonrandom[1],10000) + random[,1]
  
  ########                                          #######
  ### Model 2 : ACC is true and temp is function of GHG ###
  ########                                          #######
  
  # SEE MODEL 1 ABOVE FOR A DETAILED EXPLANATION OF THE MODEL ESTIMATION AND THE
  # PREDICTION PROCEDURE. The procedure is the same here except for the
  # approximate model
  
  fit <- lm (temp ~ 0 + GHG, data = D[1:ct-1,])
  
  ### Expected value at the end of the trading sequence
  
  nonrandom[2] <- (coef(fit)[[1]])*D["GHG"][ct + left,]
  
  ### Bootstrap past errors to determine random component of future temperature
  
  random[,2] <- sample(as.vector(resid(fit)),10000, replace = TRUE)
  
  ### Predicted distribution
  
  pred[,2] <- rep(nonrandom[2],10000) + random[,2]
  
  #######
  #### Record the matrix of distribution as a graph attribute 
  #######
  
  g <- set.graph.attribute(g,"pred",pred)  # can be obtained through command net$pred in model
  
  g

}