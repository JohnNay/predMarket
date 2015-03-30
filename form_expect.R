
FormExpect <- function(
                        g,
                        ct,  # the current time period
                         D
                      )
  
  # In the preliminary model, agents simply calibrate their approximate model via pooled-OLS
  
  # NOTE : In the preliminary model the data matrix takes the form D <- rbind(temp,GHG,G, P, GDP, X)
  
{
  
  # package requirement
  require(igraph)
  
  dis <- rep(1,5)    # a vector to store the predicted distributions for each approximate model
  lambd <- rep(1,5)  # a vector to stor the calibrated values for lambda which will be used to 
                     # generate the random component of player's prediction
  
  #######
  #### Calibrate approximate models and compute predicted mean 
  #######
  
  
    ### Model 1 : full model ###
  
   # SEE MODEL 2 BELOW FOR A DETAILED EXPLANATION OF THE MODEL ESTIMATION AND THE 
   # PREDICTION PROCEDURE. The procedure will be the same here, but it is more
   # easily explained in Model 2 because it is simpler
  
  
  fit <- lm (temp ~ 0 + temp.lag + GHG.lag + P + GDP + G.lag + X, data = D[1:(ct-1), ])
  
  # WARNING : signs of the coefficients are different than in Sumner and Jackson 
  # to accomodate the lm function
  # NOTE : The policy variable "P" is already in square root form by construction
  
  ## Initiate recursive prediction
  
  newdata <- D[ct,]
  pred.init <- predict(fit,newdata)[[1]]
  newdata <- D[ct+1, ]
  newdata["temp.lag"] <- pred.init
  
  ## Recursively predict future value
  
  from <- ct + 1
  to <- g$burn.in + g$horizon
  
  for (j in from:to){
    temp.pred <- predict(fit,newdata)[[1]]
    newdata <- D[j+1,]
    newdata["temp.lag"] <- temp.pred  
  }
  
  dis[1] <- temp.pred 
  lambd[1] <- coef(fit)[[1]]
  
  
   ### Model 2 : ACC is a myth ###
  
  # NOTE : at time (ct), one only knows past temperatures up to (ct-1), so calibration
  # can only use data up to ct-1, although explanatory variables at ct are known
  
  fit <- lm (temp ~ 0 + temp.lag, data = D[1:(ct-1), ])
  
  # Based on 
      # knowledge of T up to ct,
      # knowledge of all other covariates up to h,
      # estimation of the model in fit,
  # we now predict the expected value of temperature at horizon. This is done recursively
  
  ## Initiate recursive prediction
  
  # NOTE : the recursive procedure is initiated using the known true value
  # of temperature at (ct-1). After (ct-1), the traders must rely on
  # former PREDICTIONS in order to iterate up to the desired time horizon
  
  newdata <- D[ct,]
  pred.init <- predict(fit,newdata)[[1]]
  newdata <- D[ct+1, ]
  newdata["temp.lag"] <- pred.init # From now on, we cannot use the true values of 
      # T.lag to predict the next value because T.lag is unknown to the trader from
      # ct + 1 on. Instead, we must rely on predicted values. Therefore, we replace
      # T.lag[ct+1] by the predicted value in the new data that will be used for 
      # prediction at ct + 1

  ## Recursively predict future value (closed form can be computed if slows down model)
  
  from <- ct + 1
  to <- g$burn.in + g$horizon
  
  for (j in from:to){
    
    temp.pred <- predict(fit,newdata)[[1]]
    newdata <- D[j+1,]
    newdata["temp.lag"] <- temp.pred  # once again we alter the new vector of data that
        # that will be used for prediction in the next period to make sure that
        # that traders use predicted values of temperature and not the true values
        # which they do not know
  }
  
  dis[2] <- temp.pred  
  lambd[2] <- coef(fit)[[1]]
  
  
    ### Model 3 : ACC is true but measures are ineffective ###
  
  # SEE MODEL 2 ABOVE FOR A DETAILED EXPLANATION OF THE MODEL ESTIMATION AND THE 
  # PREDICTION PROCEDURE. The procedure will be the same here, but it is more
  # easily explained in Model 2 because it is simpler
  
  fit <- lm (temp ~ 0 + temp.lag + GHG.lag + GDP, data = D[1:ct-1,])
  
  ## Initiate recursive prediction
  
  newdata <- D[ct,]
  pred.init <- predict(fit,newdata)[[1]]
  newdata <- D[ct+1, ]
  newdata["temp.lag"] <- pred.init
  
  ## Recursively predict future value
  
  from <- ct + 1
  to <- g$burn.in + g$horizon
  
  for (j in from:to){
    temp.pred <- predict(fit,newdata)[[1]]
    newdata <- D[j+1,]
    newdata["temp.lag"] <- temp.pred  
  }
  
  dis[3] <- temp.pred
  lambd[3] <- coef(fit)[[1]]
  
  
  ### Model 4 : ACC is true policy are effective but geoingeneering is not ###
  
  # SEE MODEL 2 ABOVE FOR A DETAILED EXPLANATION OF THE MODEL ESTIMATION AND THE 
  # PREDICTION PROCEDURE. The procedure will be the same here, but it is more
  # easily explained in Model 2 because it is simpler
  
  fit <- lm (temp ~ 0 + temp.lag + GHG.lag + P + GDP, data = D[1:ct-1,])
  
  ## Initiate recursive prediction
  
  newdata <- D[ct,]
  pred.init <- predict(fit,newdata)[[1]]
  newdata <- D[ct+1, ]
  newdata["temp.lag"] <- pred.init
  
  ## Recursively predict future value
  
  from <- ct + 1
  to <- g$burn.in + g$horizon
  
  for (j in from:to){
    temp.pred <- predict(fit,newdata)[[1]]
    newdata <- D[j+1,]
    newdata["temp.lag"] <- temp.pred  
  }
  
  dis[4] <- temp.pred
  lambd[4] <- coef(fit)[[1]]
  
  
  ### Model 5 : ACC is true, geoingeneering is efficient, but policy is not ###
  
  # SEE MODEL 2 ABOVE FOR A DETAILED EXPLANATION OF THE MODEL ESTIMATION AND THE 
  # PREDICTION PROCEDURE. The procedure will be the same here, but it is more
  # easily explained in Model 2 because it is simpler
  
  fit <- lm (temp ~ 0 + temp.lag + GHG.lag + GDP + G.lag + X, data = D[1:ct-1,])
  
  ## Initiate recursive prediction
  
  newdata <- D[ct,]
  pred.init <- predict(fit,newdata)[[1]]
  newdata <- D[ct+1, ]
  newdata["temp.lag"] <- pred.init
  
  ## Recursively predict future value
  
  from <- ct + 1
  to <- g$burn.in + g$horizon
  
  for (j in from:to){
    temp.pred <- predict(fit,newdata)[[1]]
    newdata <- D[j+1,]
    newdata["temp.lag"] <- temp.pred  
  }
  
  dis[5] <- temp.pred
  lambd[5] <- coef(fit)[[1]]
  
  ########
  ### Turn predicted values into distributions
  #######
  
  err <- matrix(rnorm(mean = 0, sd = var(D[,"temp"])^(1/2)/100, n=10000))   # a reference
  # error term to compute prediction distribution. The variance needs to be somewhat
  # related to the true variance of temp, otherwise traders tend to be to inconfident
  # about their predictions and trade only extreme securities.
  er  <- kronecker(rbind((1-lambd^(500-ct))/(1-lambd)), err)
  mean <- t(replicate(10000,dis))
  pred <- er + mean
  
  #######
  #### Record the matrix of distribution as a graph attribute 
  #######
  
  g <- set.graph.attribute(g,"pred",pred)  # can be obtained through command net$pred in model
  
  g

}