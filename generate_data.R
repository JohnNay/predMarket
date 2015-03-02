generate_data <- function (
  h = 200,  # time horizon at which securities are realized
  ### Parameters for construction of other time series 
  #NOTE : these are from Sumner and Jackson' calibration, but make no sense
  # without real time series
  lambda = 0.9845,
  beta = 0.00017,
  omega = 0.36,
  theta = 0.0286,
  rho = 0.0125,
  kappa = 0.993,
  gamma = 0.15,
  delta = 2
){
  ##########
  ### Fundamental explanatory variables (arbitrary at this point)
  ##########
  
  #Geoingeneering expenditures
  
  X <- (1:h)/10 + diffinv(rnorm(h-1))
  
  #Policy expenditures
  
  P <- abs((1:h)/10 + diffinv(rnorm(h-1)))
  P <- P^(1/2)
  
  #GDP
  
  GDP <- (1.003)^(1:h) + diffinv(rnorm(h-1))/15
  
  ###########
  ###  Constructed explanatory varialbes
  ###########
  
  ### Construct "effect of geoingeneering" variable
  
  G <- 1:h
  
  # NOTE : G[t] will be constructed recursively, so we must choose an arbitrary 
  # initial value. Here it will be 1.
  
  for (t in 2:h){
    G[t] <- omega*G[t-1] + theta*X[t] + rnorm(1)
  }
  
  ### Construct Green-house gases
  
  GHG <- 1:h
  
  # NOTE : GHG[t] will be constructed recursively, so we must choose an arbitrary 
  # initial value. Here it will be 1.
  
  for (t in 2:h){
    GHG[t] <- kappa*GHG[t-1] - (rho*P[t])^(0.5) + delta*GDP[t]  + rnorm(1)
  }
  
  ###########
  ###  Constructed dependent variable (e.g. temperature)
  ###########
  
  # NOTE : T[t] will be constructed recursively, so we must choose an arbitrary 
  # initial value. Here it will be 1.
  
  temp <- 1:h
  
  for (t in 2:h){
    temp[t] <- lambda*temp[t-1] + beta*GHG[t] - rho*G[t] 
  }
  
  #######
  ### Construct laged time series for regression
  ######
  
  ### Construct LAGGED "effect of geoingeneering" 
  
  G.lag <- 1:h
  G.lag[1] <- NA
  
  for (t in 2:h){
    G.lag[t] <- G[t-1]
  }
  
  ### Construct LAGGED Green-house gases
  
  GHG.lag <- 1:h
  GHG.lag[1] <- NA
  
  for (t in 2:h){
    GHG.lag[t] <- GHG[t-1]
  }
  
  ### Construct LAGGED temperatures
  
  # NOtempE : temp[t] will be constructed recursively, so we must choose an arbitrary 
  # initial value. Here it will be 1.
  
  temp.lag <- 1:h
  temp.lag[1] <- NA
  
  for (t in 2:h){
    temp.lag[t] <- temp[t-1]
  }
  
  
  # Generate data frame
  
  D <- data.frame(temp,temp.lag,GHG,GHG.lag,G,G.lag, P, GDP, X)
  
  D
}
