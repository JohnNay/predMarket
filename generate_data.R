### Temperatures

# SOURCE : http://www7.ncdc.noaa.gov/CDO/CDODivisionalSelect.jsp
# RETRIEVED : May 5th 2015

temp_emp <- scan("temp_clean.txt")

### CO2 concentration

# SOURCE : http://data.giss.nasa.gov/modelforce/ghgases/Fig1A.ext.txt
# RETRIVED : May 5th 2015

CO2_emp <- scan("CO2_clean.txt")

generate_data <- function (
  t.fin, # final time period  
  t.mod, # the true model, 1 for AAC is a myth, 2 for AAC is fction of GHG
  temp.emp = temp_emp,
  CO2.emp = CO2_emp
){
  
  ########
  ### Read empirical data
  ########
  
#   ### Temperatures
#   
#   # SOURCE : http://www7.ncdc.noaa.gov/CDO/CDODivisionalSelect.jsp
#   # RETRIEVED : May 5th 2015
#   
#   temp.emp <- scan("temp_clean.txt")
  
  # Useful variable
  
  n <- length(temp.emp)
  
  ### (empirical) Temperatures LAGGED
  
  temp.emp.lag <- append(NA, temp.emp[1:n-1])
  
  # record distribution of residuals around mean of (linearly) detrended temp
  
  temp.detrend.resid <- ( detrend(temp.emp, tt = 'linear', bp = c())
                          - mean(detrend(temp.emp, tt = 'linear', bp = c())) )
  
  ### Emprical data frame
  
  Emp.D <- data.frame(temp.emp, temp.emp.lag, CO2.emp)
  
  ##########
  ## Generate time series from true model based on calibration
  ##########
  
  # The calibration of the true model based on empirical temperatures is meant to
  # ensure that the simulated time series are somewhat realistic
  # Similarly, the choice of sample(temp.detrend.resid,1) to generate noise around
  # the trend is meant to guarantee that the simulated time series have roughly
  # the same kind of variation around the trend as in the empirical temperature time series.
  
  ### If the true model is the autoregressive one (no anthropogenic effect)
  
  if (t.mod == 1){
    
    fit <- lm (temp.emp ~ 0 + temp.emp.lag, data = Emp.D[2:n, ])
    
    temp <- 1:t.fin
    temp[1] <- temp.emp[1]
    
    for (t in 2:t.fin){
      temp[t] <- (coef(fit)[[1]])*temp[t-1] + sample(temp.detrend.resid,1)
    }
  }
  
  ### If the true model is a function of GHG  (anthropogenic)
  
  if (t.mod == 2){
    
    fit <- lm (temp.emp ~ 0 + CO2.emp, data = Emp.D[2:n, ])
    
    temp <- 1:t.fin
    
    for (t in 1:t.fin){
      temp[t] <- (coef(fit)[[1]])*CO2.emp[t] + sample(temp.detrend.resid,1)
    }
  }
  
  #### Generate temperature LAGGED
  
  temp.lag <- append(NA, temp[1:n-1])
  
  ### Identify GHG with CO2.emp
  
  GHG <- CO2.emp
  
  ######
  ### Generate data frame
  ######
  
  D <- data.frame(temp,temp.lag,GHG)
  D
}
