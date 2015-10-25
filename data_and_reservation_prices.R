source("timeseries/prepare_data.R",chdir=T)
source("timeseries/climate_model.R",chdir=T)

DataPrediction <- function(
  g,
  scenario = c('rcp 2.6', 'rcp2.6', 'rcp26',
               'rcp 4.5', 'rcp4.5', 'rcp45',
               'rcp 6.0', 'rcp6.0', 'rcp60', 'rcp 6', 'rcp6', 
               'rcp 8.5', 'rcp8.5', 'rcp85'),
  true.model
){
  
  scenario <- match.arg(scenario)
  
  #####
  ## Useful variables
  #####
  
  n.periods <- g$burn.in + (g$n.seq * g$horizon)
  n.seq     <- g$n.seq
  n.secu    <- length((V(g)$secu)[[1]]) + 2 # (+2) accounts for the 2 boundary
  # securities covering temperatures smaller than a certain low temperature,
  # and higher than a certain high temperature.
  horizon   <- g$horizon
  burn.in   <- g$burn.in
  
  #####
  ## Generate data
  #####
  
  data <- prepare_climate_data(scenario)
  
  climate_data <- data$data
  future_data <- data$future
  
  ### Load data and create model
  
  mdl <- new("climate_model", climate = climate_data)
  
  ### Set timing parameters
  
  history_start = 135 # This must be compatible with the number of available 
  # historical data points
  future_length = n.periods - history_start
  
  ### Initialize the true models

  if (true.model == 1){  
    true_covars = list('slow.tsi')
  }
  else if (true.model == 2){
    true_covars = list('log.co2')
  }
  else {
    stop("'true.model' in data_and_reservation_prices() must be either 1 or 2 ")
  }
  message("Initializing Model: n_history = ", history_start, ", n_future = ", future_length,
          ", true covars = ", true_covars[[1]])
  mdl <- init_model(mdl, n_history = history_start,
                    n_future = future_length, true_covars = true_covars,
                    future_covars = future_data, max_p = 1, max_q = 1)
  
  #####
  ## Construct reservation prices from predictions
  #####
  
  ### Initialize reservation prices data frames
  
  reserv.tsi = data.frame(matrix(NA, nrow = n.periods, ncol = n.secu + 1))
  reserv.co2 = data.frame(matrix(NA, nrow = n.periods, ncol = n.secu + 1))
  # the additional security (+1) is a void security. When a trader tries to
  # sell a unit of the void security, it is interpreted as the seller not
  # selling anything in this period
  
  ### generate temperature intervals
  
  secu.intervals <- matrix(NA,nrow = 2, ncol = n.secu)
  # open interval for lower security
  secu.intervals[2,1] <- 1.001 * min(mdl@climate['t.anom'])
  # open interval for upper security
  secu.intervals[1,n.secu] <- 0.999 * max(mdl@climate['t.anom']) 
  # intermediate intervals
  size.intervals = (secu.intervals[1,n.secu] - secu.intervals[2,1])/(n.secu -2)
  for (i in 2:(n.secu-1)){
    secu.intervals[1,i] <- secu.intervals[2,1] + (i-2)*size.intervals
    secu.intervals[2,i] <- secu.intervals[2,1] + (i-1)*size.intervals
  }
  
  # For every sequence, every period in a sequence
  # and for both models,record reservation price
  # for each security at the end of the trading sequence
  
  for (j in 1:n.seq){
    for(per in 0:(horizon-1)){
      # So at the beginning of the first sequence, 
      # "today" represents the last year of the burn-in record and
      # the trader_horizon represents horizon years after the burn-in period.
      #
      # Thus, if burn-in represents the historical record and the market
      # is looking into the future, then the sequence begins with today 
      # being the last year of the historical record and trader_horizon
      # being horizon years in the future.
      today = burn.in + (j-1)*horizon + per
      trader_horizon = horizon - per
      ### Update models
      # trader model = log.co2
      trader.co2 <- update_model(mdl, n_today = today,
                                      n_horizon = trader_horizon,
                                      trader_covars = list('log.co2'),
                                 max_p = 1, max_q = 1)
      # trader model = Slow TSI
      trader.tsi <- update_model(mdl, n_today = today,
                                      n_horizon = trader_horizon,
                                      trader_covars = list('slow.tsi'),
                                 max_p = 1, max_q = 1)
    
      ### Record reservation prices
      ## trader model = Slow TSI
      # open interval for lower security
      reserv.tsi[today,1] <- interval_prob(trader.tsi, n_horizon = trader_horizon,
                                                t.range = c(secu.intervals[1,1],secu.intervals[2,1]),
                                                closed = FALSE)
      # open interval for upper security
      reserv.tsi[today,n.secu] <- interval_prob(trader.tsi, n_horizon = trader_horizon,
                                           t.range = c(secu.intervals[1,n.secu],secu.intervals[2,n.secu]),
                                           closed = FALSE )
      # intermediate intervals
      for (i in 2:(n.secu-1)){
        reserv.tsi[today,i] <- interval_prob(trader.tsi, n_horizon = trader_horizon,
                                             t.range = c(secu.intervals[1,i],secu.intervals[2,i]),
                                             closed = TRUE )
      }
      ## trader model = log.co2
      # open interval for lower security
      reserv.co2[today,1] <- interval_prob(trader.co2, n_horizon = trader_horizon,
                                           t.range = c(secu.intervals[1,n.secu],secu.intervals[2,n.secu]),
                                           closed = FALSE )
      # open interval for upper security
      reserv.co2[today,n.secu] <- interval_prob(trader.co2, n_horizon = trader_horizon,
                                                t.range = c(secu.intervals[1,n.secu],secu.intervals[2,n.secu]),
                                                closed = FALSE )
      # intermediate intervals
      for (i in 2:(n.secu-1)){
        reserv.co2[today,i] <- interval_prob(trader.co2, n_horizon = trader_horizon,
                                             t.range = c(secu.intervals[1,i],secu.intervals[2,i]),
                                             closed = TRUE )
      }
    
    }
  }
  
  #####
  ## Store reservation prices and data in network
  #####
  
  g <- set.graph.attribute(g,"reserv.tsi",reserv.tsi)
  g <- set.graph.attribute(g,"reserv.co2",reserv.co2)
  g <- set.graph.attribute(g,"t.anom",mdl@climate['t.anom'])
  g <- set.graph.attribute(g,"secu.inter",secu.intervals)
  
  #####
  ## Return the network
  #####
  
  g
  
}