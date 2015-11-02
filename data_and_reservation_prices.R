source("timeseries/prepare_data.R",chdir=T)
source("timeseries/climate_model.R",chdir=T)

DataPrediction <- function(
  g,
  scenario = c('rcp 2.6', 'rcp2.6', 'rcp26',
               'rcp 4.5', 'rcp4.5', 'rcp45',
               'rcp 6.0', 'rcp6.0', 'rcp60', 'rcp 6', 'rcp6', 
               'rcp 8.5', 'rcp8.5', 'rcp85'),
  true.model, time.span = NA, historical.temp = c('past', 'all', 'none')
){
  
  scenario <- match.arg(scenario)
  historical.temp <- match.arg(historical.temp)
  
  #####
  ## Useful variables
  #####
  
  n.periods <- g$burn.in + (g$n.seq * g$horizon)
  n.seq     <- g$n.seq
  n.secu    <- length((V(g)$secu)[[1]])
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
  
  if (is.na(time.span)) history_start <- nrow(climate_data)
  else history_start <- time.span # This must be compatible with the number of available 
  # historical data points
  future_length = max(0, n.periods - burn.in)
  
  sim.start <- switch(historical.temp,
                      'none' = 0,
                      'past' = burn.in,
                      'all' = nrow(climate_data)
                      )
  
  ### Initialize the true models

  if (true.model == 1){  
    true_covars = list('slow.tsi')
  }
  else if (true.model == 2){
    true_covars = list('log.co2')
  } else {
    stop("'true.model' in data_and_reservation_prices() must be either 1 or 2 ")
  }
  message("Initializing Model: n_history = ", burn.in, ", n_future = ", future_length,
          ", true covars = ", true_covars[[1]])
  mdl <- init_model(mdl, n_history = burn.in,
                    n_future = future_length, true_covars = true_covars,
                    future_covars = future_data, max_p = 1, max_q = 1)
  
  #####
  ## Construct reservation prices from predictions
  #####
  
  ### Initialize reservation prices data frames
  
  reserv.tsi = data.frame(matrix(NA, nrow = n.periods, ncol = n.secu ))
  reserv.co2 = data.frame(matrix(NA, nrow = n.periods, ncol = n.secu ))
  
  ### generate temperature intervals

  secu.intervals <- seq(min(mdl@future['t.anom']), max(mdl@future['t.anom']), 
                        length.out = n.secu)
  
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
      # open interval for lower security
      reserv.tsi[today,] <- bin_prob(trader.tsi, n_horizon = trader_horizon, 
                                     intervals = secu.intervals)
      reserv.co2[today,] <- bin_prob(trader.co2, n_horizon = trader_horizon, 
                                     intervals = secu.intervals)
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