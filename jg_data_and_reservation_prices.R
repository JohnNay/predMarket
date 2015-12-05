source("timeseries/prepare_data.R",chdir=T)
source("timeseries/climate_model.R",chdir=T)

SHOW_CLIMATE_PLOTS <- FALSE
TRACE_CLIMATE_MODEL <- FALSE
WHICH_MODEL <- 'ar1'

max_p <- 1
max_q <- 0

DataPrediction <- function(
  g,
  scenario = c('rcp 2.6', 'rcp2.6', 'rcp26',
               'rcp 4.5', 'rcp4.5', 'rcp45',
               'rcp 6.0', 'rcp6.0', 'rcp60', 'rcp 6', 'rcp6', 
               'rcp 8.5', 'rcp8.5', 'rcp85'),
  true.model, 
  load_previous = FALSE,
  load_previous_fp_co2 = "jg_climatedataco2.Rda",
  load_previous_fp_tsi = "jg_climatedatatsi.Rda",
  saving = FALSE,
  saving_fp = "",
  init_with_obs_record = TRUE
){
  
  if (true.model == 1){
    true_covar = 'slow.tsi'
  } else if (true.model == 2){
    true_covar = 'log.co2'
  } else {
    stop("'true.model' in data_and_reservation_prices() must be either 1 or 2 ")
  }
  
  if(load_previous){
    if(true_covar == 'slow.tsi'){
      if(!file.exists(load_previous_fp_tsi)){
        message("No file there so we cannot load one, we will generate data instead.")
        load_previous <- FALSE
      }
    }
    
    if(true_covar == 'log.co2'){
      if(!file.exists(load_previous_fp_co2)){
        message("No file there so we cannot load one, we will generate data instead.")
        load_previous <- FALSE
      }
    }
  }
  
  #####
  ## Useful variables
  #####
  
  n.periods <- g$burn.in + (g$n.seq * g$horizon)
  n.seq     <- g$n.seq
  n.secu    <- length((V(g)$secu)[[1]])
  horizon   <- g$horizon
  burn.in   <- g$burn.in
  
  if(load_previous){
    if(saving) message("You wanted saving but we wont be since we are loading in an already saved climate data set.")
    
    if(true_covar == 'log.co2'){
      load(load_previous_fp_co2)
      # file loaded needs to be this:
      #       climatedata <- list(reserv.tsi = reserv.tsi,
      #                           reserv.co2 = reserv.co2,
      #                           mdl = mdl,
      #                           secu.intervals = secu.intervals)
      reserv.tsi <- climatedataco2$reserv.tsi
      reserv.co2 <- climatedataco2$reserv.co2
      reserv.best <- climatedataco2$reserv.best
      mdl <- climatedataco2$mdl
      secu.intervals <- climatedataco2$secu.intervals
    }
    
    if(true_covar == 'slow.tsi'){
      load(load_previous_fp_tsi)
      # file loaded needs to be this:
      #       climatedata <- list(reserv.tsi = reserv.tsi,
      #                           reserv.co2 = reserv.co2,
      #                           mdl = mdl,
      #                           secu.intervals = secu.intervals)
      reserv.tsi <- climatedatatsi$reserv.tsi
      reserv.co2 <- climatedatatsi$reserv.co2
      reserv.best <- climatedatatsi$reserv.best
      mdl <- climatedatatsi$mdl
      secu.intervals <- climatedatatsi$secu.intervals
    }
  } else {
    scenario <- match.arg(scenario)
    #####
    ## Generate data
    #####
    
    data <- prepare_climate_data(scenario)
    
    climate_data <- data$data
    future_data <- data$future
    
    ### Load data and create model
    
    mdl <- new("climate_model", climate = climate_data)
    
    ### Set timing parameters
    
    #   if (is.na(time.span)) {
    #     history_start <- nrow(climate_data)
    #   } else history_start <- time.span # This must be compatible with the number of available 
    #   # historical data points
    future_length = max(0, n.periods - burn.in)
    
    ### Initialize the true models
    if (init_with_obs_record) {
      n_history_init <- nrow(mdl@climate)
      n_future_init <- (burn.in + future_length) - n_history_init
    } else {
      n_history_init <- burn.in
      n_future_init <- future_length
    }
    
    message("Initializing Model: n_history = ", n_history_init, ", n_future = ", n_future_init,
            ", true covars = ", true_covar)
    mdl <- init_model(mdl, n_history = n_history_init,
                      n_future = n_future_init, true_covar = true_covar,
                      future_covars = future_data,
                      max_p = max_p, max_q = max_q)
    
    #####
    ## Construct reservation prices from predictions
    #####
    
    ### Initialize reservation prices data frames
    
    reserv.tsi = data.frame(matrix(NA, nrow = n.periods, ncol = n.secu))
    reserv.co2 = data.frame(matrix(NA, nrow = n.periods, ncol = n.secu))
    
    ### Initialize best reservation prices
    
    reserv.best = data.frame(matrix(NA, nrow = n.periods, ncol = n.secu))
    
    ### generate temperature intervals
    
    secu.intervals <- seq(min(mdl@future$t.anom), max(mdl@future$t.anom), 
                          length.out = n.secu - 1)
    
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
                                   trader_covar = 'log.co2',
                                   auto_arma = TRUE,
                                   max_p = max_p, max_q = max_q)
        if (SHOW_CLIMATE_PLOTS)
          plot_model(trader.co2, trader.covar = 'log.co2')
        # trader model = Slow TSI
        trader.tsi <- update_model(mdl, n_today = today,
                                   n_horizon = trader_horizon,
                                   trader_covar = 'slow.tsi',
                                   auto_arma = TRUE,
                                   max_p = max_p, max_q = max_q)
        if (SHOW_CLIMATE_PLOTS)
          plot_model(trader.tsi, trader.covar = 'slow.tsi')
        
        ### Record reservation prices
        # open interval for lower security
        bp <- bin_prob(trader.tsi, n_horizon = trader_horizon, 
                       intervals = secu.intervals)
        if (length(bp) != length(reserv.tsi[today,])) 
          warning("Length mismatch: length(bp) = ", length(bp), ", 
                length(reserv.tsi[today,]) = ", length(reserv.tsi[today,]))
        reserv.tsi[today,] <- bp
        bp <- bin_prob(trader.co2, n_horizon = trader_horizon, 
                       intervals = secu.intervals)
        if (length(bp) != length(reserv.co2[today,])) 
          warning("Length mismatch: length(bp) = ", length(bp), ", 
                length(reserv.co2[today,]) = ", length(reserv.co2[today,]))
        reserv.co2[today,] <- bp
        
        ### Record "best" reservation price
        best <- findInterval(
          mdl@future$t.anom[today + trader_horizon],secu.intervals) + 1
        bp <- rep(0, n.secu)
        bp[best] <- 1
        reserv.best[today,] <- bp
      }
    }
    
    if(saving){
      if(true_covar == 'log.co2'){
        climatedataco2 <- list(reserv.tsi = reserv.tsi,
                            reserv.co2 = reserv.co2,
                            reserv.best = reserv.best,
                            mdl = mdl,
                            secu.intervals = secu.intervals)
        save(climatedataco2, file = paste0(saving_fp, "climatedataco2.Rda"))
      }
      
      if(true_covar == 'slow.tsi'){
        climatedatatsi <- list(reserv.tsi = reserv.tsi,
                               reserv.co2 = reserv.co2,
                               reserv.best = reserv.best,
                               mdl = mdl,
                               secu.intervals = secu.intervals)
        save(climatedatatsi, file = paste0(saving_fp, "climatedatatsi.Rda"))
      }
    }
  }
  
  #####
  ## Store reservation prices and data in network
  #####
  g <- set.graph.attribute(g,"reserv.tsi",reserv.tsi)
  g <- set.graph.attribute(g,"reserv.co2",reserv.co2)
  g <- set.graph.attribute(g,"reserv.best",reserv.best)
  if (FALSE) {
    if (anyNA(mdl@future$t.anom[burn.in:n.periods]))
      browser()
  }
  stopifnot( ! anyNA(mdl@future$t.anom[burn.in:n.periods]))
  g <- set.graph.attribute(g,"t.anom",mdl@future$t.anom)
  g <- set.graph.attribute(g,"secu.inter",secu.intervals)
  
  #####
  ## Return the network
  #####
  g
}
