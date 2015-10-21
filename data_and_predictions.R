DataPrediction <- function(
  g,
  scenario = c('rcp 2.6', 'rcp2.6', 'rcp26',
               'rcp 4.5', 'rcp4.5', 'rcp45',
               'rcp 6.0', 'rcp6.0', 'rcp60', 'rcp 6', 'rcp6', 
               'rcp 8.5', 'rcp8.5', 'rcp85'),
  true.model = c(1,2),
  n_history  = 115,
  n_future   = 0
){
  
  theme_set(theme_bw(base_size=20))
  
  source("timeseries/prepare_data.R",chdir=T)
  source("timeseries/climate_model.R",chdir=T)
  
  data <- prepare_climate_data(scenario)
  
  climate_data <- data$data
  future_covars <- data$future
  
  # Load data and create model
  
  mdl <- new("climate_model", climate = climate_data)
  
  # Set timing parameters
  
  history_start = n_history
  future_length = n_future
  
  # Initialize the true models

  mdl.co2 <- init_model(mdl, n_history = history_start,
                        n_future = future_length, true_covars = list('log.co2'),
                        future_covars = future_covars)
  
  mdl.tsi <- init_model(mdl, n_history = history_start,
                        n_future = future_length, true_covars = list('slow.tsi'),
                        future_covars = future_covars)

  mdl.co2
  
  mdl.tsi
  
  
}