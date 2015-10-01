get_grim <- function() {
  grim_url <- 'http://www.csiro.au/greenhouse-gases/GreenhouseGas/data/CapeGrim_CO2_data_download.txt'
  download.file(grim_url, file.path('data', basename(grim_url)), mode = 'wb')
}

load_grim <- function() {
  
}