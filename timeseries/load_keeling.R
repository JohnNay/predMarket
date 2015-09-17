############## RClimate Script: Mauna Loa Monthly CO2    ###################################
## Script stored on http://chartsgraphs.wordpress.com account for Users to source()       ##
## Download and process Monthly CO2 Data File                                             ##
## Developed by D Kelly O'Day to demonstrate use of source() function for climate data    ##
##                   http:chartsgraphs.wordpress.com    1/16/10                           ##
############################################################################################
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)

source('download_keeling_data.R')

month.names <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

get.co2 <- function(file = keeling_filename) {
  co2_data <- read.table(file,row.names=NULL, header=T)
  
  df <- co2_data %>% select(year = frac.year, monthly = co2.interpolated, 
                            annual = co2.interpolated.adj)
  
  monthly.cycle <- co2_data %>% group_by(month) %>% 
    summarize(co2 = mean(co2.interpolated - co2.interpolated.adj, na.rm=T))
  monthly.cycle <- rbind(monthly.cycle, monthly.cycle[1,])
  monthly.cycle$month[13] <- 13
  
  invisible(list(keeling = df, seasonal = monthly.cycle, 
                 up.to.date = tail(na.omit(co2_data),1) %>% select(year, month)))
}

