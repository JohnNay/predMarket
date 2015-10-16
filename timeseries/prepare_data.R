library(dplyr)
source('load_giss.R')

data.dir <- 'data'

prepare_climate_data <- function(scenario, modeled_tsi = TRUE) {
  t <- load_giss_data()$data %>% group_by(year) %>% summarize(t.anom = mean(t.anom)) %>% ungroup() %>% filter(!is.na(t.anom))
  scenario <- tolower(scenario)
  co2_basename = '_MIDYR_CONC.DAT'
  if (scenario %in% c('rcp 2.6', 'rcp2.6', 'rcp26')) {
    data_file = paste0('RCP3PD', co2_basename)
  } else if (scenario %in% c('rcp 4.5', 'rcp4.5', 'rcp45')) {
    data_file = paste0('RCP45', co2_basename)
  } else if (scenario %in% c('rcp 6.0', 'rcp6.0', 'rcp60', 'rcp 6', 'rcp6')) {
    data_file = paste0('RCP6', co2_basename)
  } else if (scenario %in% c('rcp 8.5', 'rcp8.5', 'rcp85')) {
    data_file = paste0('RCP85', co2_basename)
  }

  co2 <- read.table(file.path(data.dir, data_file), skip=38, stringsAsFactors=F, header=T)
  co2 <- co2 %>% select(year = YEARS, co2 = CO2) %>% mutate(log.co2 = log(co2))

  sunspots <- read.csv('data/SN_ms_tot_V2.0.csv', sep=';', header=F)
  names(sunspots) <- c('year','month','frac.year','ss','sd','nobs','def')
  sunspots <- sunspots %>% mutate(ss = ifelse(ss >= 0, ss, NA)) %>% 
    group_by(year) %>% summarize(ss = mean(ss)) %>% ungroup() %>% 
    filter(! is.na(ss))

  if (modeled_tsi) {
    tsi <- read.csv('data/tsi_prediction.csv', header=F, stringsAsFactors = F,
                    col.names = c('year', 'tsi'))
  } else {
    tsi <- read.table('data/TSI_TIM_Reconstruction.txt', comment.char = ';')
    names(tsi) <- c('year','tsi')
    tsi <- tsi %>% mutate(year = floor(year))
    tsi <- rbind(tsi, with(tsi, data.frame(year = 1:100 + max(year), 
                                           tsi = rev(tsi)[1:100])))
  }
  
  welch <- function(N) {
    w = 1 - (N:0 * 2 / (2 * N - 1) - 1)^2
    w/sum(w)
  }

  tsi <- tsi %>% mutate(slow.tsi = stats::filter(tsi, welch(44), 'convolution', 
                                          sides=1, circular = FALSE))

  data <- merge(t, co2, all.x = TRUE)
  data <- merge(data, sunspots, all.x = TRUE)
  data <- merge(data, tsi, all.x = TRUE)
  data <- data %>%  select(year, t.anom, co2, log.co2 = log.co2, sunspots = ss, 
                           tsi = tsi, slow.tsi = slow.tsi)
  
  data <- na.omit(data)
  
  future <- merge(co2, tsi) %>% filter(year > max(data$year))
  
  invisible(list(data = data, future = future))
}
