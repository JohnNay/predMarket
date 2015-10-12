library(dplyr)
source('load_giss.R')

t <- load_giss_data()$data %>% group_by(year) %>% summarize(t.anom = mean(t.anom)) %>% ungroup() %>% filter(!is.na(t.anom)) %>% mutate(year = year + 0.5)
co2 <- readRDS('data/smoothed_co2.Rds')


sunspots <- read.csv('data/SN_ms_tot_V2.0.csv', sep=';', header=F)
names(sunspots) <- c('year','month','frac.year','ss','sd','nobs','def')
sunspots <- sunspots %>% mutate(ss = ifelse(ss >= 0, ss, NA)) %>% group_by(year) %>% summarize(ss = mean(ss)) %>% ungroup() %>% filter(! is.na(ss)) %>% mutate(year = year + 0.5)

tsi <- tsi <- read.table('data/TSI_TIM_Reconstruction.txt', comment.char = ';')
names(tsi) <- c('year','tsi')
tsi$xtsi <- tsi$tsi
for(i in seq_along(tsi)[-1]) tsi$xtsi[i] <- 0.05 * tsi$tsi[i] + 0.95 * tsi$xtsi[i-1]

data <- merge(t, co2, all.x = TRUE)
data <- merge(data, sunspots, all.x = TRUE)
data <- merge(data, tsi, all.x = TRUE)
data <- data %>% select(year, t.anom, co2, sunspots = ss, tsi = xtsi) %>% mutate(year = floor(year))
data <- na.omit(data)

