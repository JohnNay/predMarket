#
#
#

library(dplyr)

law_co2_url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/law/law2006.txt'
law_co2_file <- basename(law_co2_url)

grim_url <- 'http://www.csiro.au/greenhouse-gases/GreenhouseGas/data/CapeGrim_CO2_data_download.csv'
grim_file <- basename(grim_url)

get_law_dome_data <- function(data.file = law_co2_file, data.dir = 'data', 
                              law_url = law_co2_url, quiet = TRUE) {
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, data.file)
  }
  download.file(law_url, destfile = data.file, quiet = quiet, mode = 'wb')
}

get_grim <- function(data.file = grim_file, data.dir = 'data', quiet = TRUE) {
  download.file(grim_url, destfile = file.path(data.dir, grim_file), quiet = quiet, mode = 'wb')
}

load_law_dome_co2 <- function(data.file = law_co2_file, data.dir = 'data') {
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, data.file)
  }
  
  co2 <- read.fwf(data.file,widths = c(13,11,10), 
                  skip=2464, nrow=(2715-2464), 
                  header=F, stringsAsFactors = F, 
                  blank.lines.skip=T)
  colnames(co2) <- c('source', 'year','co2')
  co2$source <- trimws(co2$source, 'both')
  co2 <- rbind(data.frame(source = 'CAPE GRIM', year = 2015, co2 = 396.7), co2)
  co2 <- na.omit(co2)
  co2$source <- ifelse(co2$source == 'CAPE GRIM', 'Cape Grim','Ice Core')
  co2$source <- factor(co2$source)
  invisible(co2)
}

build_smoothed_co2 <- function(update=FALSE) {
  if (update) {
    get_grim()
    get_law_dome_data()
  }
  co2 <- load_law_dome_co2()
  grim <- read.csv('data/CapeGrim_CO2_data_download.csv', header=T, skip=24, stringsAsFactors = F)
  grim <- na.omit(grim)
  grim <- grim %>% group_by(YYYY) %>% summarize(co2 = mean(CO2.ppm.)) %>% ungroup()
  names(grim)[1] <- 'year'
  grim$year <- as.numeric(grim$year)
  grim$source <- 'Cape Grim'
  grim <- grim[,names(co2)]
  
  x <- co2 %>% filter(year >= 1900, source == 'Ice Core')
  x <- rbind(x, grim)
  
  x$source <- factor(x$source, levels = c(levels(x$source), 'smooth'))
  
  y <- loess(co2 ~ year, x, span = 0.2)
  x <- rbind(x, data.frame(source = 'smooth', year = x$year, co2 = y$fitted))
  
  library(ggplot2)
  ggplot(x , aes(x = year, y = co2, color = source)) + geom_line() + geom_point() + theme_bw()
  
  
  co2 <- x %>% filter(source == 'smooth') %>% select(year, co2) %>% arrange(year)
  years <- with(co2, seq(floor(min(year)), floor(max(year)))) + 0.5
  smoothed_co2 <- as.data.frame(spline(co2, xout=years)) %>% na.omit()
  names(smoothed_co2) <- c('year', 'co2')
  
  saveRDS(smoothed_co2, file = 'data/smoothed_co2.Rds')
}
