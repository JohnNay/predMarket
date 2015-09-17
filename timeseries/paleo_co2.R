library(dplyr)

epica_co2_url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc-co2-2008.txt'
epica_temp_url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc3deuttemp2007.txt'
law_co2_url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/law/law2006.txt'

epica_co2_file <- basename(epica_co2_url)
epica_temp_file <- basename(epica_temp_url)
law_co2_file <- basename(law_co2_url)

get_epica_co2_data <- function(data.file = epica_co2_file, data.dir = 'data',
                           epica_url = epica_co2_url, quiet = TRUE) {
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, data.file)
  }
  download.file(epica_url, destfile = data.file, quiet = quiet, mode = 'wb')
}

get_epica_temp_data <- function(data.file = epica_temp_file, data.dir = 'data', 
                               epica_url = epica_temp_url, quiet = TRUE) {
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, data.file)
  }
  download.file(epica_url, destfile = data.file, quiet = quiet, mode = 'wb')
}

get_epica_data <- function(data.dir = 'data', quiet = TRUE) {
  get_epica_co2_data(data.dir = data.dir, quiet = quiet)
  get_epica_temp_data(data.dir = data.dir, quiet = quiet)
}

get_law_dome_data <- function(data.file = law_co2_file, data.dir = 'data', 
                              law_url = law_co2_url, quiet = TRUE) {
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, data.file)
  }
  download.file(law_url, destfile = data.file, quiet = quiet, mode = 'wb')
}

load_epica_co2 <- function(data.file = epica_co2_file, data.dir = 'data') {
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, data.file)
  }
  #
  # EPICA-C file has three sets of records. 
  # Skip to the composite record (EPICA + Vostok)
  #
  co2 <- read.table(data.file,header=F, skip=774, blank.lines.skip=T, 
                    col.names=c('age','co2'), stringsAsFactors = FALSE)
  co2$source <- 'Ice Core'
  last_epica <- head(co2,1)
  co2 <- rbind(data.frame(source = 'Cape Grim', age=c(last_epica$age, 0),
                          co2=c(last_epica$co2, 396.7)), co2)
  co2$source <- factor(co2$source)
  invisible(co2)
} 

load_epica_temp <- function(data.file = epica_temp_file, data.dir = 'data') {
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, data.file)
  }
  temp <- read.fwf(data.file, widths=c(5,13,17,13,13),
                   header=F, skip=104, 
                   blank.lines.skip=T, col.names=c('bag','ztop','age','d2','value'))
  temp <- temp[,c('age','value')]
  temp <- rbind(temp,c(age=0,value=0))
  names(temp) <- c('age', 'temp')
  invisible(temp)
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


