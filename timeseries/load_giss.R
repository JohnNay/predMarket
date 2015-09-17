#
# GISS monthly
#
library(dplyr)
library(tidyr)
library(stringr)

source('download_giss_data.R')

add_averages <- function(monthly_data) {
  monthly_data <- monthly_data %>% mutate(t.anom.annual = NA, t.anom.decadal = NA)
  for (i in seq(5*12, nrow(monthly_data)- 5*12)) {
    monthly_data$t.anom.decadal[i] <- mean(monthly_data$t.anom[i + seq(1 - 5*12, 5*12)])
  }
  for (i in 6:(nrow(monthly_data)-6)) {
    monthly_data$t.anom.annual[i] <- mean(monthly_data$t.anom[i + (-5:6)])
  }
  invisible(monthly_data)
}

load_giss_data <- function(data_type = 'land.sea') {
  file <- giss_file[data_type]
  text_label <- giss_data.source.text[data_type]
  giss_file.skip <- 7
  giss_file.col_widths <- c(rep_len(5, 13), 7, 6, rep_len(5, 4), 6)
  
  giss_names <- c('year', 
                  'jan', 'feb', 'mar', 'apr', 'may', 'jun',
                  'jul', 'aug', 'sep', 'oct', 'nov', 'dec',
                  'jan.dec', 'dec.nov', 
                  'djf','mam','jja','son',
                  'year.2')

  giss_data <- read.fwf(file, giss_file.col_widths, header = FALSE,
                        skip = giss_file.skip, col.names = giss_names,
                        strip.white = TRUE, blank.lines.skip = TRUE,
                        stringsAsFactors = FALSE)
  monthly_data <- giss_data[,1:13] %>% filter(grepl('^[0-9]', year))
  monthly_data <- monthly_data %>% gather(month, t.anom, -year)
  monthly_data$year <- as.numeric(monthly_data$year)
  monthly_data$month <- ordered(monthly_data$month, levels = giss_names[2:13])
  monthly_data$t.anom[grepl('*', monthly_data$t.anom, fixed = TRUE)] <- NA
  monthly_data$t.anom <- as.numeric(monthly_data$t.anom) / 100.
  monthly_data$time <- monthly_data$year + (as.numeric(monthly_data$month) - 0.5)/12.
  monthly_data <- monthly_data %>% arrange(time)
  monthly_data <- monthly_data %>% filter(! is.na(t.anom))
  monthly_data <- add_averages(monthly_data)
  invisible(list(data = monthly_data, label = text_label))
}
