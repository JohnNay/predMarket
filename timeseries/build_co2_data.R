#
#
#

source('paleo_co2.R')

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

