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

ggplot(x , aes(x = year, y = co2, color = source)) + geom_line() + geom_point() + theme_bw()

