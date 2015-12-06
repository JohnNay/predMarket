library(ggplot2)
library(dplyr)
library(tidyr)

source('prepare_data.R')
source('climate_model.R')

TRACE_CLIMATE_MODEL <- TRUE
PARALLEL_STAN <- TRUE
WHICH_MODEL <- 'ar1'

theme_set(theme_bw(base_size=20))

data <- prepare_climate_data('rcp 8.5')
climate_data <- data$data
future_data <- data$future

today <- nrow(climate_data)
n_future <- 2100 - max(climate_data$year)

if (! all(exists('mdl.co2'), exists('mdl.tsi')) ) {
  mdl.co2 <- new('climate_model', climate = climate_data)
  mdl.co2 <- init_model(mdl.co2, n_history = today, n_future = n_future, true_covar = 'log.co2', future_covars = future_data)
  mdl.tsi <- init_model(mdl.co2, n_history = today, n_future = n_future, true_covar = 'slow.tsi', future_covars = future_data)
  
  save(mdl.co2, mdl.tsi, file = "covar_models.Rda")
}

t <- mdl.co2@climate %>% select(year, t.anom) %>% mutate(covar = "Historical")
change.year <- max(t$year)
co2 <- mdl.co2@future %>% select(year, t.anom) %>% filter(year >= change.year) %>%
  mutate(covar = "CO2")
tsi <- mdl.tsi@future %>% select(year, t.anom) %>% filter(year >= change.year) %>%
  mutate(covar = "TSI")
t <- t %>% bind_rows(co2, tsi) %>% 
  mutate(covar = ordered(covar, levels = c("CO2", "TSI", "Historical")))

p1 <- ggplot(t, aes(x = year, y = t.anom, color = covar)) + 
  geom_line(size=1) + 
  scale_color_brewer(palette = "Set1", labels = c(CO2 = expression(CO[2]), TSI = "TSI"), 
                     name = "True driver of future climate") + 
  labs(x = "Year", y = expression(paste("Temperature Anomaly  ", (degree * C))), 
       title = "Temperature futures in two alternate realities") +
  theme(legend.position = c(0.15,0.95), legend.justification = c(0,1), 
        legend.key.width = grid::unit(0.05,'npc'), 
        legend.key = element_rect(color = NA))


print(p1)

pdf("jg_climate.pdf", width=8, height=8)
print(p1)
dev.off()


x <- mdl.co2@future %>% select(Year = year, CO2 = co2, TSI = tsi) %>% 
  gather(key = covar, value = value, -Year) %>%
  mutate(covar = factor(covar, levels = c('CO2', 'TSI'), labels = c("CO2 (ppm)", "TSI (W/m^2)")))

p2 <- ggplot(x, aes(x = Year, y = value, color = covar)) + 
  geom_line(size=1) + 
  scale_color_brewer(palette='Set1', guide = 'none') +
  labs(y  = NULL) +
  facet_wrap(~covar, nrow=2, scale='free')

print(p2)

pdf("jg_covar.pdf", width=8, height=8)
print(p2)
dev.off()
