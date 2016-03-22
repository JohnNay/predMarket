climate_plot <- function(){
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  # to generate this data: source("timeseries/climate_plot.R", chdir = TRUE)
  load("timeseries/covar_models.Rda")
  t <- mdl.co2@climate %>% select(year, t.anom) %>% mutate(covar = "Historical")
  change.year <- max(t$year)
  co2 <- mdl.co2@future %>% select(year, t.anom) %>% filter(year >= change.year) %>%
    mutate(covar = "CO2")
  tsi <- mdl.tsi@future %>% select(year, t.anom) %>% filter(year >= change.year) %>%
    mutate(covar = "TSI")
  t %>% bind_rows(co2, tsi) %>% 
    mutate(covar = ordered(covar, levels = c("CO2", "TSI", "Historical"))) %>%
    ggplot(aes(x = year, y = t.anom, color = covar)) + 
    geom_line(size=1) + 
    scale_color_brewer(palette = "Set1", labels = c(CO2 = expression(CO[2]), TSI = "TSI"), 
                       name = "True driver of future climate") + 
    labs(x = "Year", y = expression(paste("Temperature Anomaly  ", (degree * C)))) +
    theme(legend.position = c(0.04,0.95), legend.justification = c(0,1), 
          legend.key.width = grid::unit(0.05,'npc'), 
          legend.key = element_rect(color = NA))
}