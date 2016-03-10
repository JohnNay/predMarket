library(dplyr)
library(tidyr)
library(ggplot2)

future_line <- function(){
  
  load("output/convergence_future.Rda")
  
  plot_data <- average_convergence
  # plot_data$set <- factor(plot_data$set, levels = gtools::mixedsort(unique(plot_data$set)))
  #  n.edg <- round(parameters[5]*100) + 100 # integer in (100, 200)
  plot_data$set <- factor(plot_data$set, levels = c("0.05, 0.05", "0.05, 0.95", "0.95, 0.05", "0.95, 0.95"), 
                          labels = c(paste0(round(8 * 0.05 + 2, 1), " edges/trader, segmentation = 0.05"), 
                                     paste0(round(8 * 0.05 + 2, 1), " edges/trader, segmentation = 0.95"), 
                                     paste0(round(8 * 0.95 + 2, 1), " edges/trader, segmentation = 0.05"), 
                                     paste0(round(8 * 0.95 + 2, 1), " edges/trader, segmentation = 0.95")))
  
  sequence.years <- 6
  
  ggplot(data=plot_data, aes(x= trading_seq * sequence.years + 2014, y=(convergence + 1) * 50, color = true_mod)) +
    geom_point(position = position_jitter(w = 0.07 * sequence.years, h = 0), alpha = 0.1) + geom_smooth(size=1, fill=NA) +
    scale_color_brewer(palette="Set1", name="True driver of future climate", labels = c(LogCo2 = expression(CO[2]), SlowTSI = "TSI")) +
    facet_wrap(~set, ncol = 1) +
    labs(x = "Year", y = paste0("% Traders Believing True Model (n = ",
                                prettyNum(length(complete.cases(plot_data$convergence)), big.mark = ','), ")"),
         title = paste0("2015-",  2014 + 14 * 6)) +
    ylim(0,100) +
    theme(legend.justification=c(1,0), legend.position=c(1,0), 
          legend.key.width = grid::unit(0.05, 'npc'),
          legend.key = element_rect(color=NA))
}

future_box <- function(){
  load("output/convergence_future.Rda")
  
  plot_data <- average_convergence
  # plot_data$set <- factor(plot_data$set, levels = gtools::mixedsort(unique(plot_data$set)))
  #  n.edg <- round(parameters[5]*100) + 100 # integer in (100, 200)
  plot_data$set <- factor(plot_data$set, levels = c("0.05, 0.05", "0.05, 0.95", "0.95, 0.05", "0.95, 0.95"), 
                          labels = c(paste0(round(8 * 0.05 + 2, 1), " edges/trader, segmentation = 0.05"), 
                                     paste0(round(8 * 0.05 + 2, 1), " edges/trader, segmentation = 0.95"), 
                                     paste0(round(8 * 0.95 + 2, 1), " edges/trader, segmentation = 0.05"), 
                                     paste0(round(8 * 0.95 + 2, 1), " edges/trader, segmentation = 0.95")))
  
  sequence.years <- 6
  
  ggplot(data=plot_data, aes(x= trading_seq * sequence.years + 2014, y=(convergence + 1) * 50, color = true_mod)) +
    geom_boxplot(aes(group = floor(trading_seq * sequence.years + 2014 - 1.25 +  2.5 * (true_mod == "SlowTSI")), 
                     color = true_mod), position = position_dodge(width = 3), width = 2) + 
    geom_smooth(size=1, fill=NA) +
    scale_color_brewer(palette="Set1", name="True driver of future climate", labels = c(LogCo2 = expression(CO[2]), SlowTSI = "TSI")) +
    facet_wrap(~set, ncol = 1) +
    labs(x = "Year", y = paste0("% Traders Believing True Model (n = ",
                                prettyNum(length(complete.cases(plot_data$convergence)), big.mark = ','), ")"),
         title = paste0("2015-",  2014 + 14 * 6)) +
    ylim(0,100) +
    theme(legend.justification=c(1,0), legend.position=c(1,0),
          legend.key.width = grid::unit(0.05, 'npc'),
          legend.key = element_rect(color=NA))
}

history_box <- function(){
  load("output/jg_convergence_true_past.Rda")
  
  plot_data <- average_convergence %>% filter(true_mod == 'LogCo2')
  # plot_data$set <- factor(plot_data$set, levels = gtools::mixedsort(unique(plot_data$set)))
  #  n.edg <- round(parameters[5]*100) + 100 # integer in (100, 200)
  plot_data$set <- factor(plot_data$set, levels = c("0.05, 0.05", "0.05, 0.95", "0.95, 0.05", "0.95, 0.95"), 
                          labels = c(paste0(round(8 * 0.05 + 2, 1), " edges/trader, segmentation = 0.05"), 
                                     paste0(round(8 * 0.05 + 2, 1), " edges/trader, segmentation = 0.95"), 
                                     paste0(round(8 * 0.95 + 2, 1), " edges/trader, segmentation = 0.05"), 
                                     paste0(round(8 * 0.95 + 2, 1), " edges/trader, segmentation = 0.95")))
  
  
  start <- 1880 + 51
  sequence.years <- 6
  ggplot(data=plot_data, aes(x= trading_seq * sequence.years + start, y=(convergence + 1) * 50,  color = true_mod)) +
    geom_boxplot(aes(group = floor(trading_seq * sequence.years + start * (true_mod == "SlowTSI"))), 
                 position = position_dodge(width = 3), width = 2) + 
    geom_smooth(size=1, fill=NA) +
    scale_color_brewer(palette="Set1", labels = c(LogCo2 = expression(CO[2]), SlowTSI = "TSI"), guide = "none") +
    facet_wrap(~set, ncol = 1) +
    labs(x = "Year",  
         y = bquote(plain("% Traders Believing ") ~ CO[2] ~ Model ~ (n == 
                                                                       .(prettyNum(length(complete.cases(plot_data$convergence)), big.mark = ',')))),
         title = paste0(start, "-",  start - 1 + 14 * 6)) + 
    ylim(0,100) +
    theme(legend.justification=c(1,0), legend.position=c(1,0),
          legend.key.width = grid::unit(0.05, 'npc'),
          legend.key = element_rect(color=NA))
}
