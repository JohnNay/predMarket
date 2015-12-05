library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_bw(base_size = 20))

load("output/jg_convergence_true_past.Rda")

plot_data <- average_convergence
# plot_data$set <- factor(plot_data$set, levels = gtools::mixedsort(unique(plot_data$set)))
#  n.edg <- round(parameters[5]*100) + 100 # integer in (100, 200)
plot_data$set <- factor(plot_data$set, levels = c("0.05, 0.05", "0.05, 0.95", "0.95, 0.05", "0.95, 0.95"),
                        labels = c("105 edges, segmentation = 0.05", "105 edges, segmentation = 0.95", "195 edges, segmentation = 0.05", "195 edges, segmentation = 0.95"))


start <- 1880 + 51

p <- ggplot(data=plot_data, aes(x= trading_seq * 6 + start, y=(convergence + 1) * 50)) +
  geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.1) + geom_smooth(size=1, fill=NA) +
  scale_color_brewer(palette="Set1", name="True Model", labels = c(LogCo2 = expression(CO[2]), SlowTSI = "TSI")) +
  ylim(0,100) +
  facet_wrap(~set, ncol = 1) +
  labs(x = "Year",  
       y = bquote(plain("% Traders Believing ") ~ CO[2] ~ Model ~ (n == 
.(prettyNum(length(complete.cases(plot_data$convergence)), big.mark = ',')))),
       title = paste0("Convergence Over Trading Sequences\n", 
                      start, "-",  start - 1 + 14 * 6))

  print(p)

if (TRUE) {
  pdf("output/jg_timeseries_true_past.pdf", width=8, height=18)
  print(p)
  dev.off()
}