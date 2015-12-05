library(dplyr)
library(tidyr)
library(ggplot2)

load("G:/JG_Documents/Programming/R_Scripts/John_Nay/prediction_market/output/convergence_future.Rda")

plot_data <- average_convergence
# plot_data$set <- factor(plot_data$set, levels = gtools::mixedsort(unique(plot_data$set)))
#  n.edg <- round(parameters[5]*100) + 100 # integer in (100, 200)
plot_data$set <- factor(plot_data$set, levels = c("0.05, 0.05", "0.05, 0.95", "0.95, 0.05", "0.95, 0.95"),
                        labels = c("105 edges, segmentation = 0.05", "105 edges, segmentation = 0.95", "195 edges, segmentation = 0.05", "195 edges, segmentation = 0.95"))


p <- ggplot(data=plot_data, aes(x= trading_seq * 6 + 2014, y=(convergence + 1) * 50, color = true_mod)) +
  geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.1) + geom_smooth(size=1, fill=NA) +
  facet_wrap(~set, ncol = 1) +
  ggtitle(paste0("Convergence Over Trading Sequences 2015-",  2014 + 14 * 6, " (initial history = 135 years)")) +
  xlab("Year") +
  ylab(paste0("% Traders Believing True Model (n = ",
              prettyNum(length(complete.cases(plot_data$convergence)), big.mark = ','), ")")) +
  ylim(0,100) +
  theme_bw() + theme(legend.justification=c(1,0), legend.position=c(1,0)) +
  #scale_color_discrete(name="True Model") +
  scale_color_brewer(palette="Set1", name="True Model", labels = c(LogCo2 = expression(CO[2]), SlowTSI = "TSI"))

print(p)

if (FALSE) {
  pdf("output/jg_timeseries_future.pdf", width=8, height=18)
  print(p)
  dev.off()
}