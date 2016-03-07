library(ggplot2)
library(dplyr)
library(tidyr)

sa_plot <- function(){
  source('utils/pc_sa.R', chdir = T)
  load("output/src_future.Rda")
  ss <- correct_bias(src_future@result[[7]])
  ss <- ss %>% mutate(var = ordered(var, levels = c('ideo','risk.tak','n.traders',
                                                    'n.edg', 'seg', 'true.model'),
                                    labels = c('Ideology', "Risk tolerance",
                                               '# Traders', '# Edges/Trader', 'Segmentation',
                                               "True model")))
  title <- "" #bquote(R^2 == .(round(src_future@r_squared, 2)) * plain(', ') ~
               #             n == .(length(src_future@sims)))
  
  ggplot(ss, aes(x = var, y = x_corr)) + 
    geom_hline(yintercept = 0, size = 1, color = 'dark gray') +
    geom_point(size = 2) +
    geom_errorbar(aes(ymax = max_ci, ymin = min_ci), width=0.25, size = 1) +
    labs(x = NULL, y = "Standardized Rank Regression Coefficient",
         title = title) +
    ylim(c(-1,1)) +
    theme(panel.grid.major.x = element_blank(), 
          axis.text.x = element_text(angle = 30, hjust=1, vjust=1))
}
