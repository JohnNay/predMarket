library(ggplot2)
library(dplyr)
library(tidyr)

source('utilities/pc_sa.R', chdir = T)

load("output/src_future.Rda")


ss <- correct_bias(src_future@result[[7]])
ss <- ss %>% mutate(var = ordered(var, levels = c('ideo','risk.tak','n.traders',
                                                  'n.edg', 'seg', 'true.model'),
                                  labels = c('Ideology', "Risk tolerance",
                                             '# Traders', '# Edges', 'Segmentation',
                                             "True model")))

outcome_var <- paste0("Convergence of Beliefs in Future Scenario \n (R^2= ", 
                      round(src_future@r_squared, 2), " , n=",  
                      length(src_future@sims),")")



ggplot(ss, ggplot2::aes(x = var, y = x_corr)) + ggplot2::geom_point() +
  labs(x = "Variable", y = "Partial Rank Correlation Coefficient",
  title = paste("Estimated Effects on", outcome_var)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymax = max_ci, ymin = min_ci), width=0.25) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::ylim(c(-1,1)) +
  ggplot2::theme_bw()

