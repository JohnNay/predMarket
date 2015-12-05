library(ggplot2)
library(dplyr)
library(tidyr)

source('utilities/pc_sa.R', chdir = T)

theme_set(theme_bw(base_size = 20))

load("output/src_future.Rda")


ss <- correct_bias(src_future@result[[7]])
ss <- ss %>% mutate(var = ordered(var, levels = c('ideo','risk.tak','n.traders',
                                                  'n.edg', 'seg', 'true.model'),
                                  labels = c('Ideology', "Risk tolerance",
                                             '# Traders', '# Edges', 'Segmentation',
                                             "True model")))

title <- bquote(atop(plain("Estimated Effects on Convergence of Beliefs"),plain("in Future Scenario ") ~
                          (R^2 == .(round(src_future@r_squared, 2)) * plain(', ') ~
                          n == .(length(src_future@sims)))))



p_sa <- ggplot(ss, aes(x = var, y = x_corr)) + 
  geom_hline(yintercept = 0, size = 1, color = 'dark gray') +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = max_ci, ymin = min_ci), width=0.25, size = 1) +
  labs(x = NULL, y = "Partial Rank Correlation Coefficient",
       title = title) +
  ylim(c(-1,1)) +
  theme(panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 30, hjust=1, vjust=1))

print(p_sa)

pdf("jg_sa_future.pdf", width=8, height=8)
print(p_sa)
dev.off()
