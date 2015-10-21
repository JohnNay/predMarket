#
# Adapted from 
#
require(ggplot2)
require(tidyr)
require(dplyr)

acftest <- function(residue, model.arma, model.ar1, lag.max = NULL,
                    type = c('correlation','partial'),
                    ci = 0.96, ci.type=c('white','ma'))
{
  type = match.arg(type)
  ci.type = match.arg(ci.type)
  
  a <- acf(residue$r, plot=FALSE, lag.max = lag.max, type = type)
  a.df <- with(a, data.frame(acf = acf, lag = lag))
  a11 <- ARMAacf(coef(model.arma)[1], coef(model.arma)[2], tail(a$lag, 1), 
                 pacf = type == 'partial')
  a10 <- ARMAacf(coef(model.ar1)[1], 0, tail(a$lag,1),
                 pacf = type == 'partial')
  a.df <- cbind(a.df, arma11 = a11, ar1 = a10)
 
  if (ci > 0) { 
  clim0  <- qnorm((1+ci)/2) / sqrt(a$n.used)
  
  if (ci.type == 'ma') {
    clim <- clim0 * sqrt(cumsum( 2 * c(1,2*a.df$acf[-1]^2)))
    clim[1] <- NA
  } else {
    clim <- rep_len(clim0, nrow(a.df))
  }
  a.df$clim <- clim
  }
  
  ylabel <- switch(type,
    partial = "Partial ACF",
    correlation = "ACF"
  )
  
  p <- ggplot(a.df, aes(x = lag, xend=lag, y = acf, yend=0)) +
    geom_segment(size=1) + 
    geom_point(aes(y=ar1, shape="AR(1)", color = "AR(1)"), size=4) +
    geom_point(aes(y=arma11, shape="ARMA(1,1)", color = "ARMA(1,1)"), size=4)
  
  if (ci > 0) {
    p <- p +
    geom_line(aes(y = clim), color="dark blue", linetype="dashed") +
    geom_line(aes(y = -clim), color="dark blue", linetype="dashed")
  }
  
  p <- p + 
    scale_color_brewer(type="qual", palette=2, name = "Model") + 
    scale_shape(name = "Model") +
    labs(x = "Lag", y = ylabel) +
    theme_bw(base_size = 20) +
    theme(legend.position=c(0.99,0.99),
          legend.justification=c(1,1))
  p
}
