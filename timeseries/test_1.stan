data {
  int<lower=1> T;
  vector[T] y; // measured quantity.
  vector[T] x; // covariate
  real m0;
  real<lower = 0> sm0;
  real b0;
  real<lower = 0> sb0;
  real<lower = 0> ssig0;
  real phi0;
  real<lower = 0> sphi0;
}

parameters {
  real b; // intercept
  real m; // slope
  real<lower=0> sigma; // noise standard deviation
  real phi; // autoregression coefficient
}

model {
    vector[T] res;
    real ar[T];

    
    b ~ normal(b0, sb0);
    m ~ normal(m0, sm0);
    phi ~ normal(phi0, sphi0);
    sigma ~ cauchy(0, ssig0);


    res <- y - (m * x + b);
    
    ar[1] <- 0;
    for (t in 2:T) {
      ar[t] <- phi * res[t-1];
    }

  res ~ normal(ar, sigma);
}
