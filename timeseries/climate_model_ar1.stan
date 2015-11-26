data {
  int<lower=1, upper=1> P;
  int<lower=0, upper=0> Q;
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
  real theta0;
  real<lower = 0> stheta0;
  int<lower=0> T_future;
  int<lower=0> reps;
  vector[T_future] x_future;
}

parameters {
  real b; // intercept
  real m; // slope
  real<lower=1E-8> sigma; // noise standard deviation
  real phi[P]; // autoregression coefficient
#  real theta[Q]; // moving average coefficient
}

model {
    vector[T] res;
    vector[T] ar;

    
    b ~ normal(b0, sb0);
    m ~ normal(m0, sm0);
    phi ~ normal(phi0, sphi0);
    sigma ~ cauchy(0, ssig0);


    res <- y - (m * x + b);
    
    ar[1] <- 0;
    for (t in 2:T) {
      ar[t] <- phi[1] * res[t-1];
    }

  res ~ normal(ar, sigma);
}

generated quantities {
  matrix[T_future,reps] y_future;
  {
    vector[T + T_future] yy;
    vector[T] res;
    real ar;

    for (i in 1:reps) {
      res <- y - (m * x + b);

      yy <- append_row(res, rep_vector(0, T_future));
      
      ar <- 0;
      for (t in 2:T) {
        ar <- ar + phi[1] * res[t-1];
      }
      for(t in 1:T_future) {
        ar <- phi[1] * yy[T + t-1];
        yy[T + t] <- ar + normal_rng(0, sigma);
        y_future[t,i] <- yy[T + t] + m * x_future[t] + b;
      }
    }
  }
}
