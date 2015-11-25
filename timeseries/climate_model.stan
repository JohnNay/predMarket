data {
  int<lower=1> T;
  int<lower=0> P; // autoregressive degree
  int<lower=0> Q; // moving-average degree
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
  real theta[Q]; // moving average coefficient
}

model {
    vector[T] res;
    vector[T] ar;
    vector[T] ma;
    vector[T] eps;
    
    b ~ normal(b0, sb0);
    m ~ normal(m0, sm0);
    theta ~ normal(theta0, stheta0);
    phi ~ normal(phi0, sphi0);
    sigma ~ cauchy(0, ssig0);

    res <- y - (m * x + b);
    
    ar <- rep_vector(0., T);
    ma <- rep_vector(0., T);
    eps <- rep_vector(0.,T);

    for (t in 1:T) {
      for(p in 1:min(t-1,P)) {
        ar[t] <- ar[t] + phi[p] * res[t - p]; // autoregressive part
      }
      eps[t] <- res[t];
      for (q in 1:min(t-1,Q)) {
        eps[t] <- eps[t] - theta[q] * eps[t-q];
        ma[t] <- ma[t] + theta[q] * eps[t-q];
      }
#      if (fabs(eps[t]) > 1E+6) {
#        print("t = ", t, ", ar = ", ar[t], ", ma = ", ma[t], ", eps = ", eps[t]);
#        print("t = ", t, ", x = ", x[t], ", y = ", y[t], ", res = ", res[t]);
#        print("t = ", t, ", m = ", m, ", b = ", b, 
#        ", m0 = ", m0, ", sm0 = ", sm0,
#        ", b0 = ", b0, ", sb0 = ", sb0);
#        if (P > 0) {
#          print("t = ", t, ", phi[1] = ", phi[1]);
#        }
#        if (Q > 0) {
#          print("t = ", t, ", theta[1] = ", theta[1]);
#        }
#      }
    }

  y ~ normal(m * x + b + ar + ma, sigma);
}

generated quantities {
  vector[T] log_lik;
  matrix[T_future,reps] y_future;
  {
    vector[T + T_future] yy;
    vector[T] res;
    vector[T + T_future] err;
    real nu;
    real eta;

    for (i in 1:reps) {
      yy <- append_row(y, rep_vector(0, T_future));
  
      res <- y - (m * x + b);
      
      for (t in 1:T) {
        nu <-  0;
        eta <- 0;
        for(p in 1:min(t-1,P)) {
          nu <- nu + phi[p] * res[t - p]; // autoregressive part
        }
        for (q in 1:min(t-1,Q)) {
          eta <- eta + theta[q] * err[t - q]; // moving average part
        }
        err[t] <- res[t] - nu - eta;
        log_lik[t] <- normal_log(y[t], m * x[t] + b + nu + eta, sigma);
      }
    
      for(t in 1:T_future) {
        nu <- 0;
        eta <- 0;
  
        for (p in 1:min(T + t - 1,P)) {
          nu <- nu + phi[p] * yy[T + t-p];
        }
        for (q in 1:min(T + t - 1,Q)) {
          eta <- eta + theta[q] * err[T + t-q];
        }
        err[T + t] <- normal_rng(0, sigma);
        yy[T + t] <- nu + eta + err[T + t];
        y_future[t,i] <- yy[T + t] + m * x_future[t] + b;
      }
    }
  }
}
