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
#    phi ~ normal(0,2);
#    theta ~ normal(0, 2);
    

    res <- y - (m * x + b);
    
    ar <- rep_vector(0., T);
    ma <- rep_vector(0., T);
    eps <- rep_vector(0.,T);

    #
    # Formulation from eq. 2.26 in R. Prado and M. West, "Time Series: Modeling, Computation, and Finance"
    #
    for (t in 1:T) {
      for(p in 1:min(t-1,P)) {
        ar[t] <- ar[t] + phi[p] * res[t-p]; // autoregressive part
      }
      eps[t] <- res[t] - ar[t];
      for (q in 1:min(t-1,Q)) {
        ma[t] <- ma[t] + theta[q] * eps[t-q];
        eps[t] <- eps[t] - theta[q] * eps[t-q];
      }
      /*
      if (fabs(eps[t]) > 100) {
        print("t = ", t, ", ar = ", ar[t], ", ma = ", ma[t], ", eps = ", eps[t]);
        print("t = ", t, ", x = ", x[t], ", y = ", y[t], ", res = ", res[t]);
        print("t = ", t, ", m = ", m, ", b = ", b);
        print("t = ", t, ", m0 = ", m0, ", sm0 = ", sm0, ", b0 = ", b0, ", sb0 = ", sb0);
        if (P > 0) {
          print("t = ", t, ", phi[1] = ", phi[1]);
        }
        if (Q > 0) {
          print("t = ", t, ", theta[1] = ", theta[1]);
        }
      }
      */
    }

  res ~ normal(ar + ma, sigma);
}

generated quantities {
  vector[T] log_lik;
  matrix[T_future,reps] y_future;
  {
    vector[T + T_future] yy;
    vector[T] res;
    vector[T + T_future] err;
    real ar;
    real ma;

    for (i in 1:reps) {
      res <- y - (m * x + b);

      yy <- append_row(res, rep_vector(0, T_future));
      
      for (t in 1:T) {
        ar <-  0;
        ma <- 0;
        for(p in 1:min(t-1,P)) {
          ar <- ar + phi[p] * yy[t - p]; // autoregressive part
        }
        for (q in 1:min(t-1,Q)) {
          ma <- ma + theta[q] * err[t - q]; // moving average part
        }
        err[t] <- yy[t] - (ar + ma);
        log_lik[t] <- normal_log(y[t], m * x[t] + b + ar + ma, sigma);
      }
    
      for(t in 1:T_future) {
        ar <- 0;
        ma <- 0;
  
        for (p in 1:min(T + t - 1,P)) {
          ar <- ar + phi[p] * yy[T + t-p];
        }
        for (q in 1:min(T + t - 1,Q)) {
          ma <- ma + theta[q] * err[T + t-q];
        }
        err[T + t] <- normal_rng(0, sigma);
        yy[T + t] <- ar + ma + err[T + t];
        y_future[t,i] <- yy[T + t] + m * x_future[t] + b;
      }
    }
  }
}
