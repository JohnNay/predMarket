data {
  int<lower=1, upper=1> P;
  int<lower=1, upper=1> Q;
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
  real theta[Q]; // moving average coefficient
}

model {
    vector[T] res;
    real ar;
    real ma;
    vector[T] err;


    b ~ normal(b0, sb0);
    m ~ normal(m0, sm0);
#    theta ~ normal(theta0, stheta0);
#    phi ~ normal(phi0, sphi0);
    sigma ~ cauchy(0, ssig0);
    phi ~ normal(0,2);
    theta ~ normal(0, 2);


    res = y - (m * x + b);

    err = rep_vector(0.,T);

    #
    # Formulation from eq. 2.26 in R. Prado and M. West, "Time Series: Modeling, Computation, and Finance"
    #
    ar = 0;
    err[1] = res[1] - ar;
    for (t in 2:T) {
      ar = phi[1] * res[t-1];
      ma = theta[1] * err[t-1];
      err[t] = res[t] - (ar + ma);
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

  err ~ normal(0, sigma);
}

generated quantities {
  matrix[T_future,reps] y_future;
  {
    vector[T + T_future] yy;
    vector[T] res;
    vector[T + T_future] err;
    real ar;
    real ma;

    for (i in 1:reps) {
      res = y - (m * x + b);

      yy = append_row(res, rep_vector(0, T_future));

      ar = 0;
      err[1] = res[1] - ar;
      for (t in 2:T) {
        ar = phi[1] * res[t-1];
        ma = theta[1] * err[t-1];
        err[t] = res[t] - (ar + ma);
      }
      for(t in 1:T_future) {
        ar = phi[1] * yy[T + t-1];
        ma = theta[1] * err[T + t-1];
        err[T + t] = normal_rng(0, sigma);
        yy[T + t] = ar + ma + err[T + t];
        y_future[t,i] = yy[T + t] + m * x_future[t] + b;
      }
    }
  }
}
