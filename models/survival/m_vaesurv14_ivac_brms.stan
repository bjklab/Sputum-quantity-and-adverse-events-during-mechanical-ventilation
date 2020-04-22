// generated with brms 2.12.0
functions {
}
data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  int<lower=-1,upper=2> cens[N];  // indicates censoring
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> shape;  // shape parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  r_1_1 = (sd_1[1] * (z_1[1]));
}
model {
  // initialize linear predictor term
  vector[N] mu = Intercept + rep_vector(0, N);
  for (n in 1:N) {
    // add more terms to the linear predictor
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
  }
  for (n in 1:N) {
    // apply the inverse link function
    mu[n] = exp(mu[n]) / tgamma(1 + 1 / shape);
  }
  // priors including all constants
  target += normal_lpdf(Intercept | 0, 0.5);
  target += gamma_lpdf(shape | 0.01, 0.01);
  target += exponential_lpdf(sd_1 | 1);
  target += normal_lpdf(z_1[1] | 0, 1);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      // special treatment of censored data
      if (cens[n] == 0) {
        target += weibull_lpdf(Y[n] | shape, mu[n]);
      } else if (cens[n] == 1) {
        target += weibull_lccdf(Y[n] | shape, mu[n]);
      } else if (cens[n] == -1) {
        target += weibull_lcdf(Y[n] | shape, mu[n]);
      }
    }
  }
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}

