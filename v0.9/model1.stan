// The input data
data {
  int<lower=0> N;
  int<lower=0> K;
  matrix [N, K] x;
  vector [N] y;
}

// The parameters
parameters {
  real alpha;   //Intercept
  vector[K] beta;
  real<lower=0> sigma;
}

// The model
model {
  y ~ student_t(N-K, x * beta + alpha, sigma);
  beta  ~ student_t(N-K, 0, 2);
  sigma ~ inv_chi_square(0.3);
}
