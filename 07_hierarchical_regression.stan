data{
  int<lower=1> N; // number of samples
  int<lower=1> K1; // number of first 
  int<lower=1> K2;
  vector[N] y;
  matrix[N,K1] X;
  matrix[N,K2] Z;
}

parameters{
  real beta_0;
  real beta_2;
  real beta_3;
  real gamma0;
  real gamma1;
  real gamma2;
  real tau;
  real sigma;
  vector[N] beta_1;
}

model{
  target += normal_lpdf(beta_0|0,1);
  target += normal_lpdf(beta_2|0,1);
  target += normal_lpdf(beta_3|0,1);
  target += normal_lpdf(gamma0|0,1);
  target += normal_lpdf(gamma1|0,1);
  target += normal_lpdf(gamma2|0,1);
  target += inv_gamma_lpdf(sigma|1,2);
  target += inv_gamma_lpdf(tau|1,2);
  target += normal_lpdf(beta_1|gamma0*Z[,1] + gamma1*Z[,2] + gamma2*Z[,3],tau);
  target += normal_lpdf(y|beta_0 + beta_1 .* X[,1] + beta_2 * X[,2] + beta_3 * X[,3],sigma);
}



