data{
  int<lower=2> K;
  int<lower=2> V;
  int<lower=1> D;
  int<lower=1> N;
  
  int<lower=1,upper=V> w[N];
  int<lower=1,upper=D> doc[N];
  vector<lower=0>[K] alpha;
  vector<lower=0>[V] beta;
}

parameters{
  simplex[K] theta[D];
  simplex[V] phi[K];
}


model{
  for(d in 1:D){
    theta[d] ~ dirichlet(alpha);
  }
  for(k in 1:K){
    phi[k] ~ dirichlet(beta);
  }
  for(n in 1:N){
    real gamma[K];
    for(k in 1:K)
      gamma[k] = log(theta[doc[n],k]) + log(phi[k,w[n]]);
      
    target += log_sum_exp(gamma);
  }
}
