
#################################
######### Simulate Data
#################################

library(rstan)
library(gtools)

K <- 5
V <- 15
D <- 1000
avg_doc_length <- 200

doc_length <- rpois(D,avg_doc_length)
N <- sum(doc_length)

w <- rep(NA,N)
doc <- rep(NA,N)

alpha <- runif(K,min=0.5,max=1)
theta <- rdirichlet(D,alpha)
beta <- rep(200/V,V)

phi <- array(NA,c(K,V))
phi <- rdirichlet(K,beta)

n <- 1
for(d in 1:D){
  for(i in 1:doc_length[d]){
    z <- which(rmultinom(1,1,theta[d,]) == 1)
    w[n] <- which(rmultinom(1,1,phi[z,]) == 1)
    doc[n] <- d
    n <- n + 1
  }
}

data <- list(K=K, V=V, D=D, N=N, z=z, w=w, doc=doc, alpha=alpha, beta=beta)

t_start <- Sys.time()
fit <- stan(file = "D:/¹ùµÂÕæ/econometrica_code/09_LDA.stan",data=data)
t_end <- Sys.time()
print(t_end-t_start)

summary(fit)
