#############################
####### Simulate Data
#############################

n <- 10000
xv <- cbind(rnorm(n,0,1),rnorm(n,1,2),rnorm(n,2,2))
# beta <- cbind(rnorm(n,-2,1),rnorm(n,2,1),rnorm(n,0,1))
beta <- c(1,-1,1)

yv <- rpois(n,lambda = exp(xv %*% beta))

#############################################
##### Estimate Using Metropolis algorithm
#############################################
S <- 10000

prior_mean <- c(0,0,0) 
prior_var <- c(10,10,10)

## sample variance 
sample_var <- var(log(yv+1/2))*solve(t(xv)%*%xv)

accept_count <- 0

beta_est <- c(0,0,0)
BETA <- matrix(0,nrow = S, ncol=3)

library(MASS)
start=Sys.time()
for(s in 1:S){
  beta_new <- mvrnorm(1,beta_est,sample_var)
  
  # accept rate
  accept_rate <- sum(dpois(yv,exp(xv %*% beta_new),log=T)) + 
    sum(dnorm(beta_new,prior_mean,prior_var,log=T)) - 
          sum(dpois(yv,exp(xv %*% beta_est),log=T)) - 
          sum(dnorm(beta_est,prior_mean,prior_var,log=T))
  random <- runif(1)
  if(log(random) < accept_rate){
    beta_est <- beta_new
    accept_count <- accept_count + 1
  }
  BETA[s,] <- beta_est
}

end=Sys.time()
print(end-start)
