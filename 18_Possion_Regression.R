

########################
###### Simulate Data
########################

b.0 <- -1
b.1 <- c(1,-1,2)
n <- 400

xv <- rnorm(n*3,0,1)
dim(xv) <- c(n,3)

y_mean <- exp(b.0 + xv %*% b.1)

yv <- rpois(rep(1,n),y_mean)


#########################
###### Estimate
#########################

# 1. generalize linear model package
estimate.1 <- glm(yv ~ xv,family = poisson)
summary(estimate.1)

# 2. loglike estimate

loglik <- function(b){
  b.0 <- b[1]
  b.1 <- b[2:4]
  
  y_mean <- exp(b.0 + xv %*% b.1)
  
  like <- -dpois(yv,y_mean,log = TRUE)
  
  return(sum(like))
}

estimate.2 <- optim(b <- c(0,0,0,0),loglik,hessian = TRUE)

estimate.2$par

im <- solve(estimate.2$hessian)
sqrt(diag(im))

# 3. MCMC solution

# MH algorithm

S <- 100000

beta_est <- c(0,0,0,0)


# prior distribution mean= 0,variance =10
prior_mean = 0
prior_var = 10


BETA <- matrix(0,nrow = S,ncol=4)

library(MASS)
start <- Sys.time()
for(s in 1:S){
  beta_new <- rnorm(4,0,1)
  
  accept <- -loglik(beta_new) + sum(dnorm(beta_new,prior_mean,prior_var,log=TRUE)) + loglik(beta_est) - 
    sum(dnorm(beta_est,prior_mean,prior_var,log=TRUE)) # -loglike since function used to MLE
  
  random <- runif(1)
  if(log(random) < accept){
    beta_est <- beta_new
  }
  BETA[s,] <- beta_est
}

