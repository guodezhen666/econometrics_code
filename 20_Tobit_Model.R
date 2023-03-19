


#########################
##### Simulate Data
#########################

n <- 1000

xv <- rnorm(n*3,0,1)
dim(xv) <- c(n,3)

b.0 <- 1
b.1 <- c(-1,2,-1)

yv <- b.0 + xv %*% b.1 + rnorm(n,0,1)

yv[yv<0] <- 0


#########################
####### Estimate
#########################

# 1. package estimator

library(censReg)
estimate.1 <- censReg(yv ~ xv)
summary(estimate.1)

estimate.2 <- lm(yv~xv)
summary(estimate.2) # biased


# 2. likelihood estimation

d <- rep(0,n)
d[yv>0] <- 1

loglik <- function(b){
  b.0 <- b[1]
  b.1 <- b[2:4]
  sigma <- b[5]
  
  loglik <- sum((1-d)*log(pnorm((-b.0 - xv%*%b.1)/sigma)) + 
                  d*log(1/sigma*dnorm((yv-b.0 - xv%*%b.1)/sigma)))
  
  return(-loglik)
}

estimate.3 <- optim(c(0,0,0,0,0.5),loglik,hessian = TRUE)
estimate.3$par
