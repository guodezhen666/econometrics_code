
#########################
##### Simulate Data
#########################

n <- 1000


xv <- rnorm(n*3,0,1)
dim(xv) <- c(n,3)

b.0 <- -1
b.1 <- c(1,2,-1)
theta <- 1

eta <- b.0 + xv %*% b.1

yv <- rnbinom(n,mu=exp(eta),size = theta)


###########################
######## estimate
###########################

# package
library(MASS)
estimate.1 <- glm.nb(yv ~ xv)
summary(estimate.1)

# liklihood 

loglik <- function(b){
  b.0 <- b[1]
  b.1 <- b[2:4]
  theta <- b[5]
  
  eta <- b.0 + xv %*% b.1
  
  lik <- -sum(log(dnbinom(yv,mu=exp(eta),size=theta)))
  return(lik)
}


estimate.2 <- optim(c(0,0,1,0,2),loglik,hessian = TRUE)
estimate.2$par
