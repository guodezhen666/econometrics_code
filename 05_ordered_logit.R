###############################
####### Simulate data
###############################

n <- 10000
xv <- matrix(rnorm(n*3,0,1),n,3)
beta <- c(-1,2,1)

u <-  xv %*% beta[1:3] + rlogis(n,0,1)

alpha_0 <- 0
alpha_1 <- 1
yv <- matrix(-1,n,1)

yv[u < alpha_0] <- 0
yv[u > alpha_0 & u < alpha_1] <- 1
yv[u > alpha_1] <- 2

loglike <- function(b){
  p1 <- plogis(b[4]- ( xv %*% b[1:3]),0,1)
  p2 <- plogis(b[5] - ( xv %*% b[1:3]),0,1) - p1
  p3 <- 1- p1 - p2
  
  like <- (yv==0)*p1 + (yv==1)*p2 + (yv==2)*p3
  return(sum(-log(like)))
}

estimate.1 <- optim(c(0,0,0,0,1),loglike,method="L-BFGS-B",hessian=TRUE)
estimate.1$par



