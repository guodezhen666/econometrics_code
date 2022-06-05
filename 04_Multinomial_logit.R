#############################
######## Simulated Data
#############################

n <- 10000
beta_1 <- c(1,2,3,4)
beta_2 <- c(-1,2,3,-2)

xv <- matrix(rnorm(n*3,0,1),n,3)

u_1 <- beta_1[1] + xv %*% beta_1[2:4]
u_2 <- beta_2[1] + xv %*% beta_2[2:4]

p1 <- exp(u_1)/(1+exp(u_1)+exp(u_2))
p2 <- exp(u_2)/(1+exp(u_1)+exp(u_2))

rand <- runif(n,0,1)

yv <- matrix(0,n,1)

yv[rand < p1] <- 1
yv[(rand > p1) & (rand < p1+p2)] <-2
yv[rand > p1+p2] <- 0

###########################################
########### estimate
###########################################

# package approach
library(nnet)
estimate.1 <- multinom(yv ~ xv)
summary(estimate.1)
# loglike estimate

loglike <- function(b){
  u_1 <- b[1] + xv %*% b[2:4]
  u_2 <- b[5] + xv %*% b[6:8]
  p1 <- exp(u_1)/(1+exp(u_1)+exp(u_2))
  p2 <- exp(u_2)/(1+exp(u_1)+exp(u_2))
  
  like <- (yv==1)*p1 + (yv==2)*p2 + (yv==0)*(1-p1-p2)
  return(sum(-log(like)))
}

estimate.2 <- optim(rep(0,8),loglike,hessian = TRUE,method = "L-BFGS-B")
estimate.2$par
std <- sqrt(diag(solve(estimate.2$hessian)))
std
