################################
####### Simulate Data
################################

n <- 1000
beta <- c(1,-1,1,-1)
xv <- matrix(rnorm(n*3,0,1),n,3)
u <- beta[1] + xv %*% beta[2:4]
p <- exp(u)/(1+exp(u))
rand <- runif(n,0,1)
yv <- matrix(0,n,1)
yv[rand < p] <- 1

################################
######## Estimate
################################

# glm approach
estimate.1 <- glm(yv ~ xv,family = "binomial")
summary(estimate.1)

# liglike hood estimation
loglike <- function(b){
  u <- b[1] + xv %*% b[2:4]
  p <- exp(u)/(1+exp(u))
  like <- (yv == 0)*(1-p) + (yv==1)*p
  return(sum(-log(like)))
}

estimate.2 <- optim(rep(0,4),loglike,method="L-BFGS-B",hessian=TRUE)
estimate.2$par


##################################
######### Probit 
##################################
n <- 1000
beta <- c(1,-1,1,-1)
xv <- matrix(rnorm(n*3,0,1),n,3)
u <- beta[1] + xv %*% beta[2:4]
p <- pnorm(u,0,1)
rand <- runif(n,0,1)
yv <- matrix(0,n,1)
yv[rand < p] <- 1

estimate.3 <- glm(yv ~ xv,family = binomial(link = "probit"))
summary(estimate.3)

loglike <- function(b){
  u <- b[1] + xv %*% b[2:4]
  p <- pnorm(u,0,1)
  like <- (yv==1)*p + (yv==0)*(1-p)
  return(sum(-log(like)))
}

estimate.4 <- optim(rep(0,4),loglike,method = "L-BFGS-B",hessian = TRUE)
estimate.4$par
std <- sqrt(diag(solve(estimate.4$hessian)))




