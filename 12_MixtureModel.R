


##############################
######## Simulate Data
##############################
library(pracma)
N <- 1000
T <- 10
pi <- c(0.7,0.3)

b0 <- 1
b1 <- -1
b2 <- -2

b0.1 <- -1
b1.1 <- 1
b2.1 <- 3


x1 <- rnorm(N*T,0,1)
dim(x1) <- c(N,T)
x2 <- rnorm(N*T,0,1)
dim(x2) <- c(N,T)

u1 <- b0 + b1 * x1 + b2 * x2 
u2 <- b0.1 + b1.1 * x1 + b2.1 * x2

class <- sample(c(1,2),N,pi,replace = TRUE)

# p1 <- exp(u1)/(1+exp(u1))
# p2 <- exp(u2)/(1+exp(u2))
# 
# p <- matrix(0,N,1)
# p[class==0] <- p1[class==0]
# p[class==1] <- p2[class==1]

u <- (2-class)*u1 + (class-1)*u2

p <- 1/(1+exp(-u))

random <- runif(N,0,1)

yv <- ifelse(random<p,1,0)


##############################
####### Estimation
##############################

loglik <- function(parameter){

  b0 <- parameter[1]
  b1 <- parameter[2]
  b2 <- parameter[3]
  b0.1 <- parameter[4]
  b1.1 <- parameter[5]
  b2.1 <- parameter[6]
  
  u1 <- b0 + b1 * x1 + b2 * x2
  u2 <- b0.1 + b1.1 * x1 + b2.1 * x2
  p1 <- 1/(1+exp(-u1))
  p2 <- 1/(1+exp(-u2))
  
  lik1 <- yv*p1 + (1-yv)*(1-p1)
  lik2 <- yv*p2 + (1-yv)*(1-p2)
  
  # lik <- qi * log(lik1) + (1-qi)*log(lik2)
  lik <- q * log(lik1) + (1-q)*log(lik2)
  return(-sum(lik))
}

# b <- rep(2,6)
# parameter <- rep(0,6)

# initial parameters
b0 <- 0
b1 <- 0
b2 <- 0

b0.1 <- 0
b1.1 <- 0
b2.1 <- 0
pi <- runif(1)

tolerance <- 1e-5
diff <- 1000
itt <- 1

while(diff > tolerance){
  
  # E step
  u1 <- b0 + b1 * x1 + b2 * x2
  u2 <- b0.1 + b1.1 * x1 + b2.1 * x2
  
  p1 <- exp(u1)/(1+exp(u1))
  p2 <- exp(u2)/(1+exp(u2))
  
  lik1 <- p1*yv + (1-yv)*(1-p1)
  lik2 <- p2*yv + (1-yv)*(1-p2)
  
  
  l1 <- apply(lik1,1,prod)
  l2 <- apply(lik2,1,prod)
  
  qi <- (pi * l1)/(pi*l1 + (1-pi)*l2)
  dim(qi) <- c(N,1)
  q <- repmat(qi,1,T)
  pi <- mean(qi)
  
  parameter <- c(b0,b1,b2,b0.1,b1.1,b2.1)
  # M step
  mle <- optim(parameter,loglik,hessian = TRUE)
  diff <- max(abs(mle$par-parameter))
  
  b0 <- mle$par[1]
  b1 <- mle$par[2]
  b2 <- mle$par[3]
  
  b0.1 <- mle$par[4]
  b1.1 <- mle$par[5]
  b2.1 <- mle$par[6]
  
  
  tt <- itt + 1
  print(pi)
  sprintf("Iteration no. = %d",itt)
}

# real 1 -1 -2 -1 1 3
mle$par




