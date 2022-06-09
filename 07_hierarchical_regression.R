

#################################
######## Simulate Data
#################################

# y = b1 + b2*xv1 + b3*xv2 + b4*xv3
# b2 = gamma1 + gamma2 * h1 + gamma3 * h2 

n <- 10000
beta <- c(1,NaN,3,4)
gamma <- c(1,-1,1)

zv <- matrix(rnorm(n*3,0,1),n,3)
b2 <- zv %*% gamma + rnorm(n,0,1)
xv <- matrix(rnorm(n*3,0,1),n,3)
yv <- c(beta[1] + b2*xv[,1] + beta[3]*xv[,2] + beta[4]*xv[,3] + rnorm(n,0,1))

##################################
########### Estimate
##################################
library(rstan)
fit <- stan(file = "D:/¹ùµÂÕæ/econometrica_code/07_hierarchical_regression.stan",data=list(N=n,K1=3,K2=3,X=xv,y=yv,Z=zv))
summary(fit)









