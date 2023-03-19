
# GMM estimation for linear regression
library(gmm)

# Linear Model

################################
########## Simulate Data
################################

b.0 <- 1
b.1 <- -1
b.2 <- 2

xv1 <- rnorm(n,0,1)
xv2 <- rnorm(n,0,1)


y <- b.0 + b.1*xv1 + b.2 *xv2 +  rnorm(n,0,1)

##################################
######### Estimation
##################################
# package

g <- y~xv1+xv2
h <- cbind(xv1,xv1^2,xv1^3,xv2,xv2^2,xv2^3)

estimate.1 <- gmm(g,x=h)
summary(estimate.1)
