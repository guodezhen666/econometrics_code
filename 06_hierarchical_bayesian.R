
# wrong and not done
#################################
######## Simulate Data
#################################

# y = b1 + b2*xv1 + b3*xv2 + b4*xv3
# b2 = gamma1 + gamma2 * h1 + gamma3 * h2 

n <- 1000
beta <- c(1,NaN,3,4)
gamma <- c(1,-1,1)
h <- matrix(rnorm(n*2,0,1),n,2)
b2 <- gamma[1] + h %*% gamma[2:3] + rnorm(n,0,1)
xv <- matrix(rnorm(n*3,0,1),n,3)
yv <- beta[1] + b2*xv[,1] + beta[3]*xv[,2] + beta[4]*xv[,3] + rnorm(n,0,1)

##################################
########### Estimate
##################################

# Gibbs sampling
beta_initial <- c(0,0,0)
h_initial <- c(0,0,0)









