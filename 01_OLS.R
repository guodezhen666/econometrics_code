
# MLE

######################
####### Simulate Data
#######################

set.seed(123)
i.dim <- 1000

# variables 

x <- rnorm(i.dim*3,0,1)
dim(x) <- c(i.dim,3)

# error term
e <- rnorm(i.dim,0,2)

# parameters
b0 <- -2  # intercept

b1 <- c(3,-2,0) # slopes

# outcome variable
y <- b0 + x %*% b1 + e

data.frame(y,x)


rslt <- lm(y~x)
summary(rslt)

# MLE
# get negative log likelihood
loglike <- function(b){
  # parameters
  # b[5] standard error
  y_hat <- b[1] + x %*% b[2:4]
  
  #repeat data generation process
  loglik <- -sum(dnorm(y-y_hat,0,b[5],log = TRUE))
  return(loglik)
}

b <- c(0,0,0,0,1)
estimate <- optim(b,loglike,hessian = TRUE)
estimate$par
# estimate <- nlm(loglike,b,hessian = TRUE)
# estimate$estimate