## quantile regression

########################
### Simulation Data
########################

set.seed(123)

i.dim <- 1000

# variable
x <- runif(i.dim,0,1)

# quantile regression
# cause if due to heteroskedastiticy
# the variance error term is a function of x

e <- rnorm(i.dim,0,1) * sqrt(1 + 6*x)

# outcome
y <- 10 + x + e
plot(x,y)


# install.packages("quantreg")
library(quantreg)

# define quantiles
t <- c(0.1,0.25,0.5,0.75,0.90)

rslt <- rq(y~x,tau=t) 
coef(rslt)

plot(summary(rslt),parm = 'x')
plot(summary(rslt),parm = '(Intercept)')
