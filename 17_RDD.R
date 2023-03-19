

############################
####### Simulate Data
############################

n <- 1000
xv <- runif(n,1,10)
data <- as.data.frame(xv)
p1 <- 0.8
p2 <- 0.1

random <- runif(n,0,1)

treat <- (random < p1 & data$xv>=5)|(random<p2 &data$xv<5)
data$treat <- ifelse(treat,1,0)

plot(data$xv,data$treat)

data$xv1 <- rnorm(n,0,1)
data$xv2 <- rnorm(n,0,1)

b.0 <- 1
b.1 <- -1
b.2 <- 1
b.3 <- 10
b.4 <- 2

data$y <- b.0 + b.1*data$xv1 + b.2*data$xv2 + b.3*treat + b.4*xv + rnorm(n,0,1)

plot(data$xv,data$y)

##########################
###### Estimate
##########################
library(rdd)

rslt1 <- RDestimate(y ~ xv |xv1+xv2,data=data,cutpoint = 5) # sharp rdd
summary(rslt1) # biased
plot(rslt1) 

# fuzzy rdd
rslt2 <- RDestimate(y ~ xv + treat|xv1+xv2,data=data,cutpoint = 5)
summary(rslt2)
plot(rslt2)
