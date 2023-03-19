
library(MASS)
library(systemfit)
library(AER)
library(texreg)
N <- 10000

Sigma <- matrix(c(1,0.5,0.5,.5,1,0,.5,0,1),3,3)

colnames(Sigma) <- rownames(Sigma) <- c("x1","x2","z")

data <- mvrnorm(N,c(0,0,0),Sigma)
data <- as.data.frame(data)

data$y <- 0.4*data$x1 + 0.7*data$x2 + rnorm(N)

m1 <- lm(y~x1,data)
summary(m1) # inconsistent

m2 <- ivreg(y~x1|z,data=data)
summary(m2)

# Using systemfit
m3 <- systemfit(y~x1,inst = ~z,data=data,method = "2SLS")
summary(m3)
# Mannual Specification
fitted <- predict(lm(x1~z,data=data))
m4 <- lm(y~fitted,data=data)

screenreg(list(m1, m2, m3, m4), digits = 3)







