
#################################
######### Fixed effect
#################################

n <- 100 
time <- 50
N <- n*time
beta <- c(1,2,3,4)

indiv_fix <- rnorm(n,0,1)
time_fix <- rnorm(time,0,1)

X1 <- rnorm(n*time,mean = rep(indiv_fix,each=time),sd=1)

X2 <- rnorm(n*time,mean = rep(time_fix,n),1)

X3 <- runif(n*time,1,2)

indiv <- rep(1:n,each=time)
time_stamp <- rep(1:time,n)


Y <- beta[1] + X1 * beta[2] + X2 * beta[3] + X3 * beta[4] + rep(indiv_fix,each=time) + 
  rep(time_fix,n) + rnorm(n*time,0,1)

data <- data.frame(cbind(indiv,time_stamp,Y,X1,X2,X3))
library(plm)
pool <- plm(Y ~ X1 + X2+ X3, data = data,index=c("indiv","time_stamp"),model = "pooling")
summary(pool)


# fix effect 
fix <- plm(Y ~ X1 + X2+ X3 ,effect = "twoways",data = data,index=c("indiv","time_stamp"),model = "within")
summary(fix)

# LSDV estimator
fix2 <- lm(Y ~ X1 + X2 + X3 + factor(indiv) + factor(time_stamp) - 1 ,data = data)
summary(fix2)


################################
######### Random Effect
################################

n <- 100
time <- 50

beta <- c(1,2,3,4)

# individual random effect
U_individual <- rnorm(n,mean = 0,sd=1)

# time random effect
U_time <- rnorm(time,0,sd=1)

N <- n * time

xv <- matrix(rnorm(N*3,0,1),N,3)

xv <- cbind(xv,rep(1:n,time),rep(1:n,rep(time,n)))

yv <- beta[1] + xv[,1:3] %*% beta[2:4] + rep(U_individual,time) + rep(U_time,each=n)+
  rnorm(N,0,3)

data <- data.frame(cbind(yv,xv))
colnames(data) <- c("Y","X1","X2","X3","individual","time")

library(plm)
random <- plm(Y ~ X1 + X2+ X3,effect = "twoway",data = data,index=c("individual","time"),model = "random")
summary(random)
pool <- plm(Y ~ X1 + X2+ X3, data = data,index=c("individual","time"),model = "pooling")
summary(pool)
