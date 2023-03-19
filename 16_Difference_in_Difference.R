library(plm)

###################################
######## Simulate Data
###################################

n <- 100
time <- 50
N <- n*time


treat_indiv <- sample(n,50,replace = FALSE)
xv <- matrix(rnorm(N*3),N,3)
xv <- cbind(rep(1:n,time),rep(1:time,rep(n,time)),xv)
data <- as.data.frame(xv)

data$treat <- 0
data[data$V1 %in% treat_indiv,]$treat <- 1

data$after <- 0
data[data$V2>25,]$after <- 1
data$did <- data$treat * data$after

b.0 <- 1
b.1 <- 3
b.2 <- 2
b.3 <- 1
b.4 <- -1
b.5 <- 2
b.6 <- -1

data$y <- b.0 + b.1*data$did + b.2*data$treat + b.3*data$after + b.4*data$V3 +
  b.5*data$V4 + b.6*data$V5 + rnorm(N,0,1)

####################################
####### Estimate
###################################

est <- lm(y~treat+after+did,data=data)
summary(est)

est2 <- lm(y~did,data=data)
summary(est2) # biased

##################################
########## Fix effect
##################################
fix <- plm(y ~ did ,effect = "twoways",data = data,index=c("V1","V2"),
           model = "within")
summary(fix)
