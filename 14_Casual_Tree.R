
library(causalTree)
library(grf)

###############################
######### Simulate Data
###############################

n <- 50000
prop <- 0.5

set.seed(123)

treat <- rbinom(n,1,prop)

x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)

tau <- 0.5*x1 + 2

eta <- 0.5*x1 + x2 - x3

e <- 0.1 * rnorm(n)

y <- eta + treat*tau + e

sim_data <- as.data.frame(cbind(y,x1,x2,x3,treat))

###############################
######### Estimate
###############################

trIdx <- which(sim_data$treat == 1)
conIdx <- which(sim_data$treat == 0)

trainIdx <- c(sample(trIdx,length(trIdx)/2),
              sample(conIdx,length(conIdx)/2))

train_data <- sim_data[trainIdx,]
est_data <- sim_data[-trainIdx,]

honestTree <- honest.causalTree(y ~ x1 + x2 + x3,
                                data = train_data,
                                treatment = train_data$treat,
                                est_data = est_data,
                                est_treatment = est_data$treat,
                                split.Rule = "CT", split.Honest = T,
                                HonestSampleSize = nrow(est_data),
                                split.Bucket = T, cv.option = "CT",
                                cv.Honest = F)

opcp <-  honestTree$cptable[,1][which.min(honestTree$cptable[,4])]
ct <- prune(honestTree, opcp)
rpart.plot(ct)

# construct test sample
n_dp <- 3000 # number of data points
k <- 3
X.test <- as.data.frame(matrix(rnorm(n_dp*k),nrow=n_dp,ncol=k))
colnames(X.test) <- c("x1","x2","x3")

tau.test <- 0.5 *X.test$x1 + 2


tau.ct <- predict(ct,newdata = X.test)
mean(tau.ct)
mean(tau.test)
mse.ct <- mean((tau.ct - tau.test)^2)
mse.ct

cf <- causal_forest(X=sim_data[,2:4],Y=sim_data$y,W=sim_data$treat)
tau.cf <- predict(cf,newdata = data.frame(X=X.test))
mean(tau.cf$predictions)


mean(tau.cf)
mean(tau.test)
mse.cf <- mean((tau.cf$predictions - tau.test)^2)
mse.cf














