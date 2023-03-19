
library(Matching)
library(MatchIt)

################################
######### Simulate Data
################################


n <- 1000

xv <- rnorm(n,0,1)
xv2 <- rnorm(n,0,1)

epsilon <- rnorm(n,0,1)

a1 <- 1
a2 <- 2
b <- -2


u <- b + a1 * xv + a2 * xv2
p <- 1/(1+exp(-u))

random <- runif(n,0,1)

treat <- ifelse(random <= p,1,0)


model_t <- glm(treat~xv,family = binomial(link = "logit"))
pscores <- model_t$fitted.values

# outcome model linear

b1 <- -1
b2 <- 4 # treatment effect 
b3 <- 4

yv <- b1 + b2*treat + b3*xv + epsilon

################################
######### Estimation
################################

# run linear models

model1 <- lm(yv~treat+xv)
model2 <- lm(yv~treat)
summary(model1)
summary(model2)

# propensity score as a control variable
model3 <- lm(yv ~ treat + pscores)
summary(model3)

rr <- Match(Y=yv,Tr=treat,X=pscores,M=1)
summary(rr)

# Match it
dta <- data.frame(yv,treat,xv)

m.out <- matchit(treat~xv,dta)
summary(m.out)

plot(m.out,type = "jitter")
plot(m.out,type = "hist")

m.data <- match.data(m.out)

model <- lm(m.data$yv ~ m.data$treat)
summary(model)
