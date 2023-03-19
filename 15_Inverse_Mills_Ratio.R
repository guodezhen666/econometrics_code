library(sampleSelection)


##########################
###### Simulate Data 
##########################

n <- 1000
x1 <- rnorm(n,0,1)
x2 <- rnorm(n,1,4)
x3 <- runif(n,1,2)
x4 <- runif(n,1,4)



a.0 <- 1
a.1 <- 1
a.2 <- -1
a.3 <- 0.5

u <- rnorm(n,0,1)
d <- a.0 + a.1*x1 + a.2*x2 + a.3*x3 + u

# report or not
z <- ifelse(d > 0,1,0)

# real function

b.0 <- 1
b.1 <- -2
b.2 <- 1
b.3 <- 0.2
b.4 <- 0.3
e <- 0.5*u


y <- b.0 + b.1*x1 + b.2*x2 + b.3*x3 + b.4*x4 + e

y[z==0] <- 0

data1 <- as.data.frame(cbind(y,z,x1,x2,x3,x4,z1,z2,z3))

##########################
###### Estimate 
##########################

# regression directly
model_direct <- lm(y ~ x1 + x2 + x3 + x4,data=data1[z==1,])
summary(model_direct) # wrong


# using inverse mills ratio
myProbit <- glm(z ~ x1 + x2 + x3,family = binomial(link="probit"))
data1$imr <- invMillsRatio( myProbit )$IMR1

myHeckit <- lm(y ~ x1 + x2 + x3 + x4 + imr,data = data1[z==1,])
summary(myHeckit)
