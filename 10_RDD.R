# RDD

##########################
#### Simulate Data
#########################
set.seed(123)

i.dim <- 1000

# runing variable
x <- runif(i.dim,-1,1)
# cut off point x = 0, dummy variable
D <- ifelse(x>=0,1,0)
# control variable
cv <- rnorm(i.dim,0,1)
# error term
e <- rnorm(i.dim,0,2)
# LATE =10 
y <- -2 + 10 * D + 4*x + 3*cv + e

plot(x,y)

# sharp RDD
library(rdd)
rslt <- RDestimate(formula = y~x|cv)
#outcome ~ running var|cv1+cv2
summary(rslt)
plot(rslt)

# for fuzzy rdd
# assume a logistic function for probability

#Prob(D=1|x) = E(D|x)

p <- 0.4 * D + 0.6/(1+exp(-3*x))
plot(x,p)

r <- runif(i.dim,0,1)
# assignment to treatment group 
D.1 <- ifelse(r <=p,1,0)

plot(x,D.1)

### outcome for fuzzy rdd case
y.1 <- -2 + 10 * D.1 + 4*x + 3*cv + e
plot(x,y.1)

# estimation
rslt1 <- RDestimate(y.1~x|cv)
summary(rslt1) # wrong

# two step
rslt2 <- RDestimate(y.1~x+D.1|cv)
summary(rslt2)



