################################
######### Simulate Data
################################


N <- 500 # number of indiv
T <- 50 # number of time

quality <- 6
price <- 7

Q0 <- 3
varQ0 <- exp(3)

a <- 2 # quanlity sensitivity
b <- -2 # price sensitivity

var_ad <- exp(2)

ad <- matrix(runif(N*T,min=0,max=1),N,T)

ad[ad>0.7] <- 1
ad[ad<1] <- 0

# signal mean
A <- matrix(rnorm(N*T,mean=quality,sd=sqrt(var_ad)),N,T)
A <- A*ad

# price
price <- rnorm(T,price,0.5)
price <- matrix(rep(price,N),N,T,1)

q <- matrix(0,N,T) # quality expection
q[,1] <- Q0
sd <- matrix(0,N,T)
sd[,1] <- varQ0 

U <- a * q[,1] + b * price[,1]
p <- exp(U)/(1+exp(U))
random <- runif(N,0,1)

Choice <- ifelse(random<p,1,0)

for(t in 2:T){
  q[,t] <- (1 - ad[,t-1]) * q[,t-1] + # no advertisement
            ad[,t-1]*(q[,t-1]/sd[,t-1] + A[,t-1]/var_ad)/(1/sd[,t-1] + 1/var_ad)
  sd[,t] <- (1 - ad[,t-1]) * sd[,t-1] + # no advertisement
            ad[,t-1]*(1/(1/sd[,t-1] + 1/var_ad))
  
  U <- a * q[,t] + b * price[,t]
  p <- exp(U)/(1+exp(U))
  random <- runif(N,0,1)
  Choice_t <- ifelse(random<p,1,0)
  Choice <- cbind(Choice,Choice_t)
}

###################################
########## Estimation
###################################

## estimate without signal
SIM <- 100

estimate <- function(param){

  q0 <- param[1]
  varq0 <- exp(3) # for identification assume known ???

  Q <- 6
  varad <- exp(param[2])

  a <- param[3]
  b <- param[4]
  
  set.seed(13)
  for(sim in 1:SIM){

    A <- matrix(rnorm(N*T,Q,sqrt(varad)),N,T)
    A <- A*ad

    q <- matrix(0,N,T)
    sd <- matrix(0,N,T)
    q[,1] <- q0
    sd[,1] <- varq0

    for(t in 2:T){
      q[,t] <- (1 - ad[,t-1]) * q[,t-1] + # no advertisement
        ad[,t-1]*(q[,t-1]/sd[,t-1] + A[,t-1]/varad)/(1/sd[,t-1] + 1/varad)
      sd[,t] <- (1 - ad[,t-1]) * sd[,t-1] + # no advertisement
        ad[,t-1]*(1/(1/sd[,t-1] + 1/var_ad))
    }

    # utility
    U <- matrix(0,N,T)
    U <- a*q + b*price
    Pr <- exp(U)/(1+exp(U))

    lik <- matrix(0,N,T)
    lik <- Choice*Pr + (1-Choice)*(1-Pr)

    if(sim==1){
      slik <- apply(lik,1,prod)
    }else{
      slik <- slik + apply(lik,1,prod)
    }
  }
  llik <- -sum(log(slik/SIM))
  return(llik)
}

# param<-c(0,0,0,0)

library(compiler)
estimate <- cmpfun(estimate)
fit<-nlm(estimate, param<-c(0,0,0,0), hessian=TRUE,print.level=2,iterlim = 1000)
se<-sqrt(diag(solve(fit$hessian)))
se

# estimate with signal

estimate <- function(param){

  q0 <- param[1]
  varq0 <- exp(3) # for identification assume known ???

  Q <- 6
  varad <- exp(param[2])

  a <- param[3]
  b <- param[4]

  q <- matrix(0,N,T)
  sd <- matrix(0,N,T)
  q[,1] <- q0
  sd[,1] <- varq0

  U <- matrix(0,N,T)
  Pr <- matrix(0,N,T)
  U[,1] <- a*q[,1] + b*price[,1]
  Pr[,1] <- exp(U[,1])/(1+exp(U[,1]))

  lik <- matrix(0,N,T)
  lik[,1] <- Choice[,1]*Pr[,1] + (1 - Choice[,1])*(1 - Pr[,1])

  for(t in 2:T){
    q[,t] <- (1 - ad[,t-1]) * q[,t-1] + # no advertisement
      ad[,t-1]*(q[,t-1]/sd[,t-1] + A[,t-1]/varad)/(1/sd[,t-1] + 1/varad)
    sd[,t] <- (1 - ad[,t-1]) * sd[,t-1] + # no advertisement
      ad[,t-1]*(1/(1/sd[,t-1] + 1/var_ad))

    U[,t] <- a * q[,t] + b*price[,t]
    Pr[,t] <- exp(U[,t])/(1 + exp(U[,t]))
    lik[,t] <- Choice[,t]*Pr[,t] + (1-Choice[,t])*(1-Pr[,t])
    }

  llik <- -sum(log(lik))
  return(llik)
}


# param<-c(0,0,0,0)

library(compiler)
estimate <- cmpfun(estimate)
fit<-nlm(estimate, param<-c(0,0,0,0), hessian=TRUE,print.level=2,iterlim = 1000)
se<-sqrt(diag(solve(fit$hessian)))
se














