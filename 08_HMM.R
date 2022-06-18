
n <- 2
T <- 500
I <- 40

beta <- matrix(c(1,2,-8,-8+exp(1)),2,2,1)

rho <- c(2,4)

mu <- c(3,-1)

initial_pie <- c(0.7,0.3)

W <- matrix(rnorm(T*I)+3,T,I,1)

R <- matrix(rnorm(T*I)*0.5+0.5,T,I,1)

Y <- matrix(-1,T,I)

state <- array(0,dim=c(T,I,n))

#########################################
########## simulate Data
#########################################

for(i in 1:I){
  class_dict <- initial_pie
  for(t in 1:T){
    state[t,i,] <- rmultinom(1,1,class_dict)
    u <- exp(c(W[t,i],1) %*% beta %*% state[t,i,])
    p <- u/(1+u)
    random <- runif(1,0,1)
    Y[t,i] <- if(random<p) 1 else 0
    
    # transition matrix
    transition <- matrix(0,n,n)
    u1 <- exp(mu[1] - R[t,i]*rho[1])
    transition[1,2] <- 1 - u1/(1+u1)
    transition[1,1] <- u1/(1+u1)
    
    # last row
    un <- exp(mu[2*n-2] - R[t,i]*rho[n])
    transition[n,n-1] <- un/(1+un)
    transition[n,n] <- 1 - un/(1+un)
    
    class_dict <- t(state[t,i,]) %*% transition
  }
}

######################################
##############  Estimate
######################################

estimate <- function(b){
  beta <- matrix(c(b[1],b[2],b[3],b[3]+exp(b[4])),2,2,1)

  rho <- c(b[5],b[6])

  mu <-c(b[7],b[8])
  like <- matrix(0,I,1)

  for(i in 1:I){
    initial_pie <- c(0.7,0.3)
    likelihood <- matrix(initial_pie,1,2)
    foo <- matrix(initial_pie,1,2)
    lscale <- 0
    transition_matrix <- array(0,c(T,n,n))
    for(t in 1:T){
      code_lik <- matrix(0,n,n)
      for(j in 1:n){
        u <- exp(c(W[t,i],1) %*% beta[,j])
        p <- u/(1+u)
        code_lik[j,j] <- p*(Y[t,i]) + (1-p)*( 1 - Y[t,i])
      }
      # 1st row
      transiton <- matrix(0,n,n)
      u1 <- exp(mu[1] - R[t,i]*rho[1])
      transition[1,1+1] <- 1 - u1/(1+u1)
      transition[1,1] <- u1/(1+u1)

      # last row
      un <- exp(mu[2] - R[t,i]*rho[2])
      transition[n,n-1] <- un/(1+un)
      transition[n,n] <- 1 - un/(1+un)
      
      transition_matrix[t,,] <- transition
      # likelihood <- likelihood  %*% code_lik %*% transition # professor Tan
      # if(t!=T){ 
      #   likelihood <- likelihood  %*% code_lik %*% transition
      # }else{
      #   likelihood <- likelihood  %*% code_lik 
      # } # my approach
      
      # avoid under flow
      # if(t == 1){
      #   foo <- matrix(initial_pie,1,n) %*% code_lik
      # }else{
      #   foo <- foo %*% transition_matrix[t-1,,] %*% code_lik
      # }
      if(t == 1){
        foo <- foo %*% code_lik
      }else{
        foo <- foo %*% transition_matrix[(t-1),,] %*% code_lik
      }
      
      sumfoo <- sum(foo)
      lscale <- lscale + log(sumfoo)
      foo <- foo/sumfoo
    }
    # like[i,1] <- sum(likelihood)
    # like[i,1] <- sum(likelihood)
    like[i,1] <- lscale
    # like[i,1] <- sum(foo)
  }
  return(-sum((like)))
  # return(-sum(log(like)))

}


library(compiler)
estimate <- cmpfun(estimate)

b <- rep(0,8)
# true_b <- c(2,3,-8,1,2,4,3,-1)
result <- nlm(estimate, b, hessian = TRUE, print.level = 2, iterlim = 1000)
result$estimate
# result$hessian
