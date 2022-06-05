######################################
######### Simulated Data
######################################
set.seed(123)

n <- 100
Time <- 50
s <- 3 # number of state

transition_beta <- matrix(c(1,-1,1,-1,1,-2,2,-2,1,-1,-1,1),s,4)
choose_beta <- matrix(c(1,1,1.2,1,2,2,1.5,3,3),s,3)

xv <- array(rnorm(n*Time*3,0,1),c(n,Time,3))
transition_matrix <- array(0,c(n,Time,s,s))

xv_2 <- array(rnorm(n*Time*3,0,1),c(n,Time,3))

mu_1 <- rnorm(n,0,1)
mu_2 <- rnorm(n,5,4)
trans <- matrix(0,n,3)

# order_logit for transition matrix
# too ugly
for (time in 1:Time){
  for(i in 1:n){
    for(x in 1:s){
      if(x==1){
        u <- mu_2[i] - (transition_beta[x,1] + xv[i,time,] %*% transition_beta[x,2:4] + rlogis(1,0,1))
        p_up <- 1 - exp(u)/(1 + exp(u))
        random <- runif(1,0,1)
        if(random < p_up){
          transition_matrix[i,time,x,2] <- 1
        }else{
          transition_matrix[i,time,x,1] <- 1
        }
      }else if(x==2){
        u_up <- mu_2[i] - (transition_beta[x,1] + xv[i,time,] %*% transition_beta[x,2:4] + rlogis(1,0,1))
        p_up <- 1 - exp(u_up)/(1 + exp(u_up))
        u_down <- mu_1[i] - (transition_beta[x,1] + xv[i,time,] %*% transition_beta[x,2:4] + rlogis(1,0,1))
        p_down <- exp(u_down)/(1+exp(u_down))
        random <- runif(1,0,1)
        if(random < p_up){
          transition_matrix[i,time,x,3] <- 1
        }else if(random > p_up & random < p_down + p_up){
          transition_matrix[i,time,x,1] <- 1
        }else{
          transition_matrix[i,time,x,2] <- 1
        }
      }else if(x==3){
        u_down <- mu_1[i] - (transition_beta[x,1] + xv[i,time,] %*% transition_beta[x,2:4] + rlogis(1,0,1))
        p_down <- exp(u_down)/(1+exp(u_down))
        random <- runif(1,0,1)
        if(random<p_down){
          transition_matrix[i,time,x,2] <- 1
        }else{
          transition_matrix[i,time,x,3] <- 1
        }
      }
    }
  }
}

yv <- matrix(-1,n,Time)

# pai
pai <- sample(x=1:3,size=n,replace = TRUE)
pai_matrix <- matrix(0,n,s)
for(i in 1:n){
  pai_matrix[i,pai[i]] <- 1
}
pai_matrix

for (time in 1:Time){
  for(i in 1:n){
    if(time==1){
      state <- pai_matrix[i,]%*%transition_matrix[i,time,,]
    }else{
      state <- state %*% transition_matrix[i,time,,]
    }
    u <- choose_beta[t(state),] %*% xv_2[i,time,]
    p <- exp(u)/(1+exp(u))
    random <- runif(1,0,1)
    if(random < p){
      yv[i,time] <-1 
    }else{
      yv[i,time] <- 0
    }
  }
}


