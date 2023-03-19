library(Matrix)
library(MASS)

set.seed(123)

T <- 20
J <- 2 # number of products
I <- 10000

generateBLPdata <- function(){
  q <- 2 + matrix(rnorm(T*J),T) # q is the quality
  
  a <- matrix(c(1,0.5,0.5,1),nrow=2,ncol=2,byrow = TRUE)
  sig <- bdiag(replicate(2,a,simplify = FALSE))
  
  v <- mvrnorm(T,c(0,0,0,0),sig)
  ksaai <- v[,c(1,3)] # market-product shock, correlated with price
  
  z <- matrix(rnorm(T*J),T) # z is instrument for price
  p <- v[,c(2,4)] + z #p is the price to make p correlated with ksaai
  
  theta1 <- matrix(c(-2,1,-2),nrow=3) # linear part parameter
  vari <- 4
  Ind <- matrix(rnorm(I*1),I) #unobservable consumer characteristics
  htheta <- sqrt(vari)*Ind #htheta is the derivation from the mean for price parameters
  
  # market share
  s1 <- matrix(0,nrow = T)
  s2 <- matrix(0,nrow = T)
  for(t in 1:T){
    denom <- 1 + exp(as.vector(c(1,q[t,1],p[t,1])%*%theta1 + ksaai[t,1])+htheta*p[t,1]) + 
      exp(as.vector(c(1,q[t,2],p[t,2])%*%theta1+ksaai[t,2])+htheta*p[t,2])
    s1[t,1] <- colSums(exp(as.vector(c(1,q[t,1],p[t,1])%*%theta1+ksaai[t,1]) + htheta*p[t,1])/denom)/I
    s2[t,1] <- colSums(exp(as.vector(c(1,q[t,2],p[t,2])%*%theta1+ksaai[t,2])+htheta*p[t,2])/denom)/I
  }
  mylist <- list("market_share1"=s1,"market_share2"=s2,"price"=p,"quality"=q,"instrument"=z)
  return(mylist)
}

Data <- generateBLPdata()
S1 <- Data$market_share1
S2 <- Data$market_share2
P <- Data$price
Q <- Data$quality
Z <- Data$instrument

ksaai <- matrix(rnorm(T*J),T,J)
Ihtheta <- matrix(rnorm(I),I)

## define a function predict market share
predict_blp <- function(delta,theta2){
  pred_share1 <- matrix(0,nrow=T)
  pred_share2 <- matrix(0,nrow=T)
  sigma <- sqrt(theta2)
  delta <- matrix(delta,T,2)

  for(t in 1:T){
    denom <- 1 + exp(as.vector(delta[t,1]+Ihtheta*sigma*P[t,1])) +
      exp(as.vector(delta[t,2]) + Ihtheta*sigma*P[t,2])
    pred_share1[t,1] <- colSums(exp(as.vector(delta[t,1]) + Ihtheta*sigma*P[t,1])/denom)/I
    pred_share2[t,1] <- colSums(exp(as.vector(delta[t,2]) + Ihtheta*sigma*P[t,2])/denom)/I
  }
  return(as.matrix(c(pred_share1,pred_share2)))
}


# constraction mapping
contraction_blp <- function(theta2,theta1 = matrix(c(-1,0.5,-1),3),tol=1e-8,max_iter=100000){
  obs_s <- as.matrix(c(S1,S2))
  sigma <- sqrt(theta2)
  it <- 0
  x <- matrix(c(matrix(1,2*T,1),matrix(Q,2*T),matrix(P,2*T)),2*T)
  ini_delta <- x %*% theta1 + matrix(ksaai,2*T)
  delta_in <- ini_delta
  repeat{
    pred_s <- predict_blp(delta_in,theta2)
    delta <- delta_in + log(obs_s) - log(pred_s)
    if(max(abs(delta-delta_in)) < tol){
      break
    }
    delta_in <- delta
    it <- it + 1
    if(it > max_iter){
      break
    }
    print(it)
  }
  return(delta)
}

# construct objective function of theta2 to minimize
object_blp <- function(delta){
  x <- matrix(c(matrix(1,2*T,1),matrix(Q,2*T),matrix(P,2*T)),2*T)
  iv <- matrix(c(matrix(1,2*T,1),matrix(Q,2*T,1),matrix(Z,2*T,1),apply(matrix(Z,2*T,1),c(1,2),'^',2)),2*T)
  o <- iv %*% solve(t(iv)%*%iv) %*% t(iv)
  theta_1 <- solve(t(x)%*%o%*%x) %*% t(x)%*%o%*%delta # informs structural model equation 18
  mean_uti <- x%*%theta_1
  
  error <- delta - mean_uti
  GMM <- t(error)%*%o%*%error
  obj_list <- list("theta1"=theta_1,"mean.uti"=mean_uti,"error"=error,"GMMvalue"=GMM)
  return(obj_list)
}


gmm_blp <- function(theta2){
  delta <- contraction_blp(theta2)
  ob <- object_blp(delta)
  objective <- ob$GMMvalue
  return(objective)
}

thet2 <- 1
result <- optimize(gmm_blp,c(0,500)) # estimate thet2
est_thet2 <- result$minimum
f <- contraction_blp(est_thet2)
l <- object_blp(f)
est_thet1 <- l$theta1

cat("The estimated theta1 is: ",est_thet1)
cat("The estimated theta2 is: ",est_thet2)










