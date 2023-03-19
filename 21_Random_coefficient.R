
a <- -1
b.m <- 2
b.sd <- 1

i.dim <- 1000
t.dim <- 20

set.seed(999)

xv <- rnorm(t.dim,0,1)

bv <- b.m + b.sd * rnorm(i.dim,0,1)

u <- a + bv %o% xv


p <- 1/(1 + exp(-u))

rv <- runif(i.dim*t.dim,0,1)

dim(rv) <- c(i.dim,t.dim) 

yv <- ifelse(rv<=p,1,0)

n.sims <- 100

loglik <- function(b){
  
  exp.lik <- rep(0,i.dim) # likelihood
  
  set.seed(123)
  for(s in 1:n.sims){ # MonteCarlo Simulation
    sim.bv <- b[2] + b[3] * rnorm(i.dim,0,1)
    
    sim.u <- b[1] + sim.bv %o% xv
    
    sim.p <- 1/(1+exp(-sim.u))
    
    lik.it <- yv * sim.p + (1-yv)*(1-sim.p)
    
    exp.lik <- exp.lik + apply(lik.it,1,prod)/n.sims
  }
  sum(-log(exp.lik))
}

estimate.1 <- optim(b<-c(0,0,10),loglik,hessian=TRUE)

im <- solve(estimate.1$hessian)

estimate.1$par
