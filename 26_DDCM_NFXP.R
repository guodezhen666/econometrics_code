library(ggplot2)
library(msm)


############################
####### Data Simulation
############################

lower <- 0 
upper <- 15000
mu <- 6000
sigma <- 4000

p_x0 <- ptnorm(5000,mu,sigma,lower,upper)
p_x1 <- ptnorm(10000,mu,sigma,lower,upper) - p_x0

p_x2 <- 1 - p_x1 - p_x0

# p[1] chance of keeping the same mileage
# p[2] chance of increasing the mileage state by 1
# p[3] chance of increasing the mileage state by 2
p <- c(p_x0,p_x1,p_x2)

p

# Initialization of the variables

# the replacement cost
# the maintainance cost function parameters and discout rate

rc <- 20
theta1_1 <- 0.5
theta1_2 <- 0.01

beta <- 0.75

# Defination fo the cost function

myopic_costs <- function(S,MF,params,p){
  
  # S possible state of the bus
  # MF maitenance cost function MF (state as argument )

  rc <- params[1]
  maint_cost <- rep(NA,S)
  repl_cost <- rep(NA,S)
  
  for(s in 1:S){
    maint_cost[s] <- MF(s,params[-1])
    repl_cost[s] <- rc
  }
    
  cbind(maint_cost,repl_cost)
}

# different possible forms for the maintenance cost function

lin_cost <- function(s,params) s*params[1]
quad_cost <- function(s,params) s*params[1] + s^2*params[2]
exp_cost <- function(s,params) exp(s*params[1])
log_cost <- function(s,params) log(params[1] + s*params[2])


# Choic of probabilities

choice_prob <- function(cost_array){
  S <- nrow(cost_array)
  cost <- cost_array - apply(cost_array,1,min)
  util <- exp(-cost)
  pchoice <- util/rowSums(util)
  
  pchoice
}


# Contaction Mapping
contraction_mapping <- function(S,p,MF,params,beta=0.75,threshold=1e-6,suppr_output=FALSE){
  
  achieved <- TRUE
  ST_mat <- matrix(0,S,S) # state transition matrix
  lp <- length(p)
  for(i in 1:S){
    for(j in 1:lp){
      if((i+j-1)<S) ST_mat[i,i+j-1] <- p[j]
      if((i+j-1)==S) ST_mat[i,i+j-1] <- sum(p[j:lp])
    }
  }
  
  R_mat <- cbind(1,matrix(0,S,S-1)) # replace matrix
  
  k <- 0
  EV <- matrix(0,S,2)
  EV_myopic <- EV_new <- myopic_costs(S,MF,params,p)
  
  while(max(abs(EV_new-EV)) > threshold){
    EV <- EV_new
    pchoice <- choice_prob(EV)
    ecost <- rowSums(pchoice*EV)
    
    futil_maint <- ST_mat %*% ecost
    futil_repl <- R_mat %*% ecost
    
    futil <- cbind(futil_maint,futil_repl)
    
    # Future utility is discouted by beta and added to the myoptic cost
    EV_new <- EV_myopic + beta*futil
    k <- k + 1
    if(k==1000) achieved <- FALSE
  }
  if(!suppr_output){
    if(achieved){
      cat("Convergence achieved in ",k," iterations")
    }else{
      cat("CM couldn't converge! Mean difference = ",round(mean(EV_new-EV),2))
    }
  }
  
  list(CP_forward=choice_prob(EV_new),CP_myopic=choice_prob(EV_myopic))
  
}


rc <- 20
theta11 <- 0.5
theta12 <- 0.01
beta <- 0.75

p_x0 <- 0.36
p_x1 <- 0.48
p_x2 <- 0.16

# 2) Choice probabilitier a function of state
params_lin <- c(rc,theta11)
p <- c(p_x0,p_x1,p_x2)
S <- 70

out <- contraction_mapping(S=S,p=p,MF=lin_cost,params=params_lin,beta=0.75)
lin_forward <- out$CP_forward
lin_myopic <- out$CP_myopic

pchoice <-  lin_forward[,1]

ggdat1 <- data.frame(decisionRule=c(rep("Forward-Looking",nrow(lin_forward)),
                                    rep("Myopic",nrow(lin_forward))),
                     pMaint=c(lin_forward[,1],lin_myopic[,1]),
                     State=rep(1:S,times=2))

ggplot(ggdat1,aes(y=pMaint,x=State,color=decisionRule)) + geom_line(lwd=1) + theme_bw(20) + xlim(5,50)

# 3) bus replacement dataset

transition <- function(bus_df,pchoice){
  
  nbus <- max(bus_array$id)
  obs <- nrow(bus_array)
  
  ind <- (obs-nbus+1):obs
  
  prev_mileage <- bus_array$mileage[ind]
  prev_states <- bus_array$state[ind]
  
  choices <- (runif(nbus)>pchoice[prev_states+1])*1
  
  new_mileage <- (1-choices)*prev_mileage + rtnorm(nbus,mu,sigma,lower,upper)
  
  new_states <- floor(new_mileage/5000)
  mew_array <- data.frame(id=1:nbus,choice=rep(0,nbus),mileage=new_mileage,state=new_states)
  
  bus_array$choice[ind] = choices
  
  rbind(bus_array,new_array)
  
}


bus_dgp <- function(nbus,nperiod,pchoice){
  
  nobs <- nbus*nperiod
  bus_df <- data.frame(id=rep(1:nbus,times=nperiod),
                       choice=rep(NA,nobs),
                       mileage=rep(NA,nobs),
                       state=rep(NA,nobs))
  
  bus_df[1:nbus,1] <- 1:nbus
  bus_df[1:nbus,-1] <- 0
  
  i <- 1
  while(i<nobs){
    prev_ind <- seq(from=i,by=1,length.out = nbus)
    next_ind <- prev_ind + nbus
    
    prev_mileage <- bus_df$mileage[prev_ind]
    prev_state <- bus_df$state[prev_ind]
    
    choices <- (runif(nbus)>pchoice[prev_state+1])*1
    bus_df$choice[prev_ind] <- choices
    
    if(next_ind[1]<nobs){
      
      new_mileage <- (1-choices)*prev_mileage + rtnorm(nbus,mu,sigma,lower,upper)
      new_state <- floor(new_mileage/5000)
      
      bus_df$mileage[next_ind] <- new_mileage
      bus_df$state[next_ind] <- new_state
    }
    i <- i + nbus
  }
  bus_df
}

nbus <- 1000
nperiod <- 100
bus_df_lin <- bus_dgp(nbus,nperiod,pchoice)

choice_freq <- aggregate(choice~state,data=bus_df_lin,FUN=mean)

ggdat2 = data.frame(dataType=c(rep("Observed behavior",nrow(choice_freq)),
                               rep("True choice probabilities",length(pchoice))),
                    pMaint=c(1-choice_freq$choice,pchoice),
                    State=c(choice_freq$state,0:(S-1)))



#######################################
########### Estimation
#######################################

DynamicLogit <- function(params,data,S,p,MF){
  
  endog <- data$choice
  exog <- data$state
  
  N <- length(endog)
  S <- max(exog)*2
  
  state_mat <- matrix(0,S,N)
  for(s in 0:(S-1)) state_mat[s+1,] = (exog == s)*1
  
  dec_mat <- rbind(t(1-endog),endog)
  
  util <- contraction_mapping(S=S,p=p,MF=MF,params = params,beta=.75,suppr_output = TRUE)
  
  pchoice <- util$CP_forward
  logprob <- log(t(state_mat)%*%pchoice)
  
  -sum(logprob*t(dec_mat))
}

bounds <- c(1e-6,Inf)
npars <- 2
linfit <- optim(par = rep(.1,npars),fn=DynamicLogit,method = c("L-BFGS-B"),lower=bounds[1],
                upper=bounds[2],data=data,S=S,p=p,MF=lin_cost,control = list(fnscale=1))

# Return the parameters obtained after fitting the liklihood function to the data
loglike <- linfit$value
fit_params <- linfit$par

cat("Log-Likelihood:",loglike,fill=TRUE)
cat("RC:",fit_params[1],fill=TRUE)
cat("thetas: ",fit_params[-1],fill=TRUE)


params_lin <- c(20,.5)
linEst <- contraction_mapping(S=70,p=p,MF=lin_cost,params = fit_params,beta=0.75)
lin_forwardEst <- linEst$CP_forward
lin_myopicEST <- linEst$CP_myopic

gglinEst = data.frame(decisionRule=c(rep("Forward-Looking (Lin)",nrow(lin_forward)),
                                     rep("Myopic (Lin)",nrow(lin_myopic)),
                                     rep("Forward-Looking (Lin Est.)",nrow(lin_forwardEst)),
                                     rep("Myopic (Lin Est.)",nrow(lin_myopicEst))),
                      pMaint=c(lin_forward[,1],lin_myopic[,1],lin_forwardEst[,1],lin_myopicEst[,1]),
                      State=c(0:(nrow(lin_forward)-1),0:(nrow(lin_myopic)-1),0:(nrow(lin_forwardEst)-1),0:(nrow(lin_myopicEst)-1)))

ggplot(gglinEst,aes(y=pMaint,x=State,color=decisionRule))+geom_line(lwd=1)+theme_bw(20)+xlim(5,50)












