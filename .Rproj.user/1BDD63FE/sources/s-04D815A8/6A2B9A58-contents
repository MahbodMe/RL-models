

rl_ucb <- function(choice, reward, n_options, initial_mean, initial_variance,
                   innovation_term, error_term, beta) {
  
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(initial_mean, ncol = n_options, nrow = n_trials + 1) 
  # Each arms variance of the posterior distribution of the mean utility 
  arm_variance <- matrix(initial_variance, ncol = n_options, 
                         nrow = n_trials + 1)
  
  for (t in 1:(length(choice)-1)) {
    
    
    discount <- rep(0,n_options)
    
    discount[choice[t]] <- 1

    # Updating kalman gain values 
    kalman_t <- rep(0,n_options)
    kalman_t <- (arm_variance[choice[t],] + innovation_term) / 
      (arm_variance[choice[t],] + innovation_term + error_term)
    
    
    arm_mean[t+1, ] <- arm_mean[t, ] + 
      discount*kalman_t*(reward[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-(discount*kalman_t))*(arm_variance[t, ]+innovation_term)
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance))
}



test <- rl_ucb(df_sim_1$deck, df_sim_1$payoff,4, 0, 1000, 16, 16, 1)
test


softmax_uncertain_choice_prob <- function(arm_mean, ppsd, gamma, beta) {
  prob <- exp(gamma*(arm_mean + ppsd*beta))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}

softmax_uncertain_choice_prob(arm_mean = test$arm_mean, 2, 2,2)


rl_ucb_negLogLik <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- par[3]
  error_term <- par[4]
  beta <- par[6] 
  innovation_term <- par[5]
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, 4, initial_mean, initial_variance,
                innovation_term, error_term, beta)
  
  arm_mean <- kal$arm_mean
  
  ppsd <- sqrt(kal$arm_variance + innovation_term)

  prob <- softmax_uncertain_choice_prob(arm_mean, ppsd, gamma, beta)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


gamma <- seq(0,1, length=300)
out <- rep(NA,length=length(gamma))
for (i in 1:length(gamma)) out[i] <- rl_sym_logLik(c(gamma[i], 0, 1000, 16, 1.96), df_sim_2)
plot(gamma,out,ylab="Neg-Log-likelihood",xlab=expression(gamma))


est <- optim(c(.5, 0, 1000, 16, 16, 1.96),rl_ucb_negLogLik,data=df_sim_2)




est <- optim(par=c(1, 0, 1000, 16, 16),fn=rl_ucb_negLogLik,lower=c(0,-Inf,0,0,0),data=df_sim_2)

estat <- read.csv("https://github.com/speekenbrink-lab/data/raw/master/Speekenbrink_Konstantinidis_2015.csv")
tdat <- subset(dat,id2==4)

est <- optim(c(1, 0, 1000, 16, 1.96),rl_ucb_negLogLik,data=tdat)




rl_ucb_negLogLik <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- exp(par[3])
  error_term <- exp(par[4])
  beta <- par[6] 
  innovation_term <- exp(par[5])
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, 4, initial_mean, initial_variance,
                innovation_term, error_term, beta)
  
  arm_mean <- kal$arm_mean
  
  ppsd <- sqrt(kal$arm_variance + innovation_term)
  
  prob <- softmax_uncertain_choice_prob(arm_mean, ppsd, gamma, beta)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}

rl_ucb_negLogLik <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- par[3]
  error_term <- par[4]
  beta <- par[6] 
  innovation_term <- par[5]
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, 4, initial_mean, initial_variance,
                innovation_term, error_term, beta)
  
  arm_mean <- kal$arm_mean
  
  ppsd <- sqrt(kal$arm_variance + innovation_term)
  
  prob <- softmax_uncertain_choice_prob(arm_mean, ppsd, gamma, beta)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


rl_ucb_negLogLik(c(.5, 0, 1000, 16, 16, 1.96), df_sim_2)
rl_ucb_negLogLik(c(.5, 0, 20*1000, 20*16, 20*16, 1.96), df_sim_2)


est <- optim(c(.5, 0, log(1000), log(16), log(16), 1.96),rl_ucb_negLogLik, data=df_sim_2)

c(est$par[1:2], exp(est$par[3:5]), est$par[6])




library(DEoptim)

kf_sm_negLogLik_DE <- function(par,data) {
  negLL <- kf_sm_negLogLik(par,data)
  print(1)
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}
est_DE <- DEoptim(rl_ucb_negLogLik,lower=c(0,-100,0.00001,0.00001,.00001, 0),
                  upper=c(10,100,2000,100,100, 50),data=df_sim_2,
                  control=DEoptim.control(trace=FALSE))
est_DE$optim$bestmem



###



rl_ucb <- function(choice, reward, n_options, initial_mean, initial_variance,
                   innovation_term, error_term) {
  
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(initial_mean, ncol = n_options, nrow = n_trials + 1) 
  # Each arms variance of the posterior distribution of the mean utility 
  arm_variance <- matrix(initial_variance, ncol = n_options, 
                         nrow = n_trials + 1)
  
  for (t in 1:(length(choice)-1)) {
    
    
    discount <- rep(0,n_options)
    
    discount[choice[t]] <- 1
    
    # Updating kalman gain values 
    kalman_t <- rep(0,n_options)
    kalman_t <- (arm_variance[choice[t],] + innovation_term) / 
      (arm_variance[choice[t],] + innovation_term + error_term)
    
    
    arm_mean[t+1, ] <- arm_mean[t, ] + 
      discount*kalman_t*(reward[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-(discount*kalman_t))*(arm_variance[t, ]+innovation_term)
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance))
}



test <- rl_ucb(df_sim_1$deck, df_sim_1$payoff,4, 0, 1000, 16, 16)
test


softmax_uncertain_choice_prob <- function(arm_mean, gamma, beta) {
  prob <- exp(gamma*arm_mean)
  prob <- prob/rowSums(prob)
  return (prob = prob)
}

softmax_uncertain_choice_prob(arm_mean = test$arm_mean, 2, 2,2)


rl_ucb_negLogLik <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- par[3]
  error_term <- par[4]
  innovation_term <- 16
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, 4, initial_mean, initial_variance,
                innovation_term, error_term, beta)
  
  arm_mean <- kal$arm_mean
  
  
  prob <- softmax_uncertain_choice_prob(arm_mean, gamma, beta)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


gamma <- seq(0,1, length=300)
out <- rep(NA,length=length(gamma))
for (i in 1:length(gamma)) out[i] <- rl_sym_logLik(c(gamma[i], 0, 1000, 16, 1.96), df_sim_2)
plot(gamma,out,ylab="Neg-Log-likelihood",xlab=expression(gamma))


rl_ucb_negLogLik(c(.5, 0, 1000, 16), df_sim_2)
rl_ucb_negLogLik(c(.5, 0, 20*1000, 20*16), df_sim_2)
rl_ucb_negLogLik(c(.5, 0, .1*1000, 2*16, .1*16, 1.96), df_sim_2)


est <- optim(c(.5, 0, 1000, 16),rl_ucb_negLogLik,data=df_sim_2,lower=c(0,-Inf,0,0,0))




est <- optim(par=c(1, 0, 1000, 16, 16),fn=rl_ucb_negLogLik,lower=c(0,-Inf,0,0,0),data=df_sim_2)

estat <- read.csv("https://github.com/speekenbrink-lab/data/raw/master/Speekenbrink_Konstantinidis_2015.csv")
tdat <- subset(dat,id2==4)

est <- optim(c(1, 0, 1000, 16, 1.96),rl_ucb_negLogLik,data=tdat)




rl_ucb_negLogLik <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- exp(par[3])
  error_term <- exp(par[4])
  beta <- par[6] 
  innovation_term <- exp(par[5])
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, 4, initial_mean, initial_variance,
                innovation_term, error_term, beta)
  
  arm_mean <- kal$arm_mean
  
  ppsd <- sqrt(kal$arm_variance + innovation_term)
  
  prob <- softmax_uncertain_choice_prob(arm_mean, ppsd, gamma, beta)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}

rl_ucb_negLogLik <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- par[3]
  error_term <- par[4]
  beta <- par[6] 
  innovation_term <- par[5]
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, 4, initial_mean, initial_variance,
                innovation_term, error_term, beta)
  
  arm_mean <- kal$arm_mean
  
  ppsd <- sqrt(kal$arm_variance + innovation_term)
  
  prob <- softmax_uncertain_choice_prob(arm_mean, ppsd, gamma, beta)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


rl_ucb_negLogLik(c(.5, 0, 1000, 16, 16, 1.96), df_sim_2)
rl_ucb_negLogLik(c(.5, 0, 20*1000, 20*16, 20*16, 1.96), df_sim_2)




### demo 
rl_sym_lik <- function(par, data){
  beta <- par[1]
  choice <- data$deck
  reward <- data$payoff
  qs <- rl_symmetric(choice, reward, 4, 0, .5)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  lik <- prod(prob_choice)
  return(list(lik))
}


beta <- seq(0,2, length=300)
out <- rep(NA,length=length(beta))
for (i in 1:length(beta)) out[i] <- rl_sym_lik(c(beta[i]), df_sim_1)
plot(beta,out,ylab="likelihood",xlab=expression(beta))


beta[which.max(out)]


rl_sym_negLogLik <- function(par, data){
  beta <- par[1]
  choice <- data$deck
  reward <- data$payoff
  qs <- rl_symmetric(choice, reward, 4, 0, .5)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}

beta <- seq(0,3, length=300)
out <- rep(NA,length=length(beta))
for (i in 1:length(beta)) out[i] <- rl_sym_negLogLik(c(beta[i]), df_sim_1)
plot(beta,out,ylab="likelihood",xlab=expression(beta))
beta[which.min(out)]


optim(par=.25,fn=rl_sym_negLogLik,data=df_sim_1,)

optim(par=3,fn=rl_sym_negLogLik,method="Brent",lower=0,upper=8,data=df_sim_1)

