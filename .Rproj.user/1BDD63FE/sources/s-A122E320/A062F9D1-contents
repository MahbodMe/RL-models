
library(DEoptim)


### Parameter estimation symmetric kalman + UCB


rl_ucb <- function(choice, reward, n_options, initial_mean, initial_variance,
                   innovation_term, error_term) {
  
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(initial_mean, ncol = n_options, nrow = n_trials) 
  arm_variance <- matrix(initial_variance, ncol = n_options, 
                         nrow = n_trials)
  
  for (t in 1:(length(choice)-1)) {
    
    
    # Updating kalman gain values 
    kalman_t <- rep(0,n_options)
    kalman_t[choice[t]] <- (arm_variance[t,choice[t]] + innovation_term) / 
      (arm_variance[t,choice[t]] + innovation_term + error_term)
    
    
    arm_mean[t+1, ] <- arm_mean[t, ] + kalman_t*(reward[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-kalman_t)*(arm_variance[t, ]+innovation_term)
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance))
}


softmax_uncertain_choice_prob <- function(arm_mean, ppsd, gamma, beta) {
  prob <- exp(gamma*(arm_mean + ppsd*beta))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



rl_ucb_negLogLik <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- exp(par[3])
  error_term <- exp(par[4])
  beta <- par[5] 
  innovation_term <- 16
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, 4, initial_mean, initial_variance,
                innovation_term, error_term)
  
  arm_mean <- kal$arm_mean
  
  ppsd <- sqrt(kal$arm_variance + innovation_term)
  
  prob <- softmax_uncertain_choice_prob(arm_mean, ppsd, gamma, beta)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


est <- optim(c(.5, 0, log(1000), log(16), 1.96),rl_ucb_negLogLik, data=df_sim_2[1:200,])

pars <- c(est$par[1:2], exp(est$par[3:4]), est$par[5])

est2 <- optim(c(.5, 0, log(1000), log(16), 1.96),rl_ucb_negLogLik, data=df_sim_2[1:200,], method="SANN")

pars <- c(est2$par[1:2], exp(est2$par[3:4]), est2$par[5])



rl_ucb_negLogLik_2 <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- par[3]
  error_term <- par[4]
  beta <- par[5] 
  innovation_term <- 16
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, 4, initial_mean, initial_variance,
                innovation_term, error_term)
  
  arm_mean <- kal$arm_mean
  
  ppsd <- sqrt(kal$arm_variance + innovation_term)
  
  prob <- softmax_uncertain_choice_prob(arm_mean, ppsd, gamma)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


kf_sm_negLogLik_DE <- function(par,data,x) {
  negLL <- rl_ucb_negLogLik_2(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}

est_DE <- DEoptim(kf_sm_negLogLik_DE,lower=c(0,-100,0.00001,0.00001, 0),
                  upper=c(10,100,2000,100, 50),data=df_sim_3,
                  control=DEoptim.control(trace=FALSE))

fist_test <- est_DE$optim$bestmem

rl_ucb_negLogLik_2(fist_test, df_sim_3)




### Parameter estimation symmetric kalman + UCB + auto


rl_ucb_auto <- function(choice, reward, n_options, initial_mean, initial_variance,
                   innovation_term, error_term, beta, tau) {
  
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(initial_mean, ncol = n_options, nrow = n_trials + 1) 
  arm_variance <- matrix(initial_variance, ncol = n_options, 
                         nrow = n_trials + 1)
  
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials)
  
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
    
    
    
    # Updating the choice trace
    discount <- rep(0, n_options)
    discount[choice[t]] <- 1
    choice_trace[t+1, ] <- choice_trace[t,]*(1 - tau) + tau*discount
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance,
              choice_trace = choice_trace))
}


softmax_uncertain_auto_choice_prob <- function(arm_mean, ppsd, gamma, beta, ct) {
  prob <- exp(gamma*(arm_mean + ppsd*beta + ct))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



rl_ucb_auto_negLogLik <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- par[3]
  error_term <- par[4]
  beta <- par[5] 
  innovation_term <- 16
  tau <- par[6]
  phi <- par[7]
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb_auto(choice, reward, 4, initial_mean, initial_variance,
                innovation_term, error_term, beta, tau)
  
  arm_mean <- kal$arm_mean[1:(nrow(data)),]
  
  ppsd <- sqrt(kal$arm_variance[1:(nrow(data)),] + innovation_term)
  
  ct <- kal$choice_trace[1:(nrow(data)),]*phi
  
  prob <- softmax_uncertain_auto_choice_prob(arm_mean, ppsd, gamma, beta, ct)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


est <- optim(c(.5, 0, 1000, 16, 1.96, .5, 1),rl_ucb_auto_negLogLik, data=df_sim_3[1:200,])

pars <- c(est$par[1:2], exp(est$par[3:4]), est$par[5])

est2 <- optim(c(.5, 0, log(1000), log(16), 1.96),rl_ucb_negLogLik, data=df_sim_2[1:200,], method="SANN")

pars <- c(est2$par[1:2], exp(est2$par[3:4]), est2$par[5])


kf_sm_auto_negLogLik_DE <- function(par,data,x) {
  negLL <- rl_ucb_auto_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}


est_DE <- DEoptim(kf_sm_auto_negLogLik_DE,lower=c(0,-100,0.00001,0.00001, 0, .001, .001),
                  upper=c(10,100,2000,100, 50, 1, 100),data=df_sim_3,
                  control=DEoptim.control(trace=FALSE))


test <- est_DE$optim$bestmem
est_DE$optim$bestmem

rl_ucb_auto_negLogLik(test, data=df_sim_3[1:200,])

rl_ucb_auto_negLogLik(c( 7.4282223, 33.4874852, 76.9303825,  0.7334014,  1.9077357,  
                0.3387159,  0.8744183 ), data=df_sim_3[1:200,])
