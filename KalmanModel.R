
library(nnet)

### Symmetric Simple Q learning model + unceraitnty (UCB) + exploration bonus


rl_ucb_sim_expB <- function(rewards, initial_mean, initial_variance, 
                            innovation_term, error_term, gamma, beta) {
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  # Each arms mean utility 
  arm_mean <- matrix(initial_mean, ncol = n_options, nrow = n_trials + 1) 
  # Each arms variance of the posterior distribution of the mean utility 
  arm_variance <- matrix(initial_variance, ncol = n_options, 
                         nrow = n_trials + 1)
  
  ucb_values <- rep(0,n_options) # Contains UCB value for each arm
  
  exploration_bonus <- rep(0,n_options)  # exploration bonus
  
  for (t in 1:n_trials) {
    
    ucb_values <- exp((gamma*arm_mean[t,] + exploration_bonus))
    
    ucb_values <- ucb_values / sum(ucb_values) 
    
    choices[t] <- which.is.max(ucb_values) # taking the highest value
    
    if (t == 1){
      # because of an error the first trial is set to randomly select
      choices[t] <- sample(rep(1:n_options,1),1)
    }
    
    
    agents_rewards[t] <- rewards[t, choices[t]+1] # Agents reward at trial t
    
    kalman_t <- rep(0,n_options) # Contains Kalman gain for each arm
    # Updating kalman gain values 
    kalman_t <- (arm_variance[choices[t],] + innovation_term) / 
      (arm_variance[choices[t],] + innovation_term + error_term)
    

    # setting discount value 
    
    discount <- rep(0,n_options)
    discount[choices[t]] <- 1
    
    # Updating exploration bonus value
    exploration_bonus <- beta*sqrt(arm_variance[t,]+innovation_term)

    # Updating mean utility of each arm
    arm_mean[t+1, ] <- arm_mean[t, ] + 
      discount*kalman_t*(agents_rewards[t]-arm_mean[t, ]) 

    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-(discount*kalman_t))*(arm_variance[t, ]+innovation_term)
    
    print(arm_variance[t+1, ])
    
    
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance, 
              choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  
}

sim_7 <- rl_ucb_sim_expB(rewards , 0.0, 1000, 16, 16, 1, 1.96)


### Symmetric Simple Q learning model + uncertainty (UCB) 
### + exploration bonus + autocorrlation 


rl_ucb_sim_expB_choiceTrace <- function(rewards, initial_mean, initial_variance, 
                                        innovation_term, error_term, gamma, 
                                        beta, phi, tau){
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  # Each arms mean utility 
  arm_mean <- matrix(initial_mean, ncol = n_options, nrow = n_trials + 1) 
  # Each arms variance of the posterior distribution of the mean utility 
  arm_variance <- matrix(initial_variance, ncol = n_options, 
                         nrow = n_trials + 1)
  
  ucb_values <- rep(0,n_options) # Contains UCB value for each arm
  kalman_t <- rep(0,n_options) # Contains Kalman gain for each arm
  exploration_bonus <- rep(0,n_options)  # exploration bonus
  
  # Choice trace for each arm at trial t
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials + 1)

  
  for (t in 1:n_trials) {
    
    ucb_values[] <-(gamma*arm_mean[t,] + exploration_bonus + choice_trace[t,])  
    
    ucb_values<- ucb_values / sum(ucb_values) 
    
    choices[t] <- which.is.max(ucb_values) # taking the highest value
    
    if (t == 1){
      # because of an error the first trial is set to randomly select
      choices[t] <- sample(rep(1:n_options,1),1)
    }
    
    
    agents_rewards[t] <- rewards[t, choices[t]+1] # Agents reward at trial t
    
    # Updating kalman gain values 
    kalman_t[choices[t]] <- (arm_variance[choices[t]] + innovation_term) / 
      (arm_variance[choices[t]] + innovation_term + error_term)
    
    # setting discount value 
    
    discount <- rep(0,n_options)
    discount[choices[t]] <- 1
    
    # Updating exploration bonus value
    exploration_bonus <- beta*sqrt(arm_variance[t,]+innovation_term)
    
    
    # Updating mean utility of each arm
    arm_mean[t+1, ] <- arm_mean[t, ] + 
      discount*kalman_t*(agents_rewards[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1 - (discount*kalman_t)) * (arm_variance[t, ] +
                                                      innovation_term)
    
    # Choice Trace update
    
    indicator <- rep(0,n_options)
    indicator[choices[t]] <- 1
    
    choice_trace[t+1, ] <- choice_trace[t,] - tau*choice_trace[t,] + tau*indicator
    
    
    
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance, 
              choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  
}




sim_8 <- rl_ucb_sim_expB_choiceTrace(rewards , 0.0, 1000, 16, 16, 1,
                                     1.96, 0.02, 1)







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
  
  kal <- rl_ucb(choice, reward, 2, initial_mean, initial_variance,
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

# the remaining functions are the negloglik without the transformation 
# suitable for DEoptim 

rl_ucb_negLogLik_2 <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- par[3]
  error_term <- par[4]
  beta <- par[5] 
  innovation_term <- 16
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, 2, initial_mean, initial_variance,
                innovation_term, error_term)
  
  arm_mean <- kal$arm_mean
  
  ppsd <- sqrt(kal$arm_variance + innovation_term)
  
  prob <- softmax_uncertain_choice_prob(arm_mean, ppsd, gamma, beta)
  
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



