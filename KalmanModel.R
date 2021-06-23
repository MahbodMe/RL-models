
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






