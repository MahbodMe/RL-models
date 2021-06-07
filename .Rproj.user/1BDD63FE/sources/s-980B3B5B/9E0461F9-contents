


### Symmetric Simple Q learning model + unceraitnty (UCB)


library(nnet)

rl_ucb_sim <- function(rewards, intial_mean, intial_variance, innovation_term, 
                        error_term, beta) {
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials 
  arm_mean <- matrix(intial_mean, ncol = n_options, nrow = n_trials + 1)
  arm_variance <- matrix(intial_variance, ncol = n_options, 
                         nrow = n_trials + 1)
  
  ucb_values <- rep(0,4)
  kalman_t <- rep(0,4)
  discount <- rep(0,4)
  
  for (t in 1:n_trials) {

    for (a in 1: n_options){
      ucb_values[a] <-  (beta*arm_mean[t,a])/sum((beta*arm_mean[t,])) 
    }
    
    choices[t] <- which.is.max(ucb_values) 
    
    if (t == 1){
      choices[t] <- sample(c(1,2,3,4), 1)
    }
    

    agents_rewards[t] <- rewards[t, choices[t]+1]
    kalman_t[a] <- (arm_variance[a] + innovation_term) / 
      (arm_variance[a] + innovation_term + error_term)
    
    
    print("choice")
    print(choices[t])
    for (a in 1:n_options){
      if (a == choices[t]){
        discount[a] <- 1

      } else {
        discount[a] <- 0
      }
    }
    
    
    arm_mean[t+1, ] <- arm_mean[t, ] + 
      discount*kalman_t*(agents_rewards[t]-arm_mean[t+1, ])
    print("trial + arm_mean")
    print(t)
    print(arm_mean[t,])
    print(arm_variance[t,])
    
    arm_variance[t+1, ] <- (1-kalman_t)*(arm_variance[t, ]+innovation_term)


    
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance, 
              choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  
}



sim_4 <- rl_ucb_sim(rewards , 0.0, 1000, 16, 16, .2)






### Symmetric Simple Q learning model + unceraitnty (UCB) + exploration bonus


library(nnet)

rl_ucb_sim_expB <- function(rewards, intial_mean, intial_variance, innovation_term, 
                       error_term, beta, beta2) {
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials 
  arm_mean <- matrix(intial_mean, ncol = n_options, nrow = n_trials + 1)
  arm_variance <- matrix(intial_variance, ncol = n_options, 
                         nrow = n_trials + 1)
  
  ucb_values <- rep(0,4)
  kalman_t <- rep(0,4)
  discount <- rep(0,4)
  exploration_bonus <- rep(0,4)
  
  for (t in 1:n_trials) {
    
    for (a in 1: n_options){
      ucb_values[a] <-  arm_mean[t,a]/sum(arm_mean[t,]) 
    }
    
    choices[t] <- which.is.max(ucb_values) 
    
    if (t == 1){
      choices[t] <- sample(c(1,2,3,4), 1)
    }
    
    
    agents_rewards[t] <- rewards[t, choices[t]+1]
    kalman_t[a] <- (arm_variance[a] + innovation_term) / 
      (arm_variance[a] + innovation_term + error_term)
    
    
    print("choice")
    print(choices[t])
    for (a in 1:n_options){
      if (a == choices[t]){
        discount[a] <- 1
        
      } else {
        discount[a] <- 0
      }
    }
    
    
    exploration_bonus <- beta2*sqrt(arm_variance[t,]+innovation_term)
    
    arm_mean[t+1, ] <- arm_mean[t, ] + 
      discount*kalman_t*(agents_rewards[t]-arm_mean[t+1, ]) + exploration_bonus
    print("trial + arm_mean")
    print(t)
    print(arm_mean[t,])
    
    
    arm_variance[t+1, ] <- (1-kalman_t)*(arm_variance[t, ]+innovation_term)
    
    
    
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance, 
              choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  
}

sim_5 <- rl_ucb_sim_expB(rewards , 0.0, 1000, 16, 16, 1, 1.96)



### Symmetric Simple Q learning model + uncertainty (UCB) 
### + exploration bonus + autocorrlation 


library(nnet)

rl_ucb_sim_expB_choiceTrace <- function(rewards, intial_mean, intial_variance, innovation_term, 
                            error_term, beta, beta2, phi, tau){
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials 
  arm_mean <- matrix(intial_mean, ncol = n_options, nrow = n_trials + 1)
  arm_variance <- matrix(intial_variance, ncol = n_options, 
                         nrow = n_trials + 1)
  
  ucb_values <- rep(0,4)
  kalman_t <- rep(0,4)
  discount <- rep(0,4)
  exploration_bonus <- rep(0,4)
  choice_trace <- rep(0,4)
  
  for (t in 1:n_trials) {
    
    for (a in 1: n_options){
      ucb_values[a] <-  arm_mean[t,a]/sum(arm_mean[t,]) 
    }
    
    choices[t] <- which.is.max(ucb_values) 
    
    if (t == 1){
      choices[t] <- sample(c(1,2,3,4), 1)
    }
    
    
    agents_rewards[t] <- rewards[t, choices[t]+1]
    kalman_t[a] <- (arm_variance[a] + innovation_term) / 
      (arm_variance[a] + innovation_term + error_term)
    
    
    print("choice")
    print(choices[t])
    for (a in 1:n_options){
      if (a == choices[t]){
        discount[a] <- 1
        
      } else {
        discount[a] <- 0
      }
    }
    
    
    indicator <- rep(0,4)
    for (a in 1:4){
      if (a == choices[t]){
        indicator[a] <- 1
      } else { 
        indicator[a] <- 0
      }
    }
    choice_trace <- choice_trace - tau*choice_trace + tau*indicator
    
    exploration_bonus <- beta2*sqrt(arm_variance[t,]+innovation_term)
    
    arm_mean[t+1, ] <- arm_mean[t, ] + 
      discount*kalman_t*(agents_rewards[t]-arm_mean[t+1, ]) + 
      exploration_bonus + choice_trace
    print("trial + arm_mean")
    print(t)
    print(arm_mean[t,])
    
    
    arm_variance[t+1, ] <- (1-kalman_t)*(arm_variance[t, ]+innovation_term)
    
    
    
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance, 
              choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  
}

  
sim_6 <- rl_ucb_sim_expB_choiceTrace(rewards , 0.0, 1000, 16, 16, 1,
                                     1.96, .83, 1)


