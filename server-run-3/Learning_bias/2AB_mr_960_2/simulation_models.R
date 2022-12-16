# Q_learning Models for simulation

### Symmetric Simple Q learning model 

sym_sim <- function(rewards, par) {
  
  beta <- par[1]
  alpha <- par[2]
  q0 <- par[3]
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(q0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- sample(1:n_options)
  options <- rep(1:n_options, 1)
  
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*q_values[t,]) 
    
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choices[t] <- select_randomly[t]
    } else {
      choices[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    # if (t < n_options){
    #   # because of an error the first trial is set to randomly select
    #   choices[t] <- sample(select_randomly, 1)
    #   select_randomly <- select_randomly[!select_randomly %in% choices[t]]
    #   
    # } 
    # if (t == n_options) {
    #   choices[t] <- select_randomly
    # }
    
    agents_rewards[t] <- rewards[t, choices[t]+1] # Agents reward at trial t
    
    
    # setting discount value, since the forgetting rate is 0, 
    # only updates the chosen arm will be updated
    
    discount <- rep(0,n_options)
    discount[choices[t]] <- 1
    
    q_values[t+1,] <- q_values[t,] + discount*alpha*(agents_rewards[t] - 
                                                       q_values[t,])
    
  }
  return(list(q_values = q_values, choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards) ))
  
}




### Asymmetric Simple Q learning model (RPE)

asym_rpe_sim <- function(rewards, par){
  
  beta <- par[1]
  positive_alpha <- par[2]
  negative_alpha <- par[3]
  q0 <- par[4]
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(q0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- sample(1:n_options)
  options <- rep(1:n_options, 1)
  
  
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*q_values[t,]) 
    
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choices[t] <- select_randomly[t]
    } else {
      choices[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    # if (t < n_options){
    #   # because of an error the first trial is set to randomly select
    #   choices[t] <- sample(select_randomly, 1)
    #   select_randomly <- select_randomly[!select_randomly %in% choices[t]]
    #   
    # }
    # if (t == n_options) {
    #   choices[t] <- select_randomly
    # }
    
    agents_rewards[t] <- rewards[t, choices[t]+1] # Agents reward at trial t
    
    
    # setting discount value, since the forgetting rate is 0, 
    # only updates the chosen arm will be updated
    
    discount <- rep(0,n_options)
    discount[choices[t]] <- 1
    
    if ((agents_rewards[t] - q_values[t,choices[t]]) < 0){
      alpha <- negative_alpha 
    } else {
      alpha <- positive_alpha
    }
    
    q_values[t+1,] <- q_values[t,] + discount*alpha*(agents_rewards[t] - 
                                                       q_values[t,])
    
  }
  return(list(q_values = q_values, choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  # Returning variables of interest 
}




### Asymmetric Simple Q learning model (actual rewards)

asym_reward_sim <- function(rewards, par){
  beta <- par[1]
  positive_alpha <- par[2]
  negative_alpha <- par[3]
  q0 <- par[4]
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(q0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- sample(1:n_options)
  
  options <- rep(1:n_options, 1)
  
  
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*q_values[t,]) 
    
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choices[t] <- select_randomly[t]
    } else {
      choices[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    # if (t < n_options){
    #   # because of an error the first trial is set to randomly select
    #   choices[t] <- sample(select_randomly, 1)
    #   select_randomly <- select_randomly[!select_randomly %in% choices[t]]
    #   
    # }
    # if (t == n_options) {
    #   choices[t] <- select_randomly
    # }
    
    agents_rewards[t] <- rewards[t, choices[t]+1] # Agents reward at trial t
    
    
    # setting discount value, since the forgetting rate is 0, 
    # only updates the chosen arm will be updated
    
    discount <- rep(0,n_options)
    discount[choices[t]] <- 1
    
    if ((agents_rewards[t]) < 0){
      alpha <- negative_alpha 
    } else {
      alpha <- positive_alpha
    }
    
    q_values[t+1,] <- q_values[t,] + discount*alpha*(agents_rewards[t] - 
                                                       q_values[t,])
    
  }
  return(list(q_values = q_values, choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  # Returning variables of interest 
}






### Symmetric Simple Q learning model + autocorrlation



sym_auto_sim <- function(rewards, par) {
  
  beta <- par[1]
  alpha <- par[2]
  tau <- par[3]
  phi <- par[4]
  q0 <- par[5]
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(q0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- sample(1:n_options)
  options <- rep(1:n_options, 1)
  # Choice trace for each arm at trial t
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  
  
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*(q_values[t,] + phi*choice_trace[t+1,]))
    
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choices[t] <- select_randomly[t]
    } else {
      choices[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    # 
    # 
    # if (t < n_options){
    #   # because of an error the first trial i
    #   choices[t] <- sample(select_randomly, 1)
    #   select_randomly <- select_randomly[!select_randomly %in% choices[t]]
    #   
    # }
    # if (t == n_options) {
    #   choices[t] <- select_randomly
    # }
    
    
    agents_rewards[t] <- rewards[t, choices[t]+1] # Agents reward at trial t
    
    
    # setting discount value, since the forgetting rate is 0, 
    # only updates the chosen arm will be updated
    
    discount <- rep(0,n_options)
    discount[choices[t]] <- 1
    
    q_values[t+1,] <- q_values[t,] + discount*alpha*(agents_rewards[t] - 
                                                       q_values[t,])
    # Choice Trace update
    
    indicator <- rep(0,n_options)
    indicator[choices[t]] <- 1
    
    choice_trace[t+1, ] <- choice_trace[t,] - tau*choice_trace[t,] + tau*indicator
    
  }
  return(list(q_values = q_values, choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  
}




### Asymmetric Simple Q learning model (RPE) + autocorrlation

asym_rpe_auto_sim <- function(rewards, par){
  
  beta <- par[1]
  positive_alpha <- par[2]
  negative_alpha <- par[3]
  tau <- par[3]
  phi <- par[5]
  q0 <- par[6]
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(q0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- sample(1:n_options)
  options <- rep(1:n_options, 1)
  
  # Choice trace for each arm at trial t
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  
  
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*(q_values[t,] + phi*choice_trace[t+1,]))
    
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choices[t] <- select_randomly[t]
    } else {
      choices[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    # 
    # if (t < n_options){
    #   # because of an error the first trial is set to randomly select
    #   choices[t] <- sample(select_randomly, 1)
    #   select_randomly <- select_randomly[!select_randomly %in% choices[t]]
    #   
    # }
    # if (t == n_options) {
    #   choices[t] <- select_randomly
    # }
    
    agents_rewards[t] <- rewards[t, choices[t]+1] # Agents reward at trial t
    
    
    # setting discount value, since the forgetting rate is 0, 
    # only updates the chosen arm will be updated
    
    discount <- rep(0,n_options)
    discount[choices[t]] <- 1
    
    if ((agents_rewards[t] - q_values[t, choices[t]]) < 0){
      alpha <- negative_alpha 
    } else {
      alpha <- positive_alpha
    }
    
    q_values[t+1,] <- q_values[t,] + discount*alpha*(agents_rewards[t] - 
                                                       q_values[t,])
    
    # Choice Trace update
    
    indicator <- rep(0,n_options)
    indicator[choices[t]] <- 1
    
    choice_trace[t+1, ] <- choice_trace[t,] - tau*choice_trace[t,] + tau*indicator
    
  }
  return(list(q_values = q_values, choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  # Returning variables of interest 
}


### Asymmetric Simple Q learning model (actual rewards) + autocorrlation

asym_reward_auto_sim <- function(rewards, par){
  beta <- par[1]
  positive_alpha <- par[2]
  negative_alpha <- par[3] 
  tau <- par[4]
  phi <- par[5]
  q0 <- par[6]
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(q0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- sample(1:n_options)
  options <- rep(1:n_options, 1)
  
  # Choice trace for each arm at trial t
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*(q_values[t,] + phi*choice_trace[t+1,]))
    
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choices[t] <- select_randomly[t]
    } else {
      choices[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    # if (t < n_options){
    #   # because of an error the first trial is set to randomly select
    #   choices[t] <- sample(select_randomly, 1)
    #   select_randomly <- select_randomly[!select_randomly %in% choices[t]]
    #   
    # }
    # if (t == n_options) {
    #   choices[t] <- select_randomly
    # }
    
    agents_rewards[t] <- rewards[t, choices[t]+1] # Agents reward at trial t
    
    
    # setting discount value, since the forgetting rate is 0, 
    # only updates the chosen arm will be updated
    
    discount <- rep(0,n_options)
    discount[choices[t]] <- 1
    
    if ((agents_rewards[t]) < 0){
      alpha <- negative_alpha 
    } else {
      alpha <- positive_alpha
    }
    
    q_values[t+1,] <- q_values[t,] + discount*alpha*(agents_rewards[t] - 
                                                       q_values[t,])
    
    # Choice Trace update
    
    indicator <- rep(0,n_options)
    indicator[choices[t]] <- 1
    
    choice_trace[t+1, ] <- choice_trace[t,] - tau*choice_trace[t,] + tau*indicator
    
  }
  return(list(q_values = q_values, choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  # Returning variables of interest 
}





# UCB Models for simulation

### Symmetric + kalman filter + softmax UCB 


ucb_sim <- function(rewards, par) {
  
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq <- par[4]
  sigma_xi_sq <- par[5]
  beta <- par[6]
  
  nt <- nrow(rewards)
  n_options <- ncol(rewards)-1
  m <- matrix(m0,ncol=n_options,nrow=nt+1)
  v <- matrix(v0,ncol=n_options,nrow=nt+1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  
  select_randomly <- sample(1:n_options)
  options <- rep(1:n_options, 1)
  
  for(t in 1:nt) {
    
    softmax_probs <- exp(gamma*(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq))))
    
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choice[t] <- select_randomly[t]
    } else {
      choice[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    # choose the option with the highest UCB, breaking ties at random
    # choice[t] <- which.is.max(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq)))
    # choice[t] <- which.is.max(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq)))
    reward[t] <- rewards[t, choice[t]+1]
    # Kalman updates:
    kt <- rep(0,n_options)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, agents_total_rewards = sum(reward)))
}


### aymmetric (RPE) + kalman filter + UCB 

ucb_asym_rpe_sim <- function(rewards, par){
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq_pos <- par[4]
  sigma_epsilon_sq_neg <- par[5]
  sigma_xi_sq <- par[6]
  beta <- par[7]

  nt <- nrow(rewards)
  n_options <- ncol(rewards)-1
  m <- matrix(m0,ncol=n_options,nrow=nt+1)
  v <- matrix(v0,ncol=n_options,nrow=nt+1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  
  select_randomly <- sample(1:n_options)
  options <- rep(1:n_options, 1)
  
  for(t in 1:nt) {
    
    softmax_probs <- exp(gamma*(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq))))
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choice[t] <- select_randomly[t]
    } else {
      choice[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    # choose the option with the highest UCB, breaking ties at random
    # choice[t] <- which.is.max(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq)))
    reward[t] <- rewards[t, choice[t]+1]
    if (reward[t] - m[t,choice[t]] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg
    } else{
      sigma_epsilon_sq <- sigma_epsilon_sq_pos
    }
    # Kalman updates:
    kt <- rep(0,n_options)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + 
                                                       sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, 
              agents_total_rewards = sum(reward)))
}

### aymmetric (reward) + kalman filter + UCB 

ucb_asym_reward_sim <- function(rewards, par) {
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq_pos <- par[4]
  sigma_epsilon_sq_neg <- par[5]
  sigma_xi_sq <- par[6]
  beta <- par[7]
  

  nt <- nrow(rewards)
  n_options <- ncol(rewards)-1
  m <- matrix(m0,ncol=n_options,nrow=nt+1)
  v <- matrix(v0,ncol=n_options,nrow=nt+1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  
  select_randomly <- sample(1:n_options)
  options <- rep(1:n_options, 1)
  
  for(t in 1:nt) {
    softmax_probs <- exp(gamma*(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq))))
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choice[t] <- select_randomly[t]
    } else {
      choice[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    # choose the option with the highest UCB, breaking ties at random
    # choice[t] <- which.is.max(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq)))
    reward[t] <- rewards[t, choice[t]+1]
    if (reward[t] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    # Kalman updates:
    kt <- rep(0,n_options)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + 
                                                       sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, 
              agents_total_rewards = sum(reward)))
}

### symmetric (reward) + kalman filter + UCB + autocorrlation

ucb_auto_sim <- function(rewards,par) {
  
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq <- par[4]
  sigma_xi_sq <- par[5]
  beta <- par[6]
  tau <- par[7]
  phi <- par[8]
  
  rewards <- rewards[,-1]
  nt <- nrow(rewards)
  n_options <- ncol(rewards)
  m <- matrix(m0,ncol=n_options,nrow=nt+1)
  v <- matrix(v0,ncol=n_options,nrow=nt+1)
  choice_trace <- matrix(0, ncol = n_options, nrow = nt + 1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  options <- rep(1:n_options, 1)
  
  select_randomly <- sample(1:n_options)
  
  for(t in 1:nt) {
    softmax_probs <- exp(gamma*(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq)) + phi*choice_trace[t,]))
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choice[t] <- select_randomly[t]
    } else {
      choice[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    
    # choose the option with the highest UCB, breaking ties at random
    # choice[t] <- which.is.max(m[t,] + beta*sqrt(v[t,] + sigma_xi_sq ) 
    #                          + phi*choice_trace[t,])
    reward[t] <- rewards[t,choice[t]]
    # Kalman updates:
    kt <- rep(0,n_options)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/
      (v[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
    indicator <- rep(0,n_options)
    indicator[choice[t]] <- 1
    choice_trace[t+1, ] <- (choice_trace[t,] - tau*choice_trace[t,] + tau*indicator)
  }
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, agents_total_rewards = sum(reward)))
}

### aymmetric (RPE) + kalman filter + UCB + autocorrlation

ucb_asym_rpe_auto_sim <- function(rewards, par){
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq_pos <- par[4]
  sigma_epsilon_sq_neg <- par[5]
  sigma_xi_sq <- par[6]
  beta <- par[7]
  tau <- par[8]
  phi <- par[9]
  
  nt <- nrow(rewards)
  n_options <- ncol(rewards)-1
  m <- matrix(m0,ncol=n_options,nrow=nt+1)
  v <- matrix(v0,ncol=n_options,nrow=nt+1)
  choice_trace <- matrix(0, ncol = n_options, nrow = nt + 1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  options <- rep(1:n_options, 1)
  
  select_randomly <- sample(1:n_options)
  
  for(t in 1:nt) {
    softmax_probs <- exp(gamma*(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq)) + phi*choice_trace[t,]))
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choice[t] <- select_randomly[t]
    } else {
      choice[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    
    # choose the option with the highest UCB, breaking ties at random
    #choice[t] <- which.is.max(m[t,] + beta*sqrt(v[t,] + sigma_xi_sq ) 
    #                          + phi*choice_trace[t,])
    reward[t] <- rewards[t, choice[t]+1]
    if (reward[t] - m[t,choice[t]] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    # Kalman updates:
    kt <- rep(0,n_options)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + 
                                                       sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
    
    # choice trace update
    indicator <- rep(0,n_options)
    indicator[choice[t]] <- 1
    choice_trace[t+1, ] <- (choice_trace[t,] - tau*choice_trace[t,] + tau*indicator)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, 
              agents_total_rewards = sum(reward)))
}

### aymmetric (reward) + kalman filter + UCB + autocorrlation
ucb_asym_reward_auto_sim <- function(rewards, par){
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq_pos <- par[4]
  sigma_epsilon_sq_neg <- par[5]
  sigma_xi_sq <- par[6]
  beta <- par[7]
  tau <- par[8]
  phi <- par[9]
  
  nt <- nrow(rewards)
  n_options <- ncol(rewards)-1
  m <- matrix(m0,ncol=n_options,nrow=nt+1)
  v <- matrix(v0,ncol=n_options,nrow=nt+1)
  choice_trace <- matrix(0, ncol = n_options, nrow = nt + 1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  options <- rep(1:n_options, 1)
  
  select_randomly <- sample(1:n_options)
  
  for(t in 1:nt) {
    softmax_probs <- exp(gamma*(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq)) + phi*choice_trace[t,]))
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    if(t <= n_options) {
      # initial trials...sample each option in random order
      choice[t] <- select_randomly[t]
    } else {
      choice[t] <- sample(options, size = 1, prob = softmax_probs)
    }
    
    
    # choose the option with the highest UCB, breaking ties at random
    #choice[t] <- which.is.max(m[t,] + beta*sqrt(v[t,] + sigma_xi_sq ) 
    #                          + phi*choice_trace[t,])
    reward[t] <- rewards[t, choice[t]+1]
    if (reward[t] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    # Kalman updates:
    kt <- rep(0,n_options)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + 
                                                       sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
    
    # choice trace update
    indicator <- rep(0,n_options)
    indicator[choice[t]] <- 1
    choice_trace[t+1, ] <- (choice_trace[t,] - tau*choice_trace[t,] + tau*indicator)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, 
              agents_total_rewards = sum(reward)))
}
