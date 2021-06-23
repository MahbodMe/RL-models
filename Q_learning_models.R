




### Symmetric Simple Q learning model 


rl_simpleQ_symmetric_sim <- function(rewards, alpha, beta) {
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials

  # Estimated value for each arm at trial t
  q_values <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- rep(1:n_options, 1)
  options <- rep(1:n_options, 1)
  
  
  
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*q_values[t,]) 
    
    softmax_probs <- softmax_probs / sum(softmax_probs)

    choices[t] <- sample(options, size = 1, prob = softmax_probs)
    
    if (t < n_options){
      # because of an error the first trial is set to randomly select
      choices[t] <- sample(select_randomly, 1)
      select_randomly <- select_randomly[!select_randomly %in% choices[t]]
      
    } 
    if (t == n_options) {
      choices[t] <- select_randomly
    }
    
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


sim_1 <- rl_simpleQ_symmetric_sim(rewards, .5, 3)







### Asymmetric Simple Q learning model (RPE)

rl_simpleQ_asymmetric_sim <- function(rewards, positive_alpha, negative_alpha, beta){
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- rep(1:n_options, 1)
  options <- rep(1:n_options, 1)
  
  
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*q_values[t,]) 
    
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    choices[t] <- sample(options, size = 1, prob = softmax_probs)
    
    if (t < n_options){
      # because of an error the first trial is set to randomly select
      choices[t] <- sample(select_randomly, 1)
      select_randomly <- select_randomly[!select_randomly %in% choices[t]]
      
    }
    if (t == n_options) {
      choices[t] <- select_randomly
    }
    
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



sim_2 <- rl_simpleQ_asymmetric_sim(rewards, .6, .2, 3)

  
  
### Asymmetric Simple Q learning model (actual rewards)

rl_simpleQ_asymmetric_sim2 <- function(rewards, positive_alpha, negative_alpha, beta){
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- rep(1:n_options, 1)
  
  options <- rep(1:n_options, 1)
  
  
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*q_values[t,]) 
    
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    choices[t] <- sample(options, size = 1, prob = softmax_probs)
    
    if (t < n_options){
      # because of an error the first trial is set to randomly select
      choices[t] <- sample(select_randomly, 1)
      select_randomly <- select_randomly[!select_randomly %in% choices[t]]
      
    }
    if (t == n_options) {
      choices[t] <- select_randomly
    }
    
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



sim_3 <- rl_simpleQ_asymmetric_sim2(rewards, .6, .2, 3)
  