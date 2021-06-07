



rl_simpleQ_sim <- function(rewards, alpha){
  n_trials <- nrow(rewards)
  n_options <- ncol(rewards)
  
  rewards_recieved_options <- matrix("NA",ncol=n_options,nrow=n_trials+1) 
  
  rpes <- matrix(m0,ncol=no,nrow=nt+1) 
  choice <- rep(0, n_trials)
  reward <- rep(0.0, n_trials)
  
  reward_estimation <- matrix(0,ncol=n_options,nrow=n_trials+1) 
  
  
  softmax_choice <- 

  for(t in 1:nt){
    if (t<5) {
      
      
      
    }
    for s in 1:4){
      reward_estimation[t,s] <- reward_
      
      
      rpe <- previous_reward <- old_value_estimation 
      new_value_estimation <- old_value_estimation + (alpha*rpe)
    
      
    
  }

    
    
    
  }
  

  rl_softmax_sim <- function(rewards,m0,v0,sigma_xi_sq,sigma_epsilon_sq,gamma) {
    nt <- nrow(rewards) # number of time points
    no <- ncol(rewards) # number of options
    m <- matrix(m0,ncol=no,nrow=nt+1) # to hold the posterior means
    v <- matrix(v0,ncol=no,nrow=nt+1) # to hold the posterior variances
    choice <- rep(0,nt) # to hold the choices of the RL agent
    reward <- rep(0.0,nt) # to hold the obtained rewards by the RL agent
    # loop over all timepoints
    for(t in 1:nt) {
      # use the prior means and compute the probability of choosing each option
      p <- exp(gamma*m[t,])
      p <- p/sum(p)
      # choose an option according to these probabilities
      choice[t] <- sample(1:4,size=1,prob=p)
      # get the reward of the choice
      reward[t] <- rewards[t,choice[t]]
      # set the Kalman gain for unchosen options
      kt <- rep(0,4)
      # set the Kalman gain for the chosen option
      kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
      # compute the posterior means
      m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
      # compute the posterior variances
      v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
    }
    # return everything of interest
    return(list(m=m,v=v,choice=choice,reward=reward))
  }
  