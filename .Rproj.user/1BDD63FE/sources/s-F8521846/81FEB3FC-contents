

# UCB Models for simulation

### Symmetric + kalman filter + UCB 



ucb_sim <- function(rewards,m0,v0,sigma_xi_sq,sigma_epsilon_sq,beta) {
  nt <- nrow(rewards)
  no <- ncol(rewards)-1
  m <- matrix(m0,ncol=no,nrow=nt+1)
  v <- matrix(v0,ncol=no,nrow=nt+1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  for(t in 1:nt) {
    # choose the option with the highest UCB, breaking ties at random
    choice[t] <- which.is.max(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq)))
    reward[t] <- rewards[t, choice[t]+1]
    # Kalman updates:
    kt <- rep(0,no)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, agents_total_rewards = sum(reward)))
}


### aymmetric (RPE) + kalman filter + UCB 

ucb_asym_rpe_sim <- function(rewards, m0, v0, sigma_xi_sq,
                             sigma_epsilon_sq_pos, sigma_epsilon_sq_neg ,beta){
  nt <- nrow(rewards)
  no <- ncol(rewards)-1
  m <- matrix(m0,ncol=no,nrow=nt+1)
  v <- matrix(v0,ncol=no,nrow=nt+1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  for(t in 1:nt) {
    # choose the option with the highest UCB, breaking ties at random
    choice[t] <- which.is.max(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq)))
    reward[t] <- rewards[t, choice[t]+1]
    if (reward[t] - m[t,choice[t]] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    # Kalman updates:
    kt <- rep(0,no)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + 
                                                       sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, 
              agents_total_rewards = sum(reward)))
}

### aymmetric (reward) + kalman filter + UCB 

ucb_asym_reward_sim <- function(rewards, m0, v0, sigma_xi_sq,
                                sigma_epsilon_sq_pos, sigma_epsilon_sq_neg ,beta){
  nt <- nrow(rewards)
  no <- ncol(rewards)-1
  m <- matrix(m0,ncol=no,nrow=nt+1)
  v <- matrix(v0,ncol=no,nrow=nt+1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  for(t in 1:nt) {
    # choose the option with the highest UCB, breaking ties at random
    choice[t] <- which.is.max(m[t,] + beta*(sqrt(v[t,] + sigma_xi_sq)))
    reward[t] <- rewards[t, choice[t]+1]
    if (reward[t] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    # Kalman updates:
    kt <- rep(0,no)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + 
                                                       sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, 
              agents_total_rewards = sum(reward)))
}

### symmetric (reward) + kalman filter + UCB + autocorrlation

ucb_auto_sim <- function(rewards,m0,v0,sigma_xi_sq,sigma_epsilon_sq,
                         beta,tau,phi) {
  rewards <- rewards[,-1]
  nt <- nrow(rewards)
  no <- ncol(rewards)
  m <- matrix(m0,ncol=no,nrow=nt+1)
  v <- matrix(v0,ncol=no,nrow=nt+1)
  choice_trace <- matrix(0, ncol = no, nrow = nt + 1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  for(t in 1:nt) {
    # choose the option with the highest UCB, breaking ties at random
    choice[t] <- which.is.max(m[t,] + beta*sqrt(v[t,] + sigma_xi_sq ) 
                              + phi*choice_trace[t,])
    reward[t] <- rewards[t,choice[t]]
    # Kalman updates:
    kt <- rep(0,no)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/
      (v[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
    indicator <- rep(0,no)
    indicator[choice[t]] <- 1
    choice_trace[t+1, ] <- (choice_trace[t,] - tau*choice_trace[t,] + tau*indicator)
  }
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, agents_total_rewards = sum(reward)))
}

### aymmetric (RPE) + kalman filter + UCB + autocorrlation

ucb_asym_rpe_auto_sim <- function(rewards, m0, v0, sigma_xi_sq,
                                  sigma_epsilon_sq_pos, sigma_epsilon_sq_neg,
                                  beta, tau, phi){
  nt <- nrow(rewards)
  no <- ncol(rewards)-1
  m <- matrix(m0,ncol=no,nrow=nt+1)
  v <- matrix(v0,ncol=no,nrow=nt+1)
  choice_trace <- matrix(0, ncol = no, nrow = nt + 1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  for(t in 1:nt) {
    # choose the option with the highest UCB, breaking ties at random
    choice[t] <- which.is.max(m[t,] + beta*sqrt(v[t,] + sigma_xi_sq ) 
                              + phi*choice_trace[t,])
    reward[t] <- rewards[t, choice[t]+1]
    if (reward[t] - m[t,choice[t]] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    # Kalman updates:
    kt <- rep(0,no)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + 
                                                       sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
    
    # choice trace update
    indicator <- rep(0,no)
    indicator[choice[t]] <- 1
    choice_trace[t+1, ] <- (choice_trace[t,] - tau*choice_trace[t,] + tau*indicator)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, 
              agents_total_rewards = sum(reward)))
}

### aymmetric (reward) + kalman filter + UCB + autocorrlation


ucb_asym_reward_auto_sim <- function(rewards, m0, v0, sigma_xi_sq,
                                     sigma_epsilon_sq_pos, sigma_epsilon_sq_neg,
                                     beta, tau, phi){
  nt <- nrow(rewards)
  no <- ncol(rewards)-1
  m <- matrix(m0,ncol=no,nrow=nt+1)
  v <- matrix(v0,ncol=no,nrow=nt+1)
  choice_trace <- matrix(0, ncol = no, nrow = nt + 1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  for(t in 1:nt) {
    # choose the option with the highest UCB, breaking ties at random
    choice[t] <- which.is.max(m[t,] + beta*sqrt(v[t,] + sigma_xi_sq ) 
                              + phi*choice_trace[t,])
    reward[t] <- rewards[t, choice[t]+1]
    if (reward[t] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    # Kalman updates:
    kt <- rep(0,no)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + 
                                                       sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
    
    # choice trace update
    indicator <- rep(0,no)
    indicator[choice[t]] <- 1
    choice_trace[t+1, ] <- (choice_trace[t,] - tau*choice_trace[t,] + tau*indicator)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, 
              agents_total_rewards = sum(reward)))
}

