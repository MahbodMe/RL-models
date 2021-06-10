

rl_simpleQ_sim <- function(rewards, m0, v0,) {
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
}