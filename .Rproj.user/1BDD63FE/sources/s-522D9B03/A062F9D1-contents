
### Parameter estimation symmetric kalman + UCB

ucb_rl <- function(choice, reward, n_options, m0, v0,
                   sigma_xi_sq, sigma_epsilon_sq) {
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(m0, ncol = n_options, nrow = n_trials) 
  arm_variance <- matrix(v0, ncol = n_options, 
                         nrow = n_trials)
  for (t in 1:(length(choice)-1)) {
    # Updating kalman gain values 
    kalman_t <- rep(0,n_options)
    kalman_t[choice[t]] <- (arm_variance[t,choice[t]] + sigma_xi_sq) / 
      (arm_variance[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    arm_mean[t+1, ] <- arm_mean[t, ] + kalman_t*(reward[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-kalman_t)*(arm_variance[t, ]+sigma_xi_sq)
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance))
}


ucb_softmax_prob <- function(arm_mean, ppsd, gamma, beta) {
  prob <- exp(gamma*(arm_mean + ppsd*beta))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}

ucb_negLogLik <- function(par, data){
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq <- par[4]
  beta <- par[5] 
  sigma_xi_sq <- innovation_term
  choice <- data$deck
  reward <- data$payoff
  kal <- ucb_rl(choice, reward, arm, m0, v0,
                sigma_xi_sq, sigma_epsilon_sq)
  arm_mean <- kal$arm_mean
  ppsd <- sqrt(kal$arm_variance + sigma_xi_sq)
  prob <- ucb_softmax_prob(arm_mean, ppsd, gamma, beta)
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


ucb_negLogLik_DE <- function(par,data,x) {
  negLL <- ucb_negLogLik(par,data)
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}


### Parameter estimation aymmetric (RPE) kalman + UCB

ucb_asym_rpe_rl <- function(choice, reward, n_options, m0, v0,
                   sigma_xi_sq, sigma_epsilon_sq_pos, sigma_epsilon_sq_neg) {
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(m0, ncol = n_options, nrow = n_trials) 
  arm_variance <- matrix(v0, ncol = n_options, 
                         nrow = n_trials)
  for (t in 1:(length(choice)-1)) {
    # Updating kalman gain values
    if (reward[t] - arm_mean[t,choice[t]] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    kalman_t <- rep(0,n_options)
    kalman_t[choice[t]] <- (arm_variance[t,choice[t]] + sigma_xi_sq) / 
      (arm_variance[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    arm_mean[t+1, ] <- arm_mean[t, ] + kalman_t*(reward[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-kalman_t)*(arm_variance[t, ]+sigma_xi_sq)
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance))
}


ucb_asym_rpe_negLogLik <- function(par, data){
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq_pos <- par[4]
  sigma_epsilon_sq_neg <- par[5]
  beta <- par[6] 
  sigma_xi_sq <- innovation_term
  choice <- data$deck
  reward <- data$payoff
  kal <- ucb_asym_rpe_rl(choice, reward, arm, m0, v0,
                sigma_xi_sq, sigma_epsilon_sq_pos, sigma_epsilon_sq_neg)
  arm_mean <- kal$arm_mean
  ppsd <- sqrt(kal$arm_variance + sigma_xi_sq)
  prob <- ucb_softmax_prob(arm_mean, ppsd, gamma, beta)
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


ucb_asym_rpe_negLogLik_DE <- function(par,data,x) {
  negLL <- ucb_asym_rpe_negLogLik(par,data)
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}



### Parameter estimation aymmetric (reward) kalman + UCB

ucb_asym_reward_rl <- function(choice, reward, n_options, m0, v0,
                            sigma_xi_sq, sigma_epsilon_sq_pos, sigma_epsilon_sq_neg) {
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(m0, ncol = n_options, nrow = n_trials) 
  arm_variance <- matrix(v0, ncol = n_options, 
                         nrow = n_trials)
  for (t in 1:(length(choice)-1)) {
    # Updating kalman gain values
    if (reward[t] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    kalman_t <- rep(0,n_options)
    kalman_t[choice[t]] <- (arm_variance[t,choice[t]] + sigma_xi_sq) / 
      (arm_variance[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    arm_mean[t+1, ] <- arm_mean[t, ] + kalman_t*(reward[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-kalman_t)*(arm_variance[t, ]+sigma_xi_sq)
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance))
}


ucb_asym_reward_negLogLik <- function(par, data){
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq_pos <- par[4]
  sigma_epsilon_sq_neg <- par[5]
  beta <- par[6] 
  sigma_xi_sq <- innovation_term
  choice <- data$deck
  reward <- data$payoff
  kal <- ucb_asym_reward_rl(choice, reward, arm, m0, v0,
                         sigma_xi_sq, sigma_epsilon_sq_pos, sigma_epsilon_sq_neg)
  arm_mean <- kal$arm_mean
  ppsd <- sqrt(kal$arm_variance + sigma_xi_sq)
  prob <- ucb_softmax_prob(arm_mean, ppsd, gamma, beta)
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


ucb_asym_reward_negLogLik_DE <- function(par,data) {
  negLL <- ucb_asym_reward_negLogLik(par,data)
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}



### Parameter estimation symmetric kalman + UCB + auto


ucb_auto <- function(choice, reward, n_options, m0, v0,
                   sigma_xi_sq, sigma_epsilon_sq, tau) {
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(m0, ncol = n_options, nrow = n_trials + 1) 
  arm_variance <- matrix(v0, ncol = n_options, 
                         nrow = n_trials + 1)
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials)
  for (t in 1:(length(choice)-1)) {
    
    # Updating kalman gain values 
    kalman_t <- rep(0,n_options)
    kalman_t[choice[t]] <- (arm_variance[t,choice[t]] + sigma_xi_sq) / 
      (arm_variance[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    arm_mean[t+1, ] <- arm_mean[t, ] + kalman_t*(reward[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-kalman_t)*(arm_variance[t, ]+sigma_xi_sq)
    
    
    # Updating the choice trace
    indicator <- rep(0,n_options)
    indicator[choice[t]] <- 1
    choice_trace[t+1, ] <- choice_trace[t,]*(1 - tau) + tau*indicator
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance,
              choice_trace = choice_trace))
}


ucb_auto_softmax_prob <- function(arm_mean, ppsd, gamma, beta, ct, phi) {
  prob <- exp(gamma*(arm_mean + ppsd*beta + ct*phi))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



ucb_auto_negLogLik <- function(par, data){
  
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq <- par[4]
  sigma_xi_sq <- innovation_term
  beta <- par[5] 
  tau <- par[6]
  phi <- par[7]
  choice <- data$deck
  reward <- data$payoff
  
  kal <- ucb_auto(choice, reward, arm, m0, v0,
                sigma_xi_sq, sigma_epsilon_sq, tau)
  
  arm_mean <- kal$arm_mean[1:(nrow(data)),]
  
  ppsd <- sqrt(kal$arm_variance[1:(nrow(data)),] + sigma_xi_sq)
  
  ct <- kal$choice_trace[1:(nrow(data)),]
  
  prob <- ucb_auto_softmax_prob(arm_mean, ppsd, gamma, beta, ct, phi)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}



ucb_auto_negLogLik_DE <- function(par,data) {
  negLL <- ucb_auto_negLogLik(par,data)
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}



### Parameter estimation asymmetric (RPE) kalman + UCB + auto


ucb_asym_rpe_auto <- function(choice, reward, n_options, m0, v0,
                     sigma_xi_sq, sigma_epsilon_sq_pos, sigma_epsilon_sq_neg, tau) {
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(m0, ncol = n_options, nrow = n_trials + 1) 
  arm_variance <- matrix(v0, ncol = n_options, 
                         nrow = n_trials + 1)
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials)
  for (t in 1:(length(choice)-1)) {
    
    # Updating kalman gain values 
    if (reward[t] - arm_mean[t,choice[t]] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    kalman_t <- rep(0,n_options)
    kalman_t[choice[t]] <- (arm_variance[t,choice[t]] + sigma_xi_sq) / 
      (arm_variance[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    arm_mean[t+1, ] <- arm_mean[t, ] + kalman_t*(reward[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-kalman_t)*(arm_variance[t, ]+sigma_xi_sq)
    
    
    # Updating the choice trace
    indicator <- rep(0,n_options)
    indicator[choice[t]] <- 1
    choice_trace[t+1, ] <- choice_trace[t,]*(1 - tau) + tau*indicator
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance,
              choice_trace = choice_trace))
}

ucb_asym_rpe_auto_negLogLik <- function(par, data){
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq_pos <- par[4]
  sigma_epsilon_sq_neg <- par[5]
  sigma_xi_sq <- innovation_term
  beta <- par[6] 
  tau <- par[7]
  phi <- par[8]
  choice <- data$deck
  reward <- data$payoff
  kal <- ucb_asym_rpe_auto(choice, reward, arm, m0, v0,
                  sigma_xi_sq, sigma_epsilon_sq_pos, sigma_epsilon_sq_neg, tau)
  arm_mean <- kal$arm_mean[1:(nrow(data)),]
  ppsd <- sqrt(kal$arm_variance[1:(nrow(data)),] + sigma_xi_sq)
  ct <- kal$choice_trace[1:(nrow(data)),]
  prob <- ucb_auto_softmax_prob(arm_mean, ppsd, gamma, beta, ct, phi)
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}

ucb_asym_rpe_auto_negLogLik_DE <- function(par,data,x) {
  negLL <- ucb_asym_rpe_auto_negLogLik(par,data)
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}



### Parameter estimation asymmetric (reward) kalman + UCB + auto


ucb_asym_reward_auto <- function(choice, reward, n_options, m0, v0,
                              sigma_xi_sq, sigma_epsilon_sq_pos, sigma_epsilon_sq_neg, tau) {
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(m0, ncol = n_options, nrow = n_trials + 1) 
  arm_variance <- matrix(v0, ncol = n_options, 
                         nrow = n_trials + 1)
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials)
  for (t in 1:(length(choice)-1)) {
    
    # Updating kalman gain values 
    if (reward[t] < 0){
      sigma_epsilon_sq <- sigma_epsilon_sq_neg} else{
        sigma_epsilon_sq <- sigma_epsilon_sq_pos
      }
    kalman_t <- rep(0,n_options)
    kalman_t[choice[t]] <- (arm_variance[t,choice[t]] + sigma_xi_sq) / 
      (arm_variance[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    arm_mean[t+1, ] <- arm_mean[t, ] + kalman_t*(reward[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-kalman_t)*(arm_variance[t, ]+sigma_xi_sq)
    
    
    # Updating the choice trace
    indicator <- rep(0,n_options)
    indicator[choice[t]] <- 1
    choice_trace[t+1, ] <- choice_trace[t,]*(1 - tau) + tau*indicator
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance,
              choice_trace = choice_trace))
}

ucb_asym_reward_auto_negLogLik <- function(par, data){
  gamma <- par[1]
  m0 <- par[2]
  v0 <- par[3]
  sigma_epsilon_sq_pos <- par[4]
  sigma_epsilon_sq_neg <- par[5]
  sigma_xi_sq <- innovation_term
  beta <- par[6] 
  tau <- par[7]
  phi <- par[8]
  choice <- data$deck
  reward <- data$payoff
  kal <- ucb_asym_reward_auto(choice, reward, arm, m0, v0,
                           sigma_xi_sq, sigma_epsilon_sq_pos, sigma_epsilon_sq_neg, tau)
  arm_mean <- kal$arm_mean[1:(nrow(data)),]
  ppsd <- sqrt(kal$arm_variance[1:(nrow(data)),] + sigma_xi_sq)
  ct <- kal$choice_trace[1:(nrow(data)),]
  prob <- ucb_auto_softmax_prob(arm_mean, ppsd, gamma, beta, ct, phi)
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}



ucb_asym_reward_auto_negLogLik_DE <- function(par,data,x) {
  negLL <- ucb_asym_reward_auto_negLogLik(par,data)
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}





