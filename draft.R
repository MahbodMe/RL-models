


### Required packges 

library(dplyr)
library(tidyr)
library(ggplot2)
library(nnet)
library(DEoptim)
library(abind)



### the following function generates rewards 

reward_generator <- function(seed, nt, no, error_term, innovation_term, mu0){
  
  set.seed(seed) # set the random seed to replicate these results exactly
  mu <- matrix(0.0,nrow=nt,ncol=no) # to store the time-varying means
  mu[1,] <- mu0 # initialize the means for t=1
  for(t in 2:nt) {
    mu[t,] <- mu[t-1,] + rnorm(no,mean=0,sd=innovation_term) # compute mean at t based on mean at t-1
  }
  rewards <- mu + rnorm(length(mu),mean=0,sd=error_term) # generate the stochastic rewards
  mu <- as.data.frame(cbind(1:nt,mu))
  rewards <- as.data.frame(cbind(1:nt,rewards))
  colnames(rewards) <- colnames(mu) <- c("trial",paste0("A",1:no))
  
  return(rewards)
}

# Generating fake data

sim_behav_data_csv <- function(data, funs, pars, no, nt, envir = parent.frame()){
  mat <- matrix(0, nrow = length(funs)*nt, ncol = 3)
  for (i in 1:length(funs)){
    fun <- funs[i]
    par <- as.numeric(unlist(pars[i]))
    sim <- do.call(fun, c(list(data), par[1:length(par)]))
    x <- (i-1)*nt + 1
    y <- i*nt
    mat[x:y ,1] <- rep(i, nt)
    mat[x:y ,2] <- head(sim$choices, nt)
    mat[x:y ,3] <- head(sim$agents_rewards, nt)
  }
  mats <- as.data.frame(cbind(1:nt, mat))
  colnames(mats) <- c("trial", "id", "deck", "payoff")
  return(data_sim = mats)
}

### takes a list of function and parameters and data, produces para_est

find_the_best_pars <- function(lpar, upar, fun, df, nm, nt){
  value <- rep("NA", nm)
  parameter <- rep(0, nm)
  for (i in 1:nm){
    x <- (i-1)*nt + 1
    y <- i*nt
    est_DE <- DEoptim(fun,lower=as.double(lpar),
                      upper=as.double(upar),data=df[x:y,],
                      control=DEoptim.control(trace=FALSE))
    value[i] <- list(est_DE$optim$bestval)
    parameter[i] <- list(est_DE$optim$bestmem)
  }
  return(list(parameter = parameter, value = value))
}



# Q_learning Models for simulation

### Symmetric Simple Q learning model 


sym_sim <- function(rewards, beta, alpha) {
  
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




### Asymmetric Simple Q learning model (RPE)

asym_rpe_sim <- function(rewards, beta, positive_alpha, negative_alpha){
  
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






### Asymmetric Simple Q learning model (actual rewards)

asym_reward_sim <- function(rewards, beta, positive_alpha, negative_alpha){
  
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






### Symmetric Simple Q learning model + autocorrlation


sym_auto_sim <- function(rewards, beta, alpha, tau, phi) {
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- rep(1:n_options, 1)
  options <- rep(1:n_options, 1)
  # Choice trace for each arm at trial t
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  
  
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*(q_values[t,] + phi*choice_trace[t+1,]))
    
    softmax_probs <- softmax_probs / sum(softmax_probs)
    
    choices[t] <- sample(options, size = 1, prob = softmax_probs)
    
    
    
    if (t < n_options){
      # because of an error the first trial i
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
    # Choice Trace update
    
    indicator <- rep(0,n_options)
    indicator[choices[t]] <- 1
    
    choice_trace[t+1, ] <- choice_trace[t,] - tau*choice_trace[t,] + tau*indicator
    
  }
  return(list(q_values = q_values, choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  
}




### Asymmetric Simple Q learning model (RPE) + autocorrlation

asym_rpe_auto_sim <- function(rewards, beta, positive_alpha, 
                              negative_alpha, tau, phi){
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- rep(1:n_options, 1)
  options <- rep(1:n_options, 1)
  
  # Choice trace for each arm at trial t
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  
  
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*(q_values[t,] + phi*choice_trace[t+1,]))
    
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

asym_reward_auto_sim <- function(rewards, positive_alpha, negative_alpha, 
                                 beta, tau, phi){
  
  n_trials <- nrow(rewards) # Number of Trials 
  n_options <- ncol(rewards)-1 # Number of Options
  choices <- rep(0, n_trials + 1) # All the choices over the trials 
  agents_rewards <- rep(0, n_trials + 1) # All the rewards over the trials
  
  # Estimated value for each arm at trial t
  q_values <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  
  softmax_probs <- rep(0, n_options) 
  
  select_randomly <- rep(1:n_options, 1)
  options <- rep(1:n_options, 1)
  
  # Choice trace for each arm at trial t
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials + 1)
  for (t in 1:n_trials) {
    
    # updating softmax probs. for each arm 
    softmax_probs <- exp(beta*(q_values[t,] + phi*choice_trace[t+1,]))
    
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



# Parameter estimation for all Q-learning models (six)

### Parameter estimation simple symmetric Q-learning 


sym_rl <- function(choice, reward, n_options, q_value_0, alpha){
  
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  
  for (t in 1:(length(choice)-1)) {
    discount <- rep(0, n_options)
    discount[choice[t]] <- 1
    q_value[t+1, ] <- q_value[t,] + discount*alpha*(reward[t] - q_value[t,])
  }
  return(list(q_value=q_value))
}


q_softmax_prob <- function(q_values, beta) {
  prob <- exp(q_values*beta)
  prob <- prob/rowSums(prob)
  return (prob = prob)
}

sym_negLogLik <- function(par, data){
  beta <- par[1]
  alpha <- par[2]
  choice <- data$deck
  reward <- data$payoff
  qs <- sym_rl(choice, reward, arm, 0, alpha)$q_value
  prob <- q_softmax_prob(qs, beta)
  lik <- prob[cbind(1:nrow(data),choice)]
  negLogLik <- sum(log(lik))*-1
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


sym_negLogLik_DE <- function(par,data) {
  negLL <- sym_negLogLik(par,data)
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}

### Parameter estimation simple asymmetric Q-learning (RPE)


asym_rpe_rl <- function(choice, reward, n_options, q_value_0, positive_alpha, 
                        negative_alpha){
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  for (t in 1:(length(choice)-1)) {
    if ((reward[t] - q_value[t, choice[t]]) < 0){
      alpha <- negative_alpha 
    } else {
      alpha <- positive_alpha
    }
    discount <- rep(0, n_options)
    discount[choice[t]] <- 1
    q_value[t+1, ] <- q_value[t,] + discount*alpha*(reward[t] - q_value[t,])
  }
  return(list(q_value=q_value))
}



asym_rpe_negLogLik <- function(par, data){
  beta <- par[1]
  p_alpha <- par[2]
  n_alpha <- par[3]
  choice <- data$deck
  reward <- data$payoff
  qs <- asym_rpe_rl(choice, reward, arm, 0, p_alpha, n_alpha)$q_value
  prob <- q_softmax_prob(qs, beta)
  lik <- prob[cbind(1:nrow(data),choice)]
  negLogLik <- sum(log(lik))*-1
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}



asym_rpe_negLogLik_DE <- function(par,data) {
  negLL <- asym_rpe_negLogLik(par,data)
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}


### Parameter estimation simple asymmetric Q-learning (reward magnitude)

asym_reward_rl <- function(choice, reward, n_options, q_value_0, positive_alpha, 
                           negative_alpha){
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  for (t in 1:(length(choice)-1)) {
    if (reward[t] < 0){
      alpha <- negative_alpha 
    } else {
      alpha <- positive_alpha
    }
    discount <- rep(0, n_options)
    discount[choice[t]] <- 1
    q_value[t+1, ] <- q_value[t,] + discount*alpha*(reward[t] - q_value[t,])
  }
  return(list(q_value=q_value))
}


asym_reward_negLogLik <- function(par, data){
  beta <- par[1]
  p_alpha <- par[2]
  n_alpha <- par[3]
  choice <- data$deck
  reward <- data$payoff
  qs <- asym_reward_rl(choice, reward, arm, 0, p_alpha, n_alpha)$q_value
  prob <- q_softmax_prob(qs, beta)
  lik <- prob[cbind(1:nrow(data),choice)]
  negLogLik <- sum(log(lik))*-1
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


asym_reward_negLogLik_DE <- function(par,data,x) {
  negLL <- asym_reward_negLogLik(par,data)
  
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}





### Parameter estimation simple symmetric Q-learning 


sym_auto_rl <- function(choice, reward, n_options, q_value_0, alpha, tau){
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  choice_trace <- matrix(0, ncol = n_options, nrow = length(choice))
  for (t in 1:(length(choice)-1)) {
    discount <- rep(0, n_options)
    discount[choice[t]] <- 1
    q_value[t+1, ] <- q_value[t,] + discount*alpha*(reward[t] - q_value[t,])
    choice_trace[t+1, ] <- choice_trace[t,]*(1 - tau) + tau*discount
  }
  return(list(q_value=q_value, choice_trace=choice_trace))
}

q_auto_softmax_prob <- function(q_values, beta, ct, phi) {
  prob <- exp(beta*(q_values + ct*phi))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}

sym_auto_negLogLik <- function(par, data){
  beta <- par[1]
  alpha <- par[2]
  tau <- par[3]
  phi <- par[4]
  choice <- data$deck
  reward <- data$payoff
  ct <- sym_auto_rl(choice, reward, arm, 0, alpha, tau)$choice_trace
  qs <- sym_auto_rl(choice, reward, arm, 0, alpha, tau)$q_value
  prob <- q_auto_softmax_prob(qs, beta, ct, phi)
  lik <- prob[cbind(1:nrow(data),choice)]
  negLogLik <- sum(log(lik))*-1
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}

sym_auto_negLogLik_DE <- function(par,data,x) {
  negLL <- sym_auto_negLogLik(par,data)
  
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}



### Parameter estimation simple asymmetric Q-learning (RPE)

asym_rpe_auto_rl<- function(choice, reward, n_options, q_value_0, 
                            positive_alpha, negative_alpha, tau){
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  choice_trace <- matrix(0, ncol = n_options, nrow = length(choice))
  for (t in 1:(length(choice)-1)) {
    if ((reward[t] - q_value[t, choice[t]]) < 0){
      alpha <- negative_alpha 
    } else {
      alpha <- positive_alpha
    }
    discount <- rep(0, n_options)
    discount[choice[t]] <- 1
    q_value[t+1, ] <- q_value[t,] + discount*alpha*(reward[t] - q_value[t,])
    choice_trace[t+1, ] <- choice_trace[t,]*(1 - tau) + tau*discount
  } 
  
  return(list(q_value=q_value, choice_trace=choice_trace))
}

asym_rpe_auto_negLogLik <- function(par, data){
  beta <- par[1]
  p_alpha <- par[2]
  n_alpha <- par[3]
  tau <- par[4]
  phi <- par[5]
  choice <- data$deck
  reward <- data$payoff
  ct <- asym_rpe_auto_rl(choice, reward, arm, 0, p_alpha, n_alpha, tau)$choice_trace
  qs <- asym_rpe_auto_rl(choice, reward, arm, 0, p_alpha, n_alpha, tau)$q_value
  prob <- q_auto_softmax_prob(qs, beta, ct, phi)
  lik <- prob[cbind(1:nrow(data),choice)]
  negLogLik <- sum(log(lik))*-1
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


asym_rpe_auto_negLogLik_DE <- function(par,data,x) {
  negLL <- asym_rpe_auto_negLogLik(par,data)
  
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}



### Parameter estimation simple auto + asymmetric Q-learning (reward magnitude)


asym_reward_auto_rl <- function(choice, reward, n_options, q_value_0, 
                                positive_alpha, negative_alpha, tau){
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  choice_trace <- matrix(0, ncol = n_options, nrow =length(choice))
  for (t in 1:(length(choice)-1)) {
    if (reward[t] < 0){
      alpha <- negative_alpha
    } else {
      alpha <- positive_alpha
    }
    discount <- rep(0, n_options)
    discount[choice[t]] <- 1
    q_value[t+1, ] <- q_value[t,] + discount*alpha*(reward[t] - q_value[t,])
    choice_trace[t+1, ] <- choice_trace[t,]*(1 - tau) + tau*discount
  } 
  return(list(q_value=q_value, choice_trace=choice_trace))
}

asym_reward_auto_negLogLik <- function(par, data){
  beta <- par[1]
  p_alpha <- par[2]
  n_alpha <- par[3]
  phi <- par[4]
  tau <- par[5]
  
  choice <- data$deck
  reward <- data$payoff
  ct <- asym_reward_auto_rl(choice, reward,  arm, 0, p_alpha, n_alpha, tau)$choice_trace*phi
  qs <- asym_reward_auto_rl(choice, reward,  arm, 0, p_alpha, n_alpha, tau)$q_value
  prob <- q_auto_softmax_prob(qs, beta, ct, phi)
  lik <- prob[cbind(1:nrow(data),choice)]
  negLogLik <- sum(log(lik))*-1
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}



asym_reward_auto_negLogLik_DE <- function(par,data,x) {
  negLL <- asym_reward_auto_negLogLik(par,data)
  
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}






# Parameter estimation for all UCB models (six)


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


# initializing the pars. 


index_par_df <- read.csv("pars.csv")
par_df <- readRDS("para.RDS")
index_par_df <- subset(index_par_df, index_par_df$ID!=1)


par_1 <- subset(index_par_df, index_par_df$Model==1)
par_2 <- subset(index_par_df, index_par_df$Model==2)
par_3 <- subset(index_par_df, index_par_df$Model==3)
par_4 <- subset(index_par_df, index_par_df$Model==4)
par_5 <- subset(index_par_df, index_par_df$Model==5)
par_6 <- subset(index_par_df, index_par_df$Model==6)
par_7 <- subset(index_par_df, index_par_df$Model==7)
par_8 <- subset(index_par_df, index_par_df$Model==8)
par_9 <- subset(index_par_df, index_par_df$Model==9)
par_10 <- subset(index_par_df, index_par_df$Model==10)
par_11 <- subset(index_par_df, index_par_df$Model==11)
par_12 <- subset(index_par_df, index_par_df$Model==12)


# initializing the env.


sds_inv <- seq(2,8, by=1)
sds_err <- seq(1,8, by=1)
trials <- seq(30,80, by=10)
trials <- append(trials, c(150, 200))
arms <- seq(2,8, by=2)
seeds <- sample(1:1000, 3)
m_0 <- c(-4,4,-12,12,-20,20,-60,60)




r <- length(sds_inv)*length(sds_err)*length(trials)*length(arms)*length(seeds)

rs <- matrix(NA, r, 13)
colnames(rs) <- c("seed", "trials", "nArms", "error", "innovation", "A1", "A2"
                  , "A3", "A4", "A5", "A6", "A7", "A8")


m_0 <- c(-4,4,-12,12,-20,20,-60,60)

r1 <- 1
for (s in 1:length(seeds)){
  for (t in 1:length(trials)){
    for (si in 1:length(sds_inv)){
      for (se in 1:length(sds_err)){
        for (a in 1:length(arms)){
          rs[r1, 1:(5+arms[a])] <- c(seeds[s], trials[t],  arms[a], sds_inv[si], sds_err[se], m_0[1:arms[a]])
          r1 <- r1+1
        }
      }
    }
  }
}


saveRDS(rs, "env.RDS")


seeds2 <- sample(1:1000, 5)
mat_value <- array(data = NA, c(12, 12, r, length(seeds2)))
colnames(mat_value) <- c("model_1", "model_2", "model_3", "model_4", "model_5",
                         "model_6", "model_7", "model_8", "model_9", "model_10",
                         "model_11", "model_12")
rownames(mat_value) <- c("agent_1", "agent_2", "agent_3", "agent_4", "agent_5",
                         "agent_6", "agent_7", "agent_8", "agent_9", "agent_10",
                         "agent_11", "agent_12") 
counter <- 1
nm <- 12

agent_sims <- c("sym_sim", "asym_rpe_sim", "asym_reward_sim", "sym_auto_sim", 
                "asym_rpe_auto_sim", "asym_reward_auto_sim","ucb_sim", 
                "ucb_asym_rpe_sim", "ucb_asym_reward_sim", "ucb_auto_sim",
                "ucb_asym_rpe_auto_sim", "ucb_asym_reward_auto_sim")


for (s in 1:length(seeds2)){
  seed <- seeds2[s]
  for (t in 1:r){
    print(counter)
    counter <- counter + 1
    innovation_term <<- rs[t,5]
    arm <<- rs[t,3]
    set.seed(seed)
    nt <- rs[t,2]
    
    rewards <- reward_generator(rs[t,1], rs[t,2], rs[t,3], rs[t,4], rs[t,5], 
                                rs[t,6:(5+arm)])
    
    par1 <- par_1[sample(nrow(par_1),1),]
    par1 <- par_df[par1$Model, 1:2, par1$ID, par1$Seed]
    
    par2 <- par_2[sample(nrow(par_2),1),]
    par2 <- par_df[par2$Model, 1:3, par2$ID, par2$Seed]  
    
    par3 <- par_3[sample(nrow(par_3),1),]
    par3 <- par_df[par3$Model, 1:3, par3$ID, par3$Seed]  
    
    par4 <- par_4[sample(nrow(par_4),1),]
    par4 <- par_df[par4$Model, 1:4, par4$ID, par4$Seed]  
    
    par5 <- par_5[sample(nrow(par_5),1),]
    par5 <- par_df[par5$Model, 1:5, par5$ID, par5$Seed]  
    
    par6 <- par_6[sample(nrow(par_6),1),]
    par6 <- par_df[par6$Model, 1:5, par6$ID, par6$Seed]
    
    par7 <- par_7[sample(nrow(par_7),1),]
    par7 <- par_df[par7$Model, 2:5, par7$ID, par7$Seed]  
    par7 <- append(par7, innovation_term, after = 2)
    
    par8 <- par_8[sample(nrow(par_8),1),]
    par8 <- par_df[par8$Model, 2:6, par8$ID, par8$Seed]  
    par8 <- append(par8, innovation_term, after = 2)
    
    par9 <- par_9[sample(nrow(par_9),1),]
    par9 <- par_df[par9$Model, 2:6, par9$ID, par9$Seed]  
    par9 <- append(par9, innovation_term, after = 2)
    
    par10 <- par_10[sample(nrow(par_10),1),]
    par10 <- par_df[par10$Model, 2:7, par10$ID, par10$Seed]
    par10 <- append(par10, innovation_term, after = 2)
    
    par11 <- par_11[sample(nrow(par_11),1),]
    par11 <- par_df[par11$Model, 2:8, par11$ID, par11$Seed]  
    par11 <- append(par11, innovation_term, after = 2)
    
    par12 <- par_12[sample(nrow(par_12),1),]
    par12 <- par_df[par12$Model, 2:8, par12$ID, par12$Seed]  
    par12 <- append(par12, innovation_term, after = 2)
    
    
    pars <- list(par1, par2, par3, par4, par5, par6, par7, par8, par9, 
                   par10, par11, par12)
  
    

    
    dat <- sim_behav_data_csv(rewards, agent_sims, pars, arm, nt)

    

    # model 1
    est <- find_the_best_pars(c(.0001, .0001), c(100, 1), sym_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 1, t, s] <- as.vector(unlist(est[i]))
    }
    
    # model 2
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(10, 1, 1), asym_rpe_negLogLik_DE, dat, nm, nt)$value

    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 2, t, s] <- as.vector(unlist(est[i]))
    }
    
    # model 3
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(10, 1, 1), asym_reward_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 3, t, s] <- as.vector(unlist(est[i]))
    }
    
    
    
    # model 4
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001,-100), c(10, 1, 1,100), sym_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 4, t, s] <- as.vector(unlist(est[i]))
    }
    
    
    
    # model 5
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001, 0.0001, -100), c(10, 1, 1, 1, 100), asym_rpe_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 5, t, s] <- as.vector(unlist(est[i]))
    }
    
    
    
    # model 6
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001, 0.0001, -100), c(10, 1, 1, 1, 100), asym_reward_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 6, t, s] <- as.vector(unlist(est[i]))
    }
    
    
    
    # model 7
    est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001), c(10 , 100, 1000, 100, 100), ucb_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 7, t, s] <- as.vector(unlist(est[i]))
    }
    
    
    # model 8
    est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001, 0.0001), c(10 , 100, 1000, 100, 100, 100), ucb_asym_rpe_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 8, t, s] <- as.vector(unlist(est[i]))
    }
    
    
    # model 9
    est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001, 0.0001), c(10 , 100, 1000, 100, 100, 100), ucb_asym_reward_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 9, t, s] <- as.vector(unlist(est[i]))
    }
    
    
    # model 10
    est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, -100), c(10 , 100, 1000, 100, 100, 1, 100), ucb_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 10, t, s] <- as.vector(unlist(est[i]))
    }
    
    
    # model 11
    est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 1000, 100, 100, 100, 1, 100), ucb_asym_rpe_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 11, t, s] <- as.vector(unlist(est[i]))
    }
    
    
    
    # model 12
    est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 1000, 100, 100, 100, 1, 100), ucb_asym_reward_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      x <- (i-1)*nt + 1
      y <- i*nt
      mat_value[i, 12, t, s] <- as.vector(unlist(est[i]))
    }
    
    # saveRDS(mat_value, "mat_value_emp_fit.RDS")

    
  }
}








