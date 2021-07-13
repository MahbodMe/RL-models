
### Required packages 
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("nnet")
# install.packages("DEoptim")


library(dplyr)
library(tidyr)
library(ggplot2)
library(nnet)
library(DEoptim)


### the following function generates rewards 

reward_generator <- function(seed, nt, no, error_term, innovation_term, mu0){
  
  set.seed(seed) # set the random seed to replicate these results exactly
  mu <- matrix(0.0,nrow=nt,ncol=no) # to store the time-varying means
  mu[1,] <- mu0 # initialize the means for t=1
  for(t in 2:nt) {
    mu[t,] <- mu[t-1,] + rnorm(no,mean=0,sd=innovation_term) # compute mean at t based on mean at t-1
  }
  rewards <- mu + rnorm(length(mu),mean=0,sd=error_term) # generate the stochastic rewards
  rewards <- as.data.frame(cbind(1:nt,rewards))
  colnames(rewards) <- colnames(mu) <- c("trial",paste0("A",1:no))
  
  return(rewards)
}


### the following function plots rewards 

plot_rewards <- function(data){
  return(
    data %>%
      gather(key=option,value=data,-trial) %>%
      ggplot(aes(x=trial,y=data,colour=option)) + geom_line()
  )
}







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
  return(list(q_values = q_values, choices = choices, 
              agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards) ))
}





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
    
    q_values[t+1,] <- q_values[t,] + discount*alpha*
      (agents_rewards[t] - q_values[t,])
    
  }
  return(list(q_values = q_values, choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  # Returning variables of interest 
}






### Symmetric Simple Q learning model 


rl_sym_auto_sim <- function(rewards, alpha, beta, phi, tau) {
  
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
    softmax_probs <- exp(beta*q_values[t,] + phi*choice_trace[t+1,]) 
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
    choice_trace[t+1, ] <- choice_trace[t,] -tau*choice_trace[t,] + 
      tau*indicator
    
  }
  return(list(q_values = q_values, choices = choices, agents_rewards = agents_rewards, 
              agents_total_rewards = sum(agents_rewards)))
  
}



### Asymmetric Simple Q learning model (RPE)

rl_asym_auto_sim <- function(rewards, positive_alpha, negative_alpha, 
                             beta, phi, tau){
  
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
    softmax_probs <- exp(beta*q_values[t,] + phi*choice_trace[t+1,]) 
    
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





rl_ucb_sim2 <- function(rewards,m0,v0,sigma_xi_sq,sigma_epsilon_sq,beta,gamma) {
  rewards <- rewards[,-1]
  nt <- nrow(rewards)
  no <- ncol(rewards)
  m <- matrix(m0,ncol=no,nrow=nt+1)
  v <- matrix(v0,ncol=no,nrow=nt+1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  for(t in 1:nt) {
    # choose the option with the highest UCB, breaking ties at random
    
    choice[t] <- which.is.max((m[t,] + beta*sqrt(v[t,] + sigma_xi_sq))*gamma)
    
    reward[t] <- rewards[t,choice[t]]
    # Kalman updates:
    kt <- rep(0,no)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, agents_total_rewards = sum(reward)))
}



rl_ucb_auto_sim2 <- function(rewards,m0,v0,sigma_xi_sq,sigma_epsilon_sq,
                             beta,gamma,tau,phi) {
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
    
    choice[t] <- which.is.max((m[t,] + beta*sqrt(v[t,] + sigma_xi_sq + choice_trace[t,])) *gamma)
    
    reward[t] <- rewards[t,choice[t]]
    # Kalman updates:
    kt <- rep(0,no)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/
      (v[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
    
    
    indicator <- rep(0,no)
    indicator[choice[t]] <- 1
    choice_trace[t+1, ] <- (choice_trace[t,] - tau*choice_trace[t,] 
                            + tau*indicator)*phi
    
  }
  
  return(list(m=m,v=v,choices=choice, agents_rewards=reward, agents_total_rewards = sum(reward)))
}



# the following function takes an environment (data), a list of models (funs)  
# and a list of parameters for those models (pars); and generates a data set
# for each of the models 



sim_behav_data_1 <- function(data, funs, pars, no, nt, envir = parent.frame()){
  
  
  mat <- matrix(0, nrow = length(funs)*nt, ncol = 3)
  
  
  
  
  for (i in 1:length(funs)){
    
    fun <- funs[i]
    par <- as.numeric(unlist(pars[i]))
    
    
    n_par <- length(par)
    
    
    sim <- do.call(fun, c(list(data), par[1:n_par]))
    
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




### Parameter estimation simple symmetric Q-learning 


rl_symmetric <- function(choice, reward, n_options, q_value_0, alpha){
  
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  
  for (t in 1:(length(choice)-1)) {
    
    
    discount <- rep(0, n_options)
    discount[choice[t]] <- 1
    q_value[t+1, ] <- q_value[t,] + discount*alpha*(reward[t] - q_value[t,])
  }
  return(list(q_value=q_value))
}



softmax_simple_choice_prob <- function(q_values, beta) {
  prob <- exp(q_values*beta)
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



rl_sym_negLogLik <- function(par, data){
  beta <- par[1]
  alpha <- par[2]
  choice <- data$deck
  reward <- data$payoff
  qs <- rl_symmetric(choice, reward, arm, 0, alpha)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(arm:nrow(data),choice[arm:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}




rl_sym_negLogLik_DE <- function(par,data) {
  negLL <- rl_sym_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}



### Parameter estimation simple asymmetric Q-learning (RPE)


rl_asymmetric <- function(choice, reward, n_options, q_value_0, positive_alpha, 
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



softmax_simple_choice_prob <- function(q_values, beta) {
  prob <- exp(q_values*beta)
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



rl_asym_negLogLik <- function(par, data){
  beta <- par[1]
  p_alpha <- par[2]
  n_alpha <- par[3]
  choice <- data$deck
  reward <- data$payoff
  qs <- rl_asymmetric(choice, reward, arm, 0, p_alpha, n_alpha)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(arm:nrow(data),choice[arm:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}



rl_asym_negLogLik_DE <- function(par,data) {
  negLL <- rl_asym_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}




### Parameter estimation simple symmetric Q-learning + auto 


rl_symmetric_auto <- function(choice, reward, n_options, q_value_0, alpha, tau
){
  
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



softmax_simple_choice_prob_auto <- function(q_values, beta, ct) {
  prob <- exp(beta*(q_values + ct))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



rl_sym_auto_negLogLik <- function(par, data){
  beta <- par[1]
  alpha <- par[2]
  phi <- par[3]
  tau <- par[4]
  choice <- data$deck
  reward <- data$payoff
  ct <- rl_symmetric_auto(choice, reward, arm, 0, alpha, tau)$choice_trace*phi
  
  qs <- rl_symmetric_auto(choice, reward, arm, 0, alpha, tau)$q_value
  probs <- softmax_simple_choice_prob_auto(qs, beta, ct)
  prob_choice <- probs[cbind(arm:nrow(data),choice[arm:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}


rl_sym_auto_negLogLik_DE <- function(par,data) {
  negLL <- rl_sym_auto_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}



### Parameter estimation simple asymmetric Q-learning (RPE)

rl_asymmetric_auto <- function(choice, reward, n_options, q_value_0, 
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



softmax_simple_choice_prob_auto <- function(q_values, beta, ct) {
  prob <- exp(beta*(q_values + ct))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



rl_asym_auto_negLogLik <- function(par, data){
  beta <- par[1]
  p_alpha <- par[2]
  n_alpha <- par[3]
  phi <- par[4]
  tau <- par[5]
  
  choice <- data$deck
  reward <- data$payoff
  ct <- rl_asymmetric_auto(choice, reward, arm, 0, p_alpha, n_alpha, tau)$choice_trace*phi
  
  qs <- rl_asymmetric_auto(choice, reward, arm, 0, p_alpha, n_alpha, tau)$q_value
  probs <- softmax_simple_choice_prob_auto(qs, beta, ct)
  prob_choice <- probs[cbind(arm:nrow(data),choice[arm:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}



rl_asym_auto_negLogLik_DE <- function(par,data) {
  negLL <- rl_asym_auto_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}



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



rl_ucb_negLogLik_2 <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- par[3]
  error_term <- par[4]
  beta <- par[5] 
  innovation_term <- it
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, arm, initial_mean, initial_variance,
                innovation_term, error_term)
  
  arm_mean <- kal$arm_mean
  
  ppsd <- sqrt(kal$arm_variance + innovation_term)
  
  prob <- softmax_uncertain_choice_prob(arm_mean, ppsd, gamma, beta)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


kf_sm_negLogLik_DE <- function(par,data) {
  negLL <- rl_ucb_negLogLik_2(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}


### Parameter estimation symmetric kalman + UCB + auto


rl_ucb_auto <- function(choice, reward, n_options, initial_mean, initial_variance,
                        innovation_term, error_term, beta, tau) {
  
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(initial_mean, ncol = n_options, nrow = n_trials + 1) 
  arm_variance <- matrix(initial_variance, ncol = n_options, 
                         nrow = n_trials + 1)
  
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials)
  
  for (t in 1:(length(choice)-1)) {
    
    
    discount <- rep(0,n_options)
    
    discount[choice[t]] <- 1
    
    # Updating kalman gain values 
    kalman_t <- rep(0,n_options)
    kalman_t <- (arm_variance[choice[t],] + innovation_term) / 
      (arm_variance[choice[t],] + innovation_term + error_term)
    
    
    arm_mean[t+1, ] <- arm_mean[t, ] + 
      discount*kalman_t*(reward[t]-arm_mean[t, ]) 
    
    # Updating the variance of the posterior distribution of the mean utility
    arm_variance[t+1, ] <- (1-(discount*kalman_t))*(arm_variance[t, ]+innovation_term)
    
    
    
    # Updating the choice trace
    
    choice_trace[t+1, ] <- choice_trace[t,]*(1 - tau) + tau*discount
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance,
              choice_trace = choice_trace))
}


softmax_uncertain_auto_choice_prob <- function(arm_mean, ppsd, gamma, beta, ct) {
  prob <- exp(gamma*(arm_mean + ppsd*beta + ct))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



rl_ucb_auto_negLogLik <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- par[3]
  error_term <- par[4]
  beta <- par[5] 
  innovation_term <- it
  tau <- par[6]
  phi <- par[7]
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb_auto(choice, reward, arm, initial_mean, initial_variance,
                     innovation_term, error_term, beta, tau)
  
  arm_mean <- kal$arm_mean[1:(nrow(data)),]
  
  ppsd <- sqrt(kal$arm_variance[1:(nrow(data)),] + innovation_term)
  
  ct <- kal$choice_trace[1:(nrow(data)),]*phi
  
  prob <- softmax_uncertain_auto_choice_prob(arm_mean, ppsd, gamma, beta, ct)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}



kf_sm_auto_negLogLik_DE <- function(par,data) {
  negLL <- rl_ucb_auto_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}










### takes a list of function and parameters and data, produces para_est

find_the_best_pars <- function(lpar, upar, fun, df, nm, nt){
  df_set <- rep("NA", nm)
  parameter <- rep(0, nm)
  for (i in 1:nm){
    x <- (i-1)*nt + 1
    y <- i*nt
    est_DE <- DEoptim(fun,lower=as.double(lpar),
                      upper=as.double(upar),data=df[x:y,],
                      control=DEoptim.control(trace=FALSE))
    df_set[i] <- i
    parameter[i] <- list(est_DE$optim$bestmem)
  }
  return(list( parameter = parameter, df_set = df_set))
}

sds_inv <- seq(2,8, by=1)
sds_err <- seq(2,8, by=1)
trials <- seq(30,80, by=10)
arms <- seq(2,8, by=2)
seeds <- c(3, 30, 300, 3000)
m_0 <- c(-4,4,-12,12,-20,20,-60,60)




r <- length(sds_inv)*length(sds_err)*length(trials)*length(arms)*length(seeds)

rs <- matrix(NA, r, 13)
colnames(rs) <- c("seed", "trials", "nArms", "innovation", "error", "A1", "A2"
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



agent_sims <- c("rl_simpleQ_symmetric_sim", "rl_simpleQ_asymmetric_sim", 
                "rl_sym_auto_sim", "rl_asym_auto_sim", 
                "rl_ucb_sim2", "rl_ucb_auto_sim2")


pars <- list(c(.5, 1), c(.3, .6, 1),  c(.5, 1, .5, .8), c(.3, .6, 1, .5, .8),
             c(0.0, 1000, 16, 16, 1.96, 1), c(0.0, 1000, 16, 16, 1.96, 1, .8, .5))


nm <- 6


m_values <- array(0, c(nm,nm,r))

rownames(m_values) <- rep(c("model_1", "model_2", "model_4", "model_5", "model_7", "model_8"), 1)
colnames(m_values) <- rep(c("sim_model_1", "sim_model_2", "sim_model_4", "sim_model_5", 
                     "sim_model_7", "sim_model_8"), 1)


for (k in 1:1){
  it <<- rs[k,4]
  arm <<- rs[k,3]
  nt <- rs[k,2]
  rewards <- reward_generator(rs[k,1], rs[k,2], rs[k,3], rs[k,4], rs[k,5], 
                              rs[k,6:(5+arm)])
  
  reward_generator(3,30,4,4,4,rep(0,4))
  
  # plot_rewards(rewards)
  # 
  # mn_r <- min(rewards[,2:arm+1])
  # mx_x <- max(rewards[,2:arm+1])
  # 
  # rewards[,2:arm+1] <- 2*((rewards[,2:arm+1]-mn_r)/(mx_x-mn_r))-1
  # plot_rewards(rewards)
  

  dat <- sim_behav_data_1(rewards, agent_sims, pars, arm, nt)
  
  
  par1 <- find_the_best_pars(c(.0001, .0001), c(100, 1), 
                             rl_sym_negLogLik_DE, dat, nm, nt)$parameter
  
  for (i in 1:nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    value <- rl_sym_negLogLik(as.numeric(unlist(par1[i])), data=dat[x:y,])
    
    m_values[1, i, k] <- value
  }
  
  par2 <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), 
                             rl_asym_negLogLik_DE, dat, nm, nt)$parameter
  

  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_asym_negLogLik(as.numeric(unlist(par2[i])), data=dat[x:y,]) 
    m_values[2, i, k] <- value
  }
  
  
  
  par3 <- find_the_best_pars(c(0.0001, 0.0001, .001, .001), c(100, 1, 100 ,1),
                             rl_sym_auto_negLogLik_DE, dat, nm, nt)$parameter

  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_sym_auto_negLogLik(as.numeric(unlist(par3[i])), data=dat[x:y,])
    print(value)
    m_values[3, i, k] <- value
  }
  
  
  par4 <- find_the_best_pars(c(0.0001, 0.0001, .0001,.1, .1), 
                             c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE, 
                             dat, nm, nt)$parameter

  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_asym_auto_negLogLik(as.numeric(unlist(par4[i])), data=dat[x:y,]) 
    
    m_values[4, i, k] <- value
  }
  
  
  par5 <- find_the_best_pars(c(0,-100,0.001,0.0001, 0.001),
                             c(10,100,1000,100, 50), kf_sm_negLogLik_DE, 
                             dat, nm, nt)$parameter
  

  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_ucb_negLogLik_2(as.numeric(unlist(par5[i])), data=dat[x:y,]) 
    
    m_values[5, i, k] <- value
  }
  
  
  
  par6 <- find_the_best_pars(c(0,-100,0.00001,0.00001, 0.0001, .1, .1),
                             c(10,100,1000,100, 50, 1, 100), kf_sm_auto_negLogLik_DE, 
                             dat, nm, nt)$parameter

  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_ucb_auto_negLogLik(as.numeric(unlist(par6[i])), data=dat[x:y,]) 
    m_values[6, i, k] <- value
  }
  
  
}



saveRDS(m_values, "scores.rds")



