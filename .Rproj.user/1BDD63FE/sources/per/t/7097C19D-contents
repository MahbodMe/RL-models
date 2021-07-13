

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


