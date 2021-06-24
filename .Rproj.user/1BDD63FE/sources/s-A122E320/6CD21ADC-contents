












### Parameter estimation simple symmetric Q-learning 


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
  ct <- rl_symmetric_auto(choice, reward, 2, 0, alpha, tau)$choice_trace*phi
  
  qs <- rl_symmetric_auto(choice, reward, 2, 0, alpha, tau)$q_value
  probs <- softmax_simple_choice_prob_auto(qs, beta, ct)
  prob_choice <- probs[cbind(2:nrow(data),choice[2:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}



est <- optim(c(3,.5, .2, 1),rl_sym_negLogLik,data=df_sim_1[1:200,])
est

rl_sym_auto_negLogLik_DE <- function(par,data,x) {
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
  ct <- rl_asymmetric_auto(choice, reward, 2, 0, p_alpha, n_alpha, tau)$choice_trace*phi
  
  qs <- rl_asymmetric_auto(choice, reward, 2, 0, p_alpha, n_alpha, tau)$q_value
  probs <- softmax_simple_choice_prob_auto(qs, beta, ct)
  prob_choice <- probs[cbind(2:nrow(data),choice[2:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}



rl_asym_auto_negLogLik_DE <- function(par,data,x) {
  negLL <- rl_asym_auto_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}




### Parameter estimation simple auto + asymmetric Q-learning (reward magnitude)


rl_asymmetric_auto_2 <- function(choice, reward, n_options, q_value_0, 
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



softmax_simple_choice_prob_auto <- function(q_values, beta, ct) {
  prob <- exp(beta*(q_values + ct))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



rl_asym_auto_negLogLik_2 <- function(par, data){
  beta <- par[1]
  p_alpha <- par[2]
  n_alpha <- par[3]
  phi <- par[2]
  tau <- par[5]
  
  choice <- data$deck
  reward <- data$payoff
  ct <- rl_asymmetric_auto_2(choice, reward, 2, 0, p_alpha, n_alpha, tau)$choice_trace*phi
  
  qs <- rl_asymmetric_auto_2(choice, reward, 2, 0, p_alpha, n_alpha, tau)$q_value
  probs <- softmax_simple_choice_prob_auto(qs, beta, ct)
  prob_choice <- probs[cbind(2:nrow(data),choice[2:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}







rl_asym_auto_negLogLik_DE_2 <- function(par,data,x) {
  negLL <- rl_asym_auto_negLogLik_2(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
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
  qs <- rl_symmetric(choice, reward, 2, 0, alpha)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(2:nrow(data),choice[2:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}



rl_sym_negLogLik_DE <- function(par,data,x) {
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
    rpe <- reward[t] - q_value[t, choice[t]]

    if ((rpe) < 0){
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
  qs <- rl_asymmetric(choice, reward, 2, 0, p_alpha, n_alpha)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(2:nrow(data),choice[2:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}



rl_asym_negLogLik_DE <- function(par,data,x) {
  negLL <- rl_asym_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}

### Parameter estimation simple asymmetric Q-learning (reward magnitude)

rl_asymmetric_2 <- function(choice, reward, n_options, q_value_0, positive_alpha, 
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



softmax_simple_choice_prob <- function(q_values, beta) {
  prob <- exp(q_values*beta)
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



rl_asym_negLogLik_2 <- function(par, data){
  beta <- par[1]
  p_alpha <- par[2]
  n_alpha <- par[3]
  choice <- data$deck
  reward <- data$payoff
  qs <- rl_asymmetric_2(choice, reward, 2, 0, p_alpha, n_alpha)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(2:nrow(data),choice[2:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}



rl_asym_negLogLik_DE_2 <- function(par,data,x) {
  negLL <- rl_asym_negLogLik_2(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}



library(readr)
library(dplyr)
df <- read_csv("factual.csv")


mat <- matrix(data = 0, nrow = 6, ncol =15)

for (t in 1:1){
  dat <- df %>%
    filter(subjectid==t, type==1) %>%
    rename(deck = choice)
  
  dat <- dat %>%  
    mutate(payoff = reward)
  
  dat <- dat %>%
    filter(deck != 0)

  
  nt <- length(dat$deck)
  nm <- 1

  rl_sym_negLogLik(c(.5,.5), dat)
  

  
  par1 <- find_the_best_pars(c(.0001, .0001), c(100, 1), rl_sym_negLogLik_DE, dat, nm, nt)$parameter
  

  value <- rl_sym_negLogLik(as.numeric(unlist(par1[i])), data=dat) 

  mat[1, t] <- value

  
  
  par2 <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), rl_asym_negLogLik_DE, dat, nm, nt)$parameter
  

  value <- rl_asym_negLogLik(as.numeric(unlist(par2[i])), data=dat) 
  print(value)
  mat[2, t] <- value

  par3 <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), rl_asym_negLogLik_DE_2, dat, nm, nt)$parameter
  

  value <- rl_asym_negLogLik_2(as.numeric(unlist(par3[i])), data=dat) 
  print(value)
  mat[3, t] <- value
  
  par4 <- find_the_best_pars(c(0.0001, 0.0001, .1, .1), c(100, 1, 100 ,1), rl_sym_auto_negLogLik_DE, dat, nm, nt)$parameter
  
    
  value <- rl_sym_auto_negLogLik(as.numeric(unlist(par4[i])), data=dat) 
  print(value)
  mat[4, t] <- value


  par5 <- find_the_best_pars(c(0.0001, 0.0001, .0001,.1, .1), 
                             c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE, 
                             dat, nm, nt)$parameter
  

  value <- rl_asym_auto_negLogLik(as.numeric(unlist(par5[i])), data=dat) 
  print(value)
  mat[5, t] <- value

  
  par6 <- find_the_best_pars(c(0.0001, 0.0001, .0001,.1, .1), 
                             c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE_2, 
                             dat, nm, nt)$parameter
  

  value <- rl_asym_auto_negLogLik_2(as.numeric(unlist(par6[i])), data=dat) 
  print(value)
  mat[6, t] <- value

}


dat <- df %>%
  filter(subjectid==3, type==1) %>%
  rename(deck = choice)

dat <- dat %>%  
  mutate(payoff = reward)

dat <- dat %>%
  filter(deck != 0)


par5 <- find_the_best_pars(c(0.0001, 0.0001, .0001,.1, .1), 
                           c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE, 
                           dat, nm, nt)$parameter


value <- rl_asym_auto_negLogLik(as.numeric(unlist(par5)), data=dat) 
print(value)
mat[5, t] <- value
  
