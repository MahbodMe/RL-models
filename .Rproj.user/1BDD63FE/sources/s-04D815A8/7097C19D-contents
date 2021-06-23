

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
  qs <- rl_symmetric(choice, reward, 4, 0, alpha)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}


est <- optim(c(3,.5),rl_sym_negLogLik,data=df_sim_1[1:200,])



rl_sym_negLogLik_DE <- function(par,data,x) {
  negLL <- rl_sym_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}


est_DE <- DEoptim(rl_sym_negLogLik_DE,lower=c(0.0001,0.0001),
                  upper=c(100, 1),data=df_sim_1[1:200,],
                  control=DEoptim.control(trace=FALSE))



est_DE$optim$bestmem

rl_sym_negLogLik(c(0.4832322, 0.5417457), df_sim_1)


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
  qs <- rl_asymmetric(choice, reward, 4, 0, p_alpha, n_alpha)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}


est <- optim(c(3,.4, .6),rl_asym_negLogLik,data=df_sim_1[1:200,])

rl_asym_negLogLik(c(0.5141729, 0.1458994, 0.9025990), df_sim_1)


rl_sym_negLogLik_DE <- function(par,data,x) {
  negLL <- rl_asym_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}


est_DE <- DEoptim(rl_sym_negLogLik_DE,lower=c(0.0001,0.0001,0.0001),
                  upper=c(100, 1, 1),data=df_sim_1[1:200,],
                  control=DEoptim.control(trace=FALSE))



est_DE$optim$bestmem

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
  qs <- rl_asymmetric_2(choice, reward, 4, 0, p_alpha, n_alpha)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}


est <- optim(c(3,.4, .6),rl_asym_negLogLik_2,data=df_sim_1[1:200,])

x <- est$par

rl_asym_negLogLik_2(x, df_sim_1)


rl_sym_negLogLik_DE <- function(par,data,x) {
  negLL <- rl_asym_negLogLik_2(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}


est_DE <- DEoptim(rl_sym_negLogLik_DE,lower=c(0.0001,0.0001,0.0001),
                  upper=c(100, 1, 1),data=df_sim_1[1:200,],
                  control=DEoptim.control(trace=FALSE))



est_DE$optim$bestmem

rl_asym_negLogLik_2(c(0.3989881, 0.4195197, 0.6916996), df_sim_1)

