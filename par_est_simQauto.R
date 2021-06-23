

### Parameter estimation simple symmetric Q-learning 


rl_symmetric_auto <- function(choice, reward, n_options, q_value_0, alpha, tau
                         ){
  
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials)
  
  for (t in 1:(length(choice)-1)) {
    
    
    discount <- rep(0, n_options)
    discount[choice[t]] <- 1
    q_value[t+1, ] <- q_value[t,] + discount*alpha*(reward[t] - q_value[t,])
    
    
    choice_trace[t+1, ] <- choice_trace[t,]*(1 - tau) + tau*discount
  } 
  
  return(list(q_value=q_value, choice_trace=choice_trace))
}



softmax_simple_choice_prob_auto <- function(q_values, beta, ct) {
  prob <- exp(q_values*beta + ct)
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
  ct <- rl_symmetric_auto(choice, reward, 4, 0, alpha, tau)$choice_trace*phi

  qs <- rl_symmetric_auto(choice, reward, 4, 0, alpha, tau)$q_value
  probs <- softmax_simple_choice_prob_auto(qs, beta, ct)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}

rl_sym_auto_negLogLik(c(0.4831606,  0.5417335, 0.5944809, 3.0992784), df_sim_1)


est <- optim(c(3,.5, .4, 1),rl_sym_negLogLik,data=df_sim_1[1:200,])


rl_sym_auto_negLogLik_DE <- function(par,data,x) {
  negLL <- rl_sym_auto_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}


est_DE <- DEoptim(rl_sym_auto_negLogLik_DE,lower=c(0.0001, 0.0001, .0001, .0001),
                  upper=c(100, 100, 100 ,1),data=df_sim_1[1:200,],
                  control=DEoptim.control(trace=FALSE))



est_DE$optim$bestmem
rl_sym_auto_negLogLik(c(0.45887852, 0.55840368, 0.49851618, 0.04856763), df_sim_1)




### Parameter estimation simple asymmetric Q-learning (RPE)

rl_asymmetric_auto <- function(choice, reward, n_options, q_value_0, 
                              positive_alpha, negative_alpha, tau){
  
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials)
  
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
  prob <- exp(q_values*beta + ct)
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
  ct <- rl_asymmetric_auto(choice, reward, 4, 0, p_alpha, n_alpha, tau)$choice_trace*phi
  
  qs <- rl_asymmetric_auto(choice, reward, 4, 0, p_alpha, n_alpha, tau)$q_value
  probs <- softmax_simple_choice_prob_auto(qs, beta, ct)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}

rl_sym_auto_negLogLik(c(0.4831606,  0.5417335, 0.5944809, 3.0992784), df_sim_1)


est <- optim(c(3,.5, .4, 1),rl_sym_negLogLik,data=df_sim_1[1:200,])


rl_asym_auto_negLogLik_DE <- function(par,data,x) {
  negLL <- rl_asym_auto_negLogLik(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}


est_DE <- DEoptim(rl_asym_auto_negLogLik_DE,lower=c(0.0001, 0.0001, .0001,
                                                    .0001, .0001),
                  upper=c(100, 1, 1, 100 ,1),data=df_sim_1[1:200,],
                  control=DEoptim.control(trace=FALSE))



est_DE$optim$bestmem
rl_asym_auto_negLogLik(c(0.46789634, 0.53256006, 0.56212901, 0.44132140, 0.04493842), df_sim_1)




### Parameter estimation simple auto + asymmetric Q-learning (reward magnitude)


rl_asymmetric_auto_2 <- function(choice, reward, n_options, q_value_0, 
                               positive_alpha, negative_alpha, tau){
  
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  
  choice_trace <- matrix(0, ncol = n_options, nrow = n_trials)
  
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
  prob <- exp(q_values*beta + ct)
  prob <- prob/rowSums(prob)
  return (prob = prob)
}



rl_asym_auto_negLogLik_2 <- function(par, data){
  beta <- par[1]
  p_alpha <- par[2]
  n_alpha <- par[3]
  phi <- par[4]
  tau <- par[5]
  
  choice <- data$deck
  reward <- data$payoff
  ct <- rl_asymmetric_auto_2(choice, reward, 4, 0, p_alpha, n_alpha, tau)$choice_trace*phi
  
  qs <- rl_asymmetric_auto_2(choice, reward, 4, 0, p_alpha, n_alpha, tau)$q_value
  probs <- softmax_simple_choice_prob_auto(qs, beta, ct)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}




est <- optim(c(3,.5, .4, 1),rl_sym_negLogLik,data=df_sim_1[1:200,])


rl_asym_auto_negLogLik_DE_2 <- function(par,data,x) {
  negLL <- rl_asym_auto_negLogLik_2(par,data)
  print("estimating...")
  if(is.na(negLL)) negLL <- Inf
  return(negLL)
}


est_DE <- DEoptim(rl_asym_auto_negLogLik_DE_2,lower=c(0.0001, 0.0001, .0001,
                                                    .0001, .1),
                  upper=c(100, 1, 1, 100 ,1),data=df_sim_1[1:200,],
                  control=DEoptim.control(trace=FALSE))



est_DE$optim$bestmem

rl_asym_auto_negLogLik_2(c(0.3989783966, 0.4195172074, 0.6916966498, 0.0001000065, 0.1000114169), df_sim_1)


rl_asym_auto_negLogLik_2(c(0.45909484, 01.55800077, 0.87450579, 1.49196802, 1 ), df_sim_1)


