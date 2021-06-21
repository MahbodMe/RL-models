

### Parameter estimation simple symmetric Q-learning 

rl_symmetric <- function(choice, reward, n_options, q_value_0, alpha){
  
  q_value <- matrix(q_value_0, ncol=n_options, nrow=length(choice))
  
  for (t in 1:(length(choice)-1)) {
    
    qs <- rep(0, n_options)
    qs[choice[t]] <- q_value[t,] + alpha*(reward[t] - q_value[t,])
    q_value[t+1, ] <- qs
  }
  return(list(q_values = q_value))
}


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

rl_sym_lik <- function(par, data){
  beta <- par[1]
  choice <- data$deck
  reward <- data$payoff
  qs <- rl_symmetric(choice, reward, 4, 0, .5)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  lik <- prod(prob_choice)
  return(list(lik))
}


beta <- seq(0,2, length=300)
out <- rep(NA,length=length(beta))
for (i in 1:length(beta)) out[i] <- rl_sym_lik(c(beta[i]), df_sim_1)
plot(beta,out,ylab="likelihood",xlab=expression(beta))


beta[which.max(out)]


rl_sym_logLik <- function(par, data){
  beta <- par[1]
  choice <- data$deck
  reward <- data$payoff
  qs <- rl_symmetric(choice, reward, 4, 0, .5)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  logLik <- sum(log(prob_choice))
  return(logLik)
}


beta <- seq(-1,2, length=300)
out <- rep(NA,length=length(beta))
for (i in 1:length(beta)) out[i] <- rl_sym_logLik(c(beta[i]), df_sim_1)
plot(beta,out,ylab="Log-likelihood",xlab=expression(beta))




beta[which.max(out)]


rl_sym_negLogLik <- function(par, data){
  -rl_sym_logLik(par,data)
}

rl_sym_negLogLik <- function(par, data){
  beta <- par[1]
  choice <- data$deck
  reward <- data$payoff
  qs <- rl_symmetric(choice, reward, 4, 0, .5)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(4:nrow(data),choice[4:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}

beta <- seq(0,3, length=300)
out <- rep(NA,length=length(beta))
for (i in 1:length(beta)) out[i] <- rl_sym_negLogLik(c(beta[i]), df_sim_1)
plot(beta,out,ylab="likelihood",xlab=expression(beta))
beta[which.min(out)]


optim(par=.25,fn=rl_sym_negLogLik,data=df_sim_1,)

optim(par=3,fn=rl_sym_negLogLik,method="Brent",lower=0,upper=8,data=df_sim_1)






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


est <- optim(c(3,.5),rl_sym_negLogLik,data=df_sim_1)





