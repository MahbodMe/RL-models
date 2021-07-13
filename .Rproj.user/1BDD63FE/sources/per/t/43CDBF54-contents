
### parameter estimation and model comparison for the Lefebvre (2017) study



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



est <- optim(c(3,.5, .2, 1),rl_sym_auto_negLogLik,data=dat)
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



np <- length(unique(df$subjectid))

mat_value <- matrix(data = 0, nrow = np, ncol =6)
mat_n <- matrix(data = 0, nrow = np, ncol = 1)

mat_par <- matrix(data = 0 , nrow = np*6, ncol = 6)
n <- 1

for (t in 1:np){
  dat <- df %>%
    filter(subjectid==t, type==1) %>%
    rename(deck = choice)
  
  dat <- dat %>%  
    mutate(payoff = reward)
  
  dat <- dat %>%
    filter(deck != 0)
  
  
  nt <- length(dat$deck)
  nm <- 1
  mat_n[t, 1] <- nt
  
  rl_sym_negLogLik(c(.5,.5), dat)
  
  
  
  par <- find_the_best_pars(c(.0001, .0001), c(100, 1), rl_sym_negLogLik_DE, dat, nm, nt)$parameter
  
  
  value <- rl_sym_negLogLik(as.numeric(unlist(par)), data=dat) 
  
  mat_value[t, 1] <- value
  mat_par[n, 1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  n <- n+1
  
  par <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), rl_asym_negLogLik_DE, dat, nm, nt)$parameter
  
  
  value <- rl_asym_negLogLik(as.numeric(unlist(par)), data=dat) 
  print(value)
  mat_value[t, 2] <- value
  mat_par[n,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  n <- n+1
  
  
  par <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), rl_asym_negLogLik_DE_2, dat, nm, nt)$parameter
  
  
  value <- rl_asym_negLogLik_2(as.numeric(unlist(par)), data=dat) 
  print(value)
  mat_value[t, 3] <- value
  mat_par[n,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  n <- n+1  
  
  par <- find_the_best_pars(c(0.0001, 0.0001, .0001, .0001), c(100, 1, 100 ,1), rl_sym_auto_negLogLik_DE, dat, nm, nt)$parameter
  
  
  value <- rl_sym_auto_negLogLik(as.numeric(unlist(par)), data=dat) 
  print(value)
  mat_value[t, 4] <- value
  mat_par[n,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  n <- n+1
  
  par <- find_the_best_pars(c(0.0001, 0.0001, .0001,.0001, .0001), 
                            c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE, 
                            dat, nm, nt)$parameter
  
  
  value <- rl_asym_auto_negLogLik(as.numeric(unlist(par5)), data=dat) 
  print(value)
  mat_value[t, 5] <- value
  mat_par[n,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  n <- n+1  
  
  
  par <- find_the_best_pars(c(0.0001, 0.0001, .0001,.0001, .0001), 
                            c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE_2, 
                            dat, nm, nt)$parameter
  
  
  value <- rl_asym_auto_negLogLik_2(as.numeric(unlist(par)), data=dat) 
  print(value)
  mat_value[t, 6] <- value
  mat_par[n,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  n <- n+1  
}

mat_value[1:3,]

mat_par[1:9,]
mat_n[1:3,]

mat_bic <- matrix(data = 0, nrow = np, ncol =6)
par_num <- c(2,3,3,4,6,6)


for (i in 1:np){
  mat_bic[i,] <- mat_value[i,]*2 + log(mat_n[1])*par_num
  
}

x <- 1

mat_best <- matrix(0, nrow = 5, ncol = 2)

mat_best[,1] <- rep(1:6, 1)
  
rownames(mat_best) <- rep(c("model_1", "model_2", "model_4", 
                             "model_5", "model_6"), 1)

colnames(mat_bic) <- rep(c("model_1", "model_2", "model_3", "model_4", 
                           "model_5", "model_6"), 1)
mat_bic2 <- as.data.frame(mat_bic)

mat_bic2 <- mat_bic2 %>%
  select(-model_3)

mat_best <- matrix(0, nrow = 5, ncol = 1)

rownames(mat_best) <- rep(c("model_1", "model_2", "model_4", 
                            "model_5", "model_6"), 1)


mat_asym <- matrix(0, nrow = 2, ncol = 1)
rownames(mat_asym) <- rep(c("positive", "negative"), 1)



for (i in 1:np){
  x <- which.min(mat_bic2[i, ])
  mat_best[x,1] <- mat_best[x,1] + 1
  if (x == 2){
    p <- mat_par[i*6-4,2]
    n <- mat_par[i*6-4,3]
    if (p>n){
      mat_asym[1,1] <- mat_asym[1,1] + 1
    }
    if (n>p){
      mat_asym[2,1] <- mat_asym[2,1] + 1
    }
  }
}


mat_bic
mat_bic <- mat_value*(-2) + log(mat_n)*par_num

rownames(mat_par) <- rep(c("model_1", "model_2", "model_3", "model_4", 
                           "model_5", "model_6"), np)




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


mat_n

value <- rl_asym_auto_negLogLik(as.numeric(unlist(par5)), data=dat) 
print(value)
mat_value[5, t] <- value

