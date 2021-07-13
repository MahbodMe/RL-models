

library(dplyr)
library(tidyr)
library(ggplot2)
library(nnet)
library(DEoptim)










### Parameter estimation simple auto + asymmetric Q-learning (reward magnitude)



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
  qs <- rl_asymmetric_2(choice, reward, arm, 0, p_alpha, n_alpha)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(arm:nrow(data),choice[arm:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}




rl_asym_negLogLik_DE_2 <- function(par,data,x) {
  negLL <- rl_asym_negLogLik_2(par,data)
  
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
  phi <- par[4]
  tau <- par[5]
  
  choice <- data$deck
  reward <- data$payoff
  ct <- rl_asymmetric_auto_2(choice, reward,  arm, 0, p_alpha, n_alpha, tau)$choice_trace*phi
  
  qs <- rl_asymmetric_auto_2(choice, reward,  arm, 0, p_alpha, n_alpha, tau)$q_value
  probs <- softmax_simple_choice_prob_auto(qs, beta, ct)
  prob_choice <- probs[cbind( arm:nrow(data),choice[ arm:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}



rl_asym_auto_negLogLik_DE_2 <- function(par,data,x) {
  negLL <- rl_asym_auto_negLogLik_2(par,data)
  
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
  qs <- rl_symmetric(choice, reward, arm, 0, alpha)$q_value
  probs <- softmax_simple_choice_prob(qs, beta)
  prob_choice <- probs[cbind(arm:nrow(data),choice[arm:nrow(data)])]
  negLogLik <- sum(log(prob_choice))*-1
  return(negLogLik)
}




rl_sym_negLogLik_DE <- function(par,data) {
  negLL <- rl_sym_negLogLik(par,data)
  
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


df <- read.csv("https://github.com/speekenbrink-lab/data/raw/master/Speekenbrink_Konstantinidis_2015.csv")


np <- length(unique(df$id2))



seeds <- sample(1:1000, 5)
mat_n <- matrix(data = 0, nrow = np, ncol = 1)
mat_value <- array(data = NA, c(np, 8, length(seeds)))
mat_par <- array(NA, c(8,8,np,length(seeds)))
n <- 1
arm <- 4
nm <- 1
it <- 16



for (s in 1:length(seeds)){
  set.seed(seeds[s])
  for (t in 1:1){
    
    dat <- df %>%
      filter(id2==t)
    
    nt <- length(dat$deck)
    mat_n[t, 1] <- nt
    
    
    print(t)
    # model 1
    par <- find_the_best_pars(c(.0001, .0001), c(100, 1), rl_sym_negLogLik_DE, dat, nm, nt)$parameter
    mat_value[t, 1, s] <- rl_sym_negLogLik(as.numeric(unlist(par)), data=dat) 
    mat_par[1,1:length(as.vector(unlist(par))),t,s] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    #model 2
    par <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), rl_asym_negLogLik_DE, dat, nm, nt)$parameter
    mat_value[t, 2, s] <- rl_asym_negLogLik(as.numeric(unlist(par)), data=dat) 
    mat_par[2,1:length(as.vector(unlist(par))),t,s] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
    # model 3
    par <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), rl_asym_negLogLik_DE_2, dat, nm, nt)$parameter
    mat_value[t, 3,s] <- rl_asym_negLogLik_2(as.numeric(unlist(par)), data=dat) 
    mat_par[3,1:length(as.vector(unlist(par))),t,s] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
    
    # model 4
    par <- find_the_best_pars(c(0.0001, 0.0001, .0001, .0001), c(100, 1, 100 ,1), rl_sym_auto_negLogLik_DE, dat, nm, nt)$parameter
    mat_value[t, 4,s] <- rl_sym_auto_negLogLik(as.numeric(unlist(par)), data=dat) 
    mat_par[4,1:length(as.vector(unlist(par))),t,s] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
    # model 5
    par <- find_the_best_pars(c(0.0001, 0.0001, .0001,.0001, .0001), 
                              c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE, 
                              dat, nm, nt)$parameter
    mat_value[t, 5,s] <- rl_asym_auto_negLogLik(as.numeric(unlist(par)), data=dat) 
    mat_par[5,1:length(as.vector(unlist(par))),t,s] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 6
    par <- find_the_best_pars(c(0.0001, 0.0001, .0001,.0001, .0001), 
                              c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE_2, 
                              dat, nm, nt)$parameter
    mat_value[t, 6,s] <- rl_asym_auto_negLogLik_2(as.numeric(unlist(par)), data=dat) 
    mat_par[6,1:length(as.vector(unlist(par))),t,s] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
   
    
    
    # model 7
    par <- find_the_best_pars(c(0,-100,0.001,0.0001, 0.001),
                               c(10,100,1000,100, 50), kf_sm_negLogLik_DE, 
                               dat, nm, nt)$parameter
    mat_value[t, 7,s] <- rl_ucb_negLogLik_2(as.numeric(unlist(par)), data=dat) 
    mat_par[7,1:length(as.vector(unlist(par))),t,s] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 8
    par <- find_the_best_pars(c(0,-100,0.00001,0.00001, 0.0001, .1, .1),
                               c(10,100,1000,100, 50, 1, 100), kf_sm_auto_negLogLik_DE, 
                               dat, nm, nt)$parameter
    mat_value[t, 8,s] <- rl_ucb_auto_negLogLik(as.numeric(unlist(par)), data=dat) 
    mat_par[8,1:length(as.vector(unlist(par))),t,s] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  }
}



saveRDS(mat_value, "mat_value.rds")
saveRDS(mat_par, "mat_par.rds")
saveRDS(mat_n, "scores.rds")





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




}


