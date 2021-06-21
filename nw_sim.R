

rl_ucb <- function(choice, reward, n_options, initial_mean, initial_variance,
                   innovation_term, error_term, beta) {
  
  n_trials <- length(choice) # Number of Trials 
  n_options <- n_options # Number of Options
  arm_mean <- matrix(initial_mean, ncol = n_options, nrow = n_trials + 1) 
  # Each arms variance of the posterior distribution of the mean utility 
  arm_variance <- matrix(initial_variance, ncol = n_options, 
                         nrow = n_trials + 1)
  
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
  }
  return(list(arm_mean = arm_mean, arm_variance = arm_variance))
}



test <- rl_ucb(df_sim_1$deck, df_sim_1$payoff,4, 0, 1000, 16, 16, 1)
test


softmax_uncertain_choice_prob <- function(arm_mean, ppsd, gamma, beta) {
  prob <- exp(gamma*(arm_mean + ppsd*beta))
  prob <- prob/rowSums(prob)
  return (prob = prob)
}

softmax_uncertain_choice_prob(arm_mean = test$arm_mean, 2, 2,2)


rl_ucb_negLogLik <- function(par, data){
  
  gamma <- par[1]
  initial_mean <- par[2]
  initial_variance <- par[3]
  error_term <- par[4]
  beta <- par[5] 
  innovation_term <- 16
  choice <- data$deck
  reward <- data$payoff
  
  kal <- rl_ucb(choice, reward, 4, initial_mean, initial_variance,
                innovation_term, error_term, beta)
  
  arm_mean <- kal$arm_mean
  
  ppsd <- sqrt(kal$arm_variance + innovation_term)

  prob <- softmax_uncertain_choice_prob(arm_mean, ppsd, gamma, beta)
  
  lik <- prob[cbind(1:nrow(data), choice)]
  negLogLik <- -sum(log(lik))
  if(is.na(negLogLik) | negLogLik == Inf) negLogLik <- 1e+300
  return(negLogLik)
}


gamma <- seq(0,1, length=300)
out <- rep(NA,length=length(gamma))
for (i in 1:length(gamma)) out[i] <- rl_sym_logLik(c(gamma[i], 0, 1000, 16, 1.96), df_sim_2)
plot(gamma,out,ylab="Neg-Log-likelihood",xlab=expression(gamma))


est <- optim(c(.5, 0, 1000, 16, 1.96),rl_ucb_negLogLik,data=df_sim_2)


est <- optim(par=c(1, 0, 1000, 16, 16),fn=rl_ucb_negLogLik,lower=c(0,-Inf,0,0,0),data=df_sim_2)

dat <- read.csv("https://github.com/speekenbrink-lab/data/raw/master/Speekenbrink_Konstantinidis_2015.csv")
tdat <- subset(dat,id2==4)

est <- optim(c(1, 0, 1000, 16, 1.96),rl_ucb_negLogLik,data=tdat)
