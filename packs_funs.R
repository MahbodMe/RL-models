

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


# Example:
rw <- reward_generator(3, 200, 4, 16, 16, c(-4, 4, -12, 12))

### the following function plots rewards 

plot_rewards <- function(data){
  return(
    data %>%
      gather(key=option,value=data,-trial) %>%
      ggplot(aes(x=trial,y=data,colour=option)) + geom_line()
  )
}


# Example:



plot_rewards(rw)



# Generating fake data

# the following function takes an environment (data), a list of models (funs)  
# and a list of parameters for those models (pars); and generates a data set
# for each of the models 


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

df_sim <- sim_behav_data_csv(rw, c("ucb_sim"), list(c(0, 1000, 16,16, 1.96)), 4, 200)


sim_behav_data_3d <- function(data, funs, pars, no, nt, envir = parent.frame()){
  df <- array(0, c(nt,4,length(funs)))
  for (i in 1:length(funs)){
    fun <- funs[i]
    print(pars[1])
    par <- as.numeric(unlist(pars[i]))
    print(par)
    sim <- do.call(fun, c(list(data), par[1:length(par)]))
    df[ ,1 , i] <- rep(1:nt, 1)
    df[ ,2 ,i] <- rep(i, nt)
    df[ ,3 ,i]<- head(sim$choices, nt)
    df[ ,4 ,i] <- head(sim$agents_rewards, nt)
  }
  colnames(df) <- c("trial", "id", "deck", "payoff")
  
  return(data_sim = df)
}


# example:

df_sim <- sim_behav_data_3d(rewards, c("sym_sim"), list(c(1, .5)), 4, 80)





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

# example:

est <- find_the_best_pars(c(.0001, .0001), c(100, 1), sym_negLogLik_DE, df_sim,
                          1, 80)

sym_negLogLik(as.numeric(unlist(est$parameter)), df_sim)




### reward output average for 100 attempts for each models 

mat_sim <- matrix(0, nrow = 12, ncol =1)
rownames(mat_sim) <- c("agent_1","agent_2","agent_3","agent_4",
                       "agent_5","agent_6","agent_7","agent_8",
                       "agent_9", "agent_10", "agent_11", "agent_12")


for (n in 1:100){
  sim <- c(sym_sim(rewards, .5, .8)$agents_total_rewards,
           asym_rpe_sim(rewards, .6, .2, .8)$agents_total_rewards,
           asym_reward_sim(rewards, .6, .2, .8)$agents_total_rewards,
           sym_auto_sim(rewards, .5, .8, .02, 1)$agents_total_rewards,
           asym_rpe_auto_sim(rewards, .6, .2, .8, .02, 1)$agents_total_rewards,
           asym_reward_auto_sim(rewards, .6, .2, .8, .02, 1)$agents_total_rewards,
           ucb_sim(rewards , 0.0, 1000, 16, 16, 1.96)$agents_total_rewards,
           ucb_asym_rpe_sim(rewards, 0, 1000, 16, 10, 22, 1.96)$agents_total_rewards,
           ucb_asym_reward_sim(rewards, 0, 1000, 16, 10, 22, 1.96)$agents_total_rewards,
           ucb_auto_sim(rewards , 0.0, 1000, 16, 16, 1.96, 0.5, .8)$agents_total_rewards,
           ucb_asym_rpe_auto_sim(rewards, 0, 1000, 16, 12, 22, 1.96, .8, 1)$agents_total_rewards,
           ucb_asym_reward_auto_sim(rewards, 0, 1000, 16, 12, 22, 1.96, .8, 1)$agents_total_rewards)
  
  print(n)
  mat_sim[,1] <- c(mat_sim[,1] + sim)
}

mat_sim[,1] <- mat_sim[,1]/100

mat_sim





### sample a value from a dist.

sample_beta_dist <- function(l_value, n_sample){
  mu1 <- mean(l_value)
  v1 <- sd(l_value)**2
  a <- (((1-mu1)/v1)-(1/mu1))*(mu1**2)
  b <- a*((1/mu1)-1)
  r_v <- rbeta(n_sample, a, b)
  return(r_v)
}

sample_log_dist <- function(l_value, n_sample){
  l_value <- log(l_value)
  mu1 <- mean(l_value)
  s1 <- sd(l_value)
  r_v <- exp(rnorm(n_sample, mu1, s1))
  return(r_v)
}


sample_norm_dist <- function(l_value, n_sample){
  mu1 <- mean(l_value)
  s1 <- sd(l_value)
  r_v <- rnorm(n_sample, mu1, s1)
  return(r_v)
}



