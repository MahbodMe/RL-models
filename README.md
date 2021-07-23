# MSc Thesis project

### 12 RL models

| Models | Learning Rate | Autocorrelation | Decision Rule | Model name in code | Number of Free Parameters | 
|---------------|-----------|-----------|-----------|-----------|-----------|
|Model 1| Q_learning (Symmetric) | NA | SoftMax | sym_sim()| 2 |
|Model 2| Q_learning (Asymmetric-RPE) | NA | SoftMax | asym_rpe_sim()| 3 |
|Model 3| Q_learning (Asymmetric-Reward) | NA | SoftMax | asym_reward_sim()| 3 |
|Model 4| Q_learning (Symmetric) | Choice Trace | SoftMax | sym_auto_sim()| 4 |
|Model 5| Q_learning (Asymmetric-Reward) | Choice Trace | SoftMax | asym_rpe_auto_sim()| 5 |
|Model 6| Q_learning (Asymmetric-Reward) |Choice TraceNA | SoftMax | asym_reward_auto_sim()| 5 |
|Model 7| Kalman filter (Symmetric) | NA | UCB + Exploration Bonus | ucb_sim()| 5 |
|Model 8| Kalman filter (Asymmetric-RPE) | NA | UCB + Exploration Bonus | ucb_asym_rpe_sim()| 6 |
|Model 9| Kalman filter (Asymmetric-Reward) | NA | UCB + Exploration Bonus | ucb_asym_reward_sim()| 6 |
|Model 10| Kalman filter (Symmetric) | Choice Trace | UCB + Exploration Bonus | ucb_sim()| 7 |
|Model 11| Kalman filter (Asymmetric-RPE) | Choice Trace | UCB + Exploration Bonus | ucb_asym_rpe_sim()| 8 |
|Model 12| Kalman filter (Asymmetric-Reward) | Choice Trace | UCB + Exploration Bonus | ucb_asym_reward_sim()| 8 |


* Agent simulation functions for the 6 Q_learning models (Model 1 to Model 6) are in Q_learning_models.R
* Agent simulation functions for the 6 Kalman filter models (Model 7 to Model 12) are in Kf_UCB_models.R

#### Parameter estimation is conducted using the DEoptim package 
* Parameter estimation functions for the 6 Q_learning models (Model 1 to Model 6) are in par_est_Q_learning.R
* Parameter estimation functions for the 6 Kalman filter models (Model 7 to Model 12) are in par_est_UCB.R


\* For the Kalman filter estimation parameters, the innovation_term value is fixed to the environment's true value *


### Rewards are generated according to the following function
``` 
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
``` 

### Generating fake data
Behavioural data is simulated using the following function which takes an environment (data), a list of model's function in string form (funs), a list of parameters for each of the functions in vector form (pars), number of arms (no), and number of trials (nt). 
``` 
# CSV form
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

# Multi-dim array form 

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

# example 
df_sim <- sim_behav_data_csv(rewards, c("ucb_sim"), list(c(0, 1000, 16,16, 1.96)), 8, 80)
```
The output:
* trial: trial number
* id: the model used with respect to the order they are passed
* deck: the chosen arm
* payoff: reward recived from the chosen deck at that trial


### Parameter estimation function
The following function takes a list of upper and lower limits (lpar and upar), negloglikDE function (fun), behavioural data (df), number of arms (no), and number of trials (nt).
```
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

est <- find_the_best_pars(c(.0001, .0001), c(100, 1), sym_negLogLik_DE, 
                          df_sim, 1, 80)
```

### Sampling emprical priors from normal/beta distribution 

```
# 0 to 1
sample_beta_dist <- function(l_value, n_sample){
  mu1 <- mean(l_value)
  v1 <- sd(l_value)**2
  a <- (((1-mu1)/v1)-(1/mu1))*(mu1**2)
  b <- a*((1/mu1)-1)
  r_v <- rbeta(n_sample, a, b)
  return(r_v)
}

# 0 to +inf
sample_log_dist <- function(l_value, n_sample){
  l_value <- log(l_value)
  mu1 <- mean(l_value)
  s1 <- sd(l_value)
  r_v <- exp(rnorm(n_sample, mu1, s1))
  return(r_v)
}

# -inf to +inf 
sample_norm_dist <- function(l_value, n_sample){
  mu1 <- mean(l_value)
  s1 <- sd(l_value)
  r_v <- rnorm(n_sample, mu1, s1)
  return(r_v)
}
```
### Server runs

* est_Speekenbrink_Konstantinidis_2015.R: parameter estimation and negloglik value for all four conditions using 5 different seeds
* est_S&K_best_fitted.R: model recovery using best fitted parameters from S&K (2015) estimates as emprical priors
* est_S&K_emp_dist.R:  model recovery sampling from distributions from S&K (2015) estimated parameters as emprical priors
