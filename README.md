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
Behavioural data is simulated using the following function which takes an environment (data), a list of model's function in string form (funs), a list of parameters for each of the functions in vector form (pars), number of arms (NO), number of trials (NT). 
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
```
The outputs:
* trial: trial number
* id: the model used with respect to the order they are passed
* deck: the chosen arm
* payoff: reward recived from the chosen deck at that trial



