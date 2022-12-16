

### Required packges 
library(dplyr)
library(tidyr)
library(ggplot2)
library(nnet)
library(DEoptim)
library(abind)
library(parallel)


### Required functions
source("estimation_SK_models.R")
source("packs_funs.R")
source("simulation_models.R")



reward_generator_binary <- function(seed, nt, rewards_best_first, probs_of_winning){
  no <- 2
  set.seed(seed) # set the random seed to replicate these results exactly
  mu <- matrix(0.0,nrow=nt,ncol=no) # to store the time-varying means
  for(t in 1:nt) {
    for(i in 1:no) {
      p <- probs_of_winning[i]
      p <- c(p, 1-p)
      mu[t,i] <- sample(rewards_best_first, size = 1, prob = p)
    }
  }
  rewards <- as.data.frame(cbind(1:nt,mu))
  colnames(rewards) <- c("trial",paste0("A",1:no))
  
  return(rewards)
}

df1 <- reward_generator_binary(123, 960, c(10,-10), c(.5,.5))
df2 <- reward_generator_binary(123, 960, c(10,-10), c(.25,.75))
df3 <- reward_generator_binary(123, 960, c(10,-10), c(.75,.25))
df4 <- reward_generator_binary(123, 480, c(10,-10), c(.13,.85))
df5 <- reward_generator_binary(123, 480, c(10,-10), c(.85,.13))
df4 <- rbind(df4,df5)


sim_behav_data_csv_2 <- function(funs, pars, no, nt, envir = parent.frame()){
  mat <- matrix(0, nrow = length(funs)*nt, ncol = 4)
  for (i in 1:length(funs)){
    fun <- funs[i]
    par <- as.numeric(unlist(pars[i]))
    sim1 <- do.call(fun, c(list(df1), list(par)))
    sim2 <- do.call(fun, c(list(df2), list(par)))
    sim3 <- do.call(fun, c(list(df3), list(par)))
    sim4 <- do.call(fun, c(list(df4), list(par)))
    sim <- rbind(sim1,sim2,sim3,sim4)

    x <- (i-1)*nt + 1
    y <- i*nt
    mat[x:y ,1] <- rep(i, nt)
    mat[x:y ,2] <- c(head(sim1$choices, 960), head(sim2$choices, 960),
                     head(sim3$choices, 960), head(sim4$choices, 960))
    mat[x:y ,3] <- c(head(sim1$agents_rewards, 960), head(sim2$agents_rewards, 960),
                     head(sim3$agents_rewards, 960), head(sim4$agents_rewards, 960))
    mat[x:y ,4] <- c(rep(1, 960), rep(2, 960),
                     rep(3, 960), rep(4, 960))
  }
  mats <- as.data.frame(cbind(1:nt, mat))
  colnames(mats) <- c("trial", "id", "deck", "payoff", "type")
  return(data_sim = mats)
}

 dats <-  sim_behav_data_csv_2(rep("asym_rpe_auto_sim", 18),
                              list(c(.3, .4, .4, -10, 0, 0), c(.3, .4, .4, 0, 0, 0), 
                                   c(.3, .5, .2, -10, 0, 0), c(.3, .5, .2, 0, 0, 0),
                                   c(.3, .2, .5, -10, 0, 0), c(.3, .2, .5, 0, 0, 0),
                                   c(.3, .4, .4, -10, .4, 1.5), c(.3, .4, .4, 0, .4, 1.5),
                                   c(.3, .5, .2, -10, .4, 1.5), c(.3, .5, .2, 0, .4, 1.5),
                                   c(.3, .2, .5, -10, .4, 1.5), c(.3, .2, .5, 0, .4, 1.5),
                                   c(.3, .4, .4, -10, 1, 1.5), c(.3, .4, .4, 0, 1, 1.5),
                                   c(.3, .5, .2, -10, 1, 1.5), c(.3, .5, .2, 0, 1, 1.5),
                                   c(.3, .2, .5, -10, 1, 1.5), c(.3, .2, .5, 0, 1, 1.5)),
                              2, 3840)
 

lim_intial <- c(-10,10)
lim_phi <- diff(lim_intial)
lim_phi <- c(-1*lim_phi, lim_phi)
lim_inv_temp <- lim_intial
lim_intial_m0 <- c(0, 0.000000001)
lim_tau_imp <- c(0.999999, 1)

arm <- 2
nt <- 3840
nm <- 18

mat_mr <- rep(NA,nm)


mat_par <- matrix(NA, 18, 6)
seeds <- sample(1:1000, 200)


mod_id <- function(X){
  print(X)
  set.seed(seeds[X])

  # model 18 ArSQwA_m0_imp
  
  dat <- dats %>%
    filter(id == X)
 
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, -10,  -10), c(10, 1, 1, 1, 10, 10), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
 
  pars <- est$parameter
 
  ngll <- est$value
 
  mat_mr <- as.vector(unlist(ngll))
  par <- pars
  mat_par <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  return(results = list(mv = mat_mr, pv = mat_par))
}

result <- mclapply(c(1:18), mod_id, mc.preschedule = FALSE, mc.cores = 18)




ngll_values <- matrix(NA, 1, nm)
for (i in 1:nm){
  ngll_values[1,i] <- as.vector(unlist(result[[i]]$mv))
}


par_ests <- array(NA, c(nm, 6))
for (i in 1:nm){
  par_ests[i,] <- as.vector(unlist(result[[i]]$p))
}



saveRDS(ngll_values, "mat_ngll.RDS")
saveRDS(par_ests, "mat_pal.RDS")
saveRDS(result, "result.RDS")







