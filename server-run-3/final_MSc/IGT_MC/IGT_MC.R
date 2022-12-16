
### Required packges 
library(dplyr)
library(tidyr)
library(ggplot2)
library(nnet)
library(DEoptim)
library(abind)
library(parallel)


### Required functions
source("estimation_models.R")
source("packs_funs.R")



df <- readRDS("IGT_mat.RDS")
r1 <- readRDS("IGT_rewards//rewards_IGT_1.RDS")
r2 <- readRDS("IGT_rewards//rewards_IGT_2.RDS")
r2 <- r2[1:10,]
r3 <- readRDS("IGT_rewards//rewards_IGT_3.RDS")
df[,4] <- df[,4]/100
r1[,2:5] <- r1[,2:5]/100
r2[,2:5] <- r2[,2:5]/100
r3[,2:5] <- r3[,2:5]/100


np <- length(unique(df$ID))
nm <- 12
arm <- 4

es <- rep(0, 3)
for (i in 2:5){
  es <- es + c(var(r1[,i]), var(r2[,i]), var(r3[,i]))
}
es <- es/4





mat_value <- rep(NA, nm)
mat_par <- matrix(NA, nm, 9)



mod_MC <- function(X){
  set.seed(123)
  dat <- df %>%
    filter(ID == X)
  
  print(X)
  nt <- nrow(dat)
  
  
  if (dat$reward_scheme[1] == 1){e <- es[1]
  rw <- r1}
  if (dat$reward_scheme[1] == 2){e <- es[2]
  rw <- r2}
  if (dat$reward_scheme[1] == 3){e <- es[3]
  rw <- r3}

  lim_intial <- c(min(rw[,2:5]),max(rw[,2:5]))
  lim_phi <- diff(lim_intial)*10
  lim_phi <- c(-1*lim_phi, lim_phi)
  lim_intial[2] <- lim_intial[1]*(-1)
  lim_inv_temp <- lim_intial
  # lim_intial <- c(0, 0.000000001)
  variance_limit <- e*10
  

  
  # model 1
  est <- find_the_best_pars(c(.00001, .00001, lim_intial[1]), c(lim_inv_temp[2], 1, lim_intial[2]), sym_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1] <- as.vector(unlist(est$value))
  mat_par[1,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 2
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial[1]), c(lim_inv_temp[2], 1, 1,  lim_intial[2]), asym_rpe_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[2] <- as.vector(unlist(est$value))
  mat_par[2,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  # model 3
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial[1]), c(lim_inv_temp[2], 1, 1,  lim_intial[2]), asym_reward_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[3] <- as.vector(unlist(est$value))
  mat_par[3,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 4
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1,lim_phi[2],  lim_intial[2]), sym_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[4] <- as.vector(unlist(est$value))
  mat_par[4,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 5
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1, 1, lim_phi[2],  lim_intial[2]), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[5] <- as.vector(unlist(est$value))
  mat_par[5,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
  
  
  # model 6
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1, 1, lim_phi[2],  lim_intial[2]), asym_reward_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[6] <- as.vector(unlist(est$value))
  mat_par[6,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
  
  
  # model 7
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001), c(lim_inv_temp[2],  lim_intial[2], variance_limit, e, variance_limit, lim_inv_temp[2]), ucb_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[7] <- as.vector(unlist(est$value))
  mat_par[7,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 8
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001), c(lim_inv_temp[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2]), ucb_asym_rpe_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[8] <- as.vector(unlist(est$value))
  mat_par[8,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
  
  # model 9
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001), c(lim_inv_temp[2],  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2]), ucb_asym_reward_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[9] <- as.vector(unlist(est$value))
  mat_par[9,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))              
  
  
  # model 10
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_inv_temp[2],  lim_intial[2], variance_limit, e, variance_limit, lim_inv_temp[2], 1, lim_phi[2]), ucb_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[10]<- as.vector(unlist(est$value))
  mat_par[10,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 11
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_inv_temp[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2], 1, lim_phi[2]), ucb_asym_rpe_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[11] <- as.vector(unlist(est$value))
  mat_par[11,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 12
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_inv_temp[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2], 1, lim_phi[2]), ucb_asym_reward_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[12] <- as.vector(unlist(est$value))
  mat_par[12,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  return(results = list(v = mat_value, p = mat_par))
}


ids <- sort(unique(df$ID))
result <- mclapply(ids, mod_MC, mc.preschedule = FALSE, mc.cores = 21)


ngll_values <- matrix(NA, np, nm)
colnames(ngll_values) <- c("model_1", "model_2", "model_3", "model_4",
                           "model_5", "model_6", "model_7", "model_8",
                           "model_9", "model_10", "model_11", "model_12")


for (i in 1:length(ids)){
  ngll_values[i,] <- array(unlist(result[[i]]$v), dim = c(1, nm))
}




par_ests <- array(NA, c(nm, 9, np))
for (i in 1:length(ids)){
  par_ests[,,i] <- array(unlist(result[[i]]$p), dim = c(nm, 9))
}



saveRDS(ngll_values, "IGT_values.RDS")
saveRDS(par_ests,"IGT_par_est.RDS")

