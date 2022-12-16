
### Required packges 
library(dplyr)
library(tidyr)
library(ggplot2)
library(nnet)
library(DEoptim)
library(abind)
library(parallel)


### Required functions
source("packs_funs.R")


df <- readRDS("Lefebvre_dataset.RDS")


df1 <- df[,,1]
df2 <- df[,,2]
df1 <- as.data.frame(df1)
df2 <- as.data.frame(df2)

df1$payoff <- df1$payoff*20
df2$payoff <- df2$payoff*20



# 
# es <- rep(NA, 4)
# e <- reward_generator_binary(2, 48, c(10,-10), c(.5, .5))
# es[1] <- (var(e[,2]) + var(e[,3]))/2
# e <- reward_generator_binary(2, 48, c(10,-10), c(.25, .75))
# es[2:3] <- (var(e[,2]) + var(e[,3]))/2
# e <- reward_generator_binary(2, 48, c(10,-10), c(.17, .83))
# es[4] <- (var(e[,2]) + var(e[,3]))/2
# 
# e <- sum(es)/4
# variance_limit <- e*10




lim_intial <- c(-10,10)
lim_phi <- diff(lim_intial)*10
lim_phi <- c(-1*lim_phi, lim_phi)
lim_inv_temp <- lim_intial
lim_intial_m0 <- c(0, 0.000000001)
lim_tau_imp <- c(0.999999, 1)


source("estimation_SK_models.R")


df <- df1
np <- length(unique(df$ID))
nc <- 1
nm <- 18
arm <- 2




mat_n <- rep(NA, nc)
mat_value <- array(NA, c(1, nm))

mat_par <- array(NA, c(nm, 9))





mod_MC <- function(X){
  set.seed(123)
  dat <- df %>%
    filter(ID == X)
  
  print(X)
  
  nt <- nrow(dat)
  mat_n <- nt
  
  
  
  # model 1 SQ
  est <- find_the_best_pars(c(.00001, .00001, lim_intial[1]), c(lim_inv_temp[2], 1, lim_intial[2]), sym_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 1] <- as.vector(unlist(est$value))
  mat_par[1,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 2 SQ_M0
  est <- find_the_best_pars(c(0.00001, .00001, lim_intial_m0[1]), c(lim_inv_temp[2], 1, lim_intial_m0[2]), sym_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 2] <- as.vector(unlist(est$value))
  mat_par[2,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  lim_intial_m0
  
  # model 3 ApSQ
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial[1]), c(lim_inv_temp[2], 1, 1,  lim_intial[2]), asym_rpe_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 3] <- as.vector(unlist(est$value))
  mat_par[3,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  # model 4 ApSQ_m0
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial_m0[1]), c(lim_inv_temp[2], 1, 1,  lim_intial_m0[2]), asym_rpe_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 4] <- as.vector(unlist(est$value))
  mat_par[4,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 5 ArSQ
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial[1]), c(lim_inv_temp[2], 1, 1,  lim_intial[2]), asym_reward_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 5] <- as.vector(unlist(est$value))
  mat_par[5,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  # model 6 ArSQ_m0
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial_m0[1]), c(lim_inv_temp[2], 1, 1,  lim_intial_m0[2]), asym_reward_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 6] <- as.vector(unlist(est$value))
  mat_par[6,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 7 SQwA
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1,lim_phi[2],  lim_intial[2]), sym_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 7] <- as.vector(unlist(est$value))
  mat_par[7,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 8 SQwA_m0
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, lim_phi[1],  lim_intial_m0[1]), c(lim_inv_temp[2], 1, 1,lim_phi[2],  lim_intial_m0[2]), sym_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 8] <- as.vector(unlist(est$value))
  mat_par[8,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  # model 9 SQwA_imp
  est <- find_the_best_pars(c(0.00001,0.00001,lim_tau_imp[1],lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, lim_tau_imp[2],lim_phi[2],  lim_intial[2]), sym_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 9] <- as.vector(unlist(est$value))
  mat_par[9,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 10 SQwA_m0_imp
  est <- find_the_best_pars(c(0.00001,0.00001,lim_tau_imp[1], lim_phi[1],  lim_intial_m0[1]), c(lim_inv_temp[2], 1, lim_tau_imp[2],lim_phi[2],  lim_intial_m0[2]), sym_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 10] <- as.vector(unlist(est$value))
  mat_par[10,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 11 ApSQwA
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1, 1, lim_phi[2],  lim_intial[2]), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 11] <- as.vector(unlist(est$value))
  mat_par[11,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
  
  # model 12 ApSQwA_m0
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial_m0[1]), c(lim_inv_temp[2], 1, 1, 1, lim_phi[2],  lim_intial_m0[2]), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 12] <- as.vector(unlist(est$value))
  mat_par[12,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
  
  
  # model 13 ApSQwA_imp
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, lim_tau_imp[1], lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1, lim_tau_imp[2], lim_phi[2],  lim_intial[2]), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 13] <- as.vector(unlist(est$value))
  mat_par[13,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
  
  # model 14 ApSQwA_m0_imp
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, lim_tau_imp[1], lim_phi[1],  lim_intial_m0[1]), c(lim_inv_temp[2], 1, 1, lim_tau_imp[2], lim_phi[2],  lim_intial_m0[2]), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 14] <- as.vector(unlist(est$value))
  mat_par[14,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
  
  # model 15 ArSQwA
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1, 1, lim_phi[2],  lim_intial[2]), asym_reward_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 15] <- as.vector(unlist(est$value))
  mat_par[15,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))])) 
  
  # model 16 ArSQwA_m0
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial_m0[1]), c(lim_inv_temp[2], 1, 1, 1, lim_phi[2],  lim_intial_m0[2]), asym_reward_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 16] <- as.vector(unlist(est$value))
  mat_par[16, 1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
  
  # model 17 ArSQwA_imp
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, lim_tau_imp[1], lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1, lim_tau_imp[2], lim_phi[2],  lim_intial[2]), asym_reward_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 17] <- as.vector(unlist(est$value))
  mat_par[17,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))])) 
  
  # model 18 ArSQwA_m0_imp
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, lim_tau_imp[1], lim_phi[1],  lim_intial_m0[1]), c(lim_inv_temp[2], 1, 1, lim_tau_imp[2], lim_phi[2],  lim_intial_m0[2]), asym_reward_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1, 18] <- as.vector(unlist(est$value))
  mat_par[18,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
  
  
  return(results = list(n = mat_n, v = mat_value, p = mat_par))
}



ids <- sort(unique(df$ID))
result <- mclapply(ids, mod_MC, mc.preschedule = FALSE, mc.cores = 22)


num_obs <- matrix(NA, np, nm)
for (i in 1:length(ids)){
  num_obs[i,] <- as.vector(unlist(result[[i]]$n))
}

ngll_values <- matrix(NA, np, nm)
for (i in 1:length(ids)){
  ngll_values[i,] <- array(unlist(result[[i]]$v), dim = c(1, nm))
}


par_ests <- array(NA, c(nm, 9, np))
for (i in 1:length(ids)){
  par_ests[,,i] <- array(unlist(result[[i]]$p), dim = c(nm, 9))
}



saveRDS(ngll_values, "Lefebvre_exp1_2017_ngll_values.RDS")
saveRDS(par_ests,"Lefebvre_exp1_2017_par_est.RDS")
saveRDS(num_obs, "Lefebvre_exp1_2017_num_obs.RDS")



