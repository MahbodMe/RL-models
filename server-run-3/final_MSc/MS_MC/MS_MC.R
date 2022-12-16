
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

source("estimation_models.R")


df <- read.csv("https://github.com/speekenbrink-lab/data/raw/master/Speekenbrink_Konstantinidis_2015.csv")

df$payoff <- df$payoff/10



rewards <- readRDS("MS_rewards")
# # TN NTN TS NTS = 1 2 3 4
# es <- rep(NA, 4)
# for (i3 in 1:4){
#   e <- 0
#   for (i2 in 1:4){
#     for (i in 1:4){
#       e <- var(rewards[,i,i2,i3]) + e
#     }
#   }
#   es[i3] <- e/16
# }
e <- 16/10

variance_limit <- e*10

rewards <- rewards[,,1,1]
rw <- rewards[,2:5]/10
lim_intial <- c(min(rw),max(rw))
lim_phi <- diff(lim_intial)
lim_phi <- c(-1*lim_phi, lim_phi)
lim_inv_temp <- lim_intial

np <- length(unique(df$id2))

nm <- 12
arm <- 4

mat_value <- rep(NA, nm)
mat_par <- matrix(NA, nm, 9)


mod_MC <- function(X){
  set.seed(123)
  dat <- df %>%
    filter(id2 == X)

  print(X)
  nt <- nrow(dat)
  
  # model 1
  est <- find_the_best_pars(c(.00001, .00001, lim_intial[1]), c(lim_intial[2], 1, lim_intial[2]), sym_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1] <- as.vector(unlist(est$value))
  mat_par[1,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 2
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial[1]), c(lim_intial[2], 1, 1,  lim_intial[2]), asym_rpe_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[2] <- as.vector(unlist(est$value))
  mat_par[2,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  # model 3
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial[1]), c(lim_intial[2], 1, 1,  lim_intial[2]), asym_reward_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[3] <- as.vector(unlist(est$value))
  mat_par[3,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 4
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001,lim_phi[1],  lim_intial[1]), c(lim_intial[2], 1, 1,lim_phi[2],  lim_intial[2]), sym_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[4] <- as.vector(unlist(est$value))
  mat_par[4,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 5
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial[1]), c(lim_intial[2], 1, 1, 1, lim_phi[2],  lim_intial[2]), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[5] <- as.vector(unlist(est$value))
  mat_par[5,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
  
  
  # model 6
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial[1]), c(lim_intial[2], 1, 1, 1, lim_phi[2],  lim_intial[2]), asym_reward_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[6] <- as.vector(unlist(est$value))
  mat_par[6,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
  
  
  # model 7
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001), c(lim_intial[2],  lim_intial[2], variance_limit, e, variance_limit, lim_intial[2]), ucb_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[7] <- as.vector(unlist(est$value))
  mat_par[7,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 8
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001), c(lim_intial[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_intial[2]), ucb_asym_rpe_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[8] <- as.vector(unlist(est$value))
  mat_par[8,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
  
  # model 9
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001), c(lim_intial[2],  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_intial[2]), ucb_asym_reward_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[9] <- as.vector(unlist(est$value))
  mat_par[9,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))              
  
  
  # model 10
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_intial[2],  lim_intial[2], variance_limit, e, variance_limit, lim_intial[2], 1, lim_phi[2]), ucb_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[10]<- as.vector(unlist(est$value))
  mat_par[10,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 11
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_intial[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_intial[2], 1, lim_phi[2]), ucb_asym_rpe_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[11] <- as.vector(unlist(est$value))
  mat_par[11,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 12
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_intial[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_intial[2], 1, lim_phi[2]), ucb_asym_reward_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[12] <- as.vector(unlist(est$value))
  mat_par[12,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  return(results = list(v = mat_value, p = mat_par))
}


ids <- c(1:20)
result <- mclapply(ids, mod_MC, mc.preschedule = FALSE, mc.cores = 40)


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



saveRDS(ngll_values, "MS_values.RDS")
saveRDS(par_ests,"MS_par_est.RDS")
