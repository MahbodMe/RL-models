
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
df[,4] <- df[,4]/100
initial_variance <- sd(df$payoff)*100


np <- length(unique(df$ID))
nm <- 12
arm <- 4

mat_value <- rep(NA, 12)


mat_par <- matrix(NA, nm, 8)


mod_MC <- function(X){
  set.seed(123)
  dat <- df %>%
    filter(ID == X)
  
  print(X)
  nt <- nrow(dat)
  
  # model 1
  est <- find_the_best_pars(c(.0001, .0001, -10000), c(200, 1, 10000), sym_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[1] <- as.vector(unlist(est$value))
  mat_par[1,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 2
  est <- find_the_best_pars(c(0.0001,0.0001,0.0001, -10000), c(200, 1, 1, 10000), asym_rpe_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[2] <- as.vector(unlist(est$value))
  mat_par[2,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  # model 3
  est <- find_the_best_pars(c(0.0001,0.0001,0.0001, -10000), c(200, 1, 1, 10000), asym_reward_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[3]<- as.vector(unlist(est$value))
  mat_par[3,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 4
  est <- find_the_best_pars(c(0.0001,0.0001,0.0001,-10000, -10000), c(200, 1, 1,10000, 10000), sym_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[4] <- as.vector(unlist(est$value))
  mat_par[4,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 5
  est <- find_the_best_pars(c(0.0001,0.0001,0.0001, 0.0001, -10000, -1000), c(200, 1, 1, 1, 10000, 10000), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[5] <- as.vector(unlist(est$value))
  mat_par[5,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
  
  
  # model 6
  est <- find_the_best_pars(c(0.0001,0.0001,0.0001, 0.0001, -100, -10000), c(200, 1, 1, 1, 10000, 10000), asym_reward_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[6] <- as.vector(unlist(est$value))
  mat_par[6,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
  
  
  # model 7
  est <- find_the_best_pars(c(0.0001, -10000, 0.00001, 0.0001, 0.0001), c(200, 10000, 100000, 100000, 100), ucb_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[7] <- as.vector(unlist(est$value))
  mat_par[7,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 8
  est <- find_the_best_pars(c(0.0001, -10000, 0.00001, 0.0001, 0.0001, 0.0001), c(200, 10000, 100000, 100000, 100000, 100), ucb_asym_rpe_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[8] <- as.vector(unlist(est$value))
  mat_par[8,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
  
  # model 9
  est <- find_the_best_pars(c(0.0001, -10000, 0.00001, 0.0001, 0.0001, 0.0001), c(200, 10000, 100000, 100000, 100000, 100), ucb_asym_reward_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[9] <- as.vector(unlist(est$value))
  mat_par[9,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))              
  
  
  # model 10
  est <- find_the_best_pars(c(0.0001 , -10000, 0.00001, 0.00001, 0.0001, 0.0001, -10000), c(200, 10000, 100000, 100000, 100, 1, 10000), ucb_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[10]<- as.vector(unlist(est$value))
  mat_par[10,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 11
  est <- find_the_best_pars(c(0.0001 , -10000, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -10000), c(200, 10000, 100000, 100000, 100000, 100, 1, 10000), ucb_asym_rpe_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[11] <- as.vector(unlist(est$value))
  mat_par[11,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  
  # model 12
  est <- find_the_best_pars(c(0.0001, -10000, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -10000), c(200, 10000, 100000, 100000, 100000, 100, 1, 10000), ucb_asym_reward_auto_negLogLik_DE, dat, 1, nt)
  par <- est$parameter
  mat_value[12] <- as.vector(unlist(est$value))
  mat_par[12,1:length(as.vector(unlist(par)))] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
  
  return(results = list(v = mat_value, p = mat_par))
}


ids <- sort(unique(df$ID))
length(ids)/42
result <- mclapply(ids, mod_MC, mc.preschedule = FALSE, mc.cores = 42)


ngll_values <- matrix(NA, np, nm)
colnames(ngll_values) <- c("model_1", "model_2", "model_3", "model_4",
                           "model_5", "model_6", "model_7", "model_8",
                           "model_9", "model_10", "model_11", "model_12")


for (i in 1:length(ids)){
  ngll_values[i,] <- array(unlist(result[[i]]$v), dim = c(1, nm))
}




par_ests <- array(NA, c(nm, 8, np))
for (i in 1:length(ids)){
  par_ests[,,i] <- array(unlist(result[[i]]$p), dim = c(nm, 8))
}



saveRDS(ngll_values, "IGT_values.RDS")
saveRDS(par_ests,"IGT_par_est.RDS")

