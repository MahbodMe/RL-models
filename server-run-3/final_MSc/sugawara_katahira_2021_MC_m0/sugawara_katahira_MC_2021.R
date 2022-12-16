
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



df <- read.csv("sugawara_katahira_2021.csv")



df <- df %>%
  rename("ID" = subjectid, "payoff" = reward, "deck" = choice) %>%
  filter(deck != 0) %>%
  mutate(deck = ifelse(deck == 3 | deck == 5 | deck == 7, 1, deck)) %>%
  mutate(deck = ifelse(deck == 4 | deck == 6 | deck == 8, 2, deck))


es <- rep(NA, 4)
e <- reward_generator_binary(2, 48, c(10,-10), c(.5, .5))
es[1] <- (var(e[,2]) + var(e[,3]))/2
e <- reward_generator_binary(2, 48, c(10,-10), c(.25, .75))
es[2:3] <- (var(e[,2]) + var(e[,3]))/2
e <- reward_generator_binary(2, 48, c(10,-10), c(.17, .83))
es[4] <- (var(e[,2]) + var(e[,3]))/2


lim_intial <- c(min(df$payoff),max(df$payoff))
lim_phi <- diff(lim_intial)*10
lim_phi <- c(-1*lim_phi, lim_phi)
lim_inv_temp <- lim_intial
lim_intial <- c(0, 0.000000001)


source("estimation_models.R")

np <- length(unique(df$ID))
nc <- length(unique(df$type))
nm <- 12
arm <- 2




mat_n <- rep(NA, nc)
mat_value <- array(NA, c(1, nm, nc))
colnames(mat_value) <- c("model_1", "model_2", "model_3", "model_4",
                         "model_5", "model_6", "model_7", "model_8",
                         "model_9", "model_10", "model_11", "model_12")

mat_par <- array(NA, c(nm, 9, nc))





mod_MC <- function(X){
  set.seed(123)
  dat2 <- df %>%
    filter(ID == X)
  
  print(X)
  
  for (t in 1:nc){
    dat <- dat2 %>%
      filter(type == t)
    
    nt <- nrow(dat)
    mat_n[t] <- nt
    
    
    e <- es[t]
    variance_limit <- e*10
   
    
    # model 1
    est <- find_the_best_pars(c(.00001, .00001, lim_intial[1]), c(lim_inv_temp[2], 1, lim_intial[2]), sym_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 1, t] <- as.vector(unlist(est$value))
    mat_par[1,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 2
    est <- find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial[1]), c(lim_inv_temp[2], 1, 1,  lim_intial[2]), asym_rpe_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 2, t] <- as.vector(unlist(est$value))
    mat_par[2,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    # model 3
    est <- find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial[1]), c(lim_inv_temp[2], 1, 1,  lim_intial[2]), asym_reward_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 3, t] <- as.vector(unlist(est$value))
    mat_par[3,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 4
    est <- find_the_best_pars(c(0.00001,0.00001,0.00001,lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1,lim_phi[2],  lim_intial[2]), sym_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 4, t] <- as.vector(unlist(est$value))
    mat_par[4,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 5
    est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1, 1, lim_phi[2],  lim_intial[2]), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 5, t] <- as.vector(unlist(est$value))
    mat_par[5,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
    
    
    # model 6
    est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1, 1, lim_phi[2],  lim_intial[2]), asym_reward_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 6, t] <- as.vector(unlist(est$value))
    mat_par[6,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
    
    
    # model 7
    est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001), c(lim_inv_temp[2],  lim_intial[2], variance_limit, e, variance_limit, lim_inv_temp[2]), ucb_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 7, t] <- as.vector(unlist(est$value))
    mat_par[7,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 8
    est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001), c(lim_inv_temp[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2]), ucb_asym_rpe_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 8, t] <- as.vector(unlist(est$value))
    mat_par[8,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
    
    # model 9
    est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001), c(lim_inv_temp[2],  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2]), ucb_asym_reward_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 9, t] <- as.vector(unlist(est$value))
    mat_par[9,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))              
    
    
    # model 10
    est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_inv_temp[2],  lim_intial[2], variance_limit, e, variance_limit, lim_inv_temp[2], 1, lim_phi[2]), ucb_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 10, t]<- as.vector(unlist(est$value))
    mat_par[10,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 11
    est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_inv_temp[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2], 1, lim_phi[2]), ucb_asym_rpe_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 11, t] <- as.vector(unlist(est$value))
    mat_par[11,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 12
    est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_inv_temp[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2], 1, lim_phi[2]), ucb_asym_reward_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 12, t] <- as.vector(unlist(est$value))
    mat_par[12,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
  }
  return(results = list(n = mat_n, v = mat_value, p = mat_par))
}


ids <- sort(unique(df$ID))
result <- mclapply(ids, mod_MC, mc.preschedule = FALSE, mc.cores = 40)


num_obs <- matrix(NA, np, nc)
for (i in 1:length(ids)){
  num_obs[i,] <- as.vector(unlist(result[[i]]$n))
}

colnames(num_obs) <- c("type_1", "type_2", "type_3", "type_4")


ngll_values <- array(NA, c(np, nm, nc))
for (i in 1:length(ids)){
  ngll_values[i,,] <- array(unlist(result[[i]]$v), dim = c(1, nm, nc))
}


par_ests <- array(NA, c(nm, 9, np, nc))
for (i in 1:length(ids)){
  par_ests[,,i,] <- array(unlist(result[[i]]$p), dim = c(nm, 9, nc))
}



saveRDS(ngll_values, "sug_kat_2021_ngll_values.RDS")
saveRDS(par_ests,"sug_kat_2021_par_est.RDS")
saveRDS(num_obs, "sug_kat_2021_num_obs.RDS")

