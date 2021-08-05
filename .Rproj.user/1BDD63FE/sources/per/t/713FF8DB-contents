
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



df <- read.csv("sugawara_katahira_2021.csv")


df <- df %>%
  rename("ID" = subjectid, "payoff" = reward, "deck" = choice) %>%
  filter(deck != 0) %>%
  mutate(deck = ifelse(deck == 3 | deck == 5 | deck == 7, 1, deck)) %>%
  mutate(deck = ifelse(deck == 4 | deck == 6 | deck == 8, 2, deck))



np <- length(unique(df$ID))
nc <- length(unique(df$type))
nm <- 12
arm <- 2
innovation_term <- 0

mat_n <- rep(NA, np)
mat_value <- matrix(NA, np, nm)
colnames(mat_value) <- c("model_1", "model_2", "model_3", "model_4",
                         "model_5", "model_6", "model_7", "model_8",
                         "model_9", "model_10", "model_11", "model_12")

mat_par <- array(NA, c(nm, 8, np))




mod_MC <- function(X){
  set.seed(123)
  dat2 <- df %>%
    filter(type == X)
  s <- X
  for (t in 1:np){
    print(t)
    dat <- dat2 %>%
      filter(ID == t)
    nt <- nrow(dat)
    
    mat_n[t] <- nt
    
    # model 1
    est <- find_the_best_pars(c(.0001, .0001), c(100, 1), sym_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 1] <- as.vector(unlist(est$value))
    mat_par[1,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 2
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(10, 1, 1), asym_rpe_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 2] <- as.vector(unlist(est$value))
    mat_par[2,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    # model 3
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(10, 1, 1), asym_reward_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 3] <- as.vector(unlist(est$value))
    mat_par[3,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 4
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001,-100), c(10, 1, 1,100), sym_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 4] <- as.vector(unlist(est$value))
    mat_par[4,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 5
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001, 0.0001, -100), c(10, 1, 1, 1, 100), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 5] <- as.vector(unlist(est$value))
    mat_par[5,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
    
    
    # model 6
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001, 0.0001, -100), c(10, 1, 1, 1, 100), asym_reward_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 6] <- as.vector(unlist(est$value))
    mat_par[6,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
    
    
    # model 7
    est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001), c(10 , 100, 1000, 100, 100), ucb_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 7] <- as.vector(unlist(est$value))
    mat_par[7,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 8
    est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001, 0.0001), c(10 , 100, 1000, 100, 100, 100), ucb_asym_rpe_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 8] <- as.vector(unlist(est$value))
    mat_par[8,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
    
    # model 9
    est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001, 0.0001), c(10 , 100, 1000, 100, 100, 100), ucb_asym_reward_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 9] <- as.vector(unlist(est$value))
    mat_par[9,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))              
    
    
    # model 10
    est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, -100), c(10 , 100, 1000, 100, 100, 1, 100), ucb_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 10] <- as.vector(unlist(est$value))
    mat_par[10,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 11
    est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 1000, 100, 100, 100, 1, 100), ucb_asym_rpe_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 11] <- as.vector(unlist(est$value))
    mat_par[11,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 12
    est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 1000, 100, 100, 100, 1, 100), ucb_asym_reward_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[t, 12] <- as.vector(unlist(est$value))
    mat_par[12,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))

  }
  return(results = list(n = mat_n, v = mat_value, p = mat_par))
}


conditions <- sort(unique(df$type))
result <- mclapply(conditions, mod_MC, mc.preschedule = FALSE, mc.cores = 40)


num_obs <- matrix(NA, np, nc)
for (i in 1:length(conditions)){
  num_obs[,i] <- as.vector(unlist(result[[i]]$n))
}

colnames(num_obs) <- c("type_1", "type_2", "type_3", "type_4")


ngll_values <- array(NA, c(np, nm, nc))
for (i in 1:length(conditions)){
  ngll_values[,,i] <- array(unlist(result[[i]]$v), dim = c(np, nm))
}


par_ests <- array(NA, c(nm, 8, np, nc))
for (i in 1:length(conditions)){
  par_ests[,,,i] <- array(unlist(result[[i]]$p), dim = c(nm, 8, np))
}



saveRDS(ngll_values, "sug_kat_2021_ngll_values.RDS")
saveRDS(par_ests,"sug_kat_2021_par_est.RDS")
saveRDS(num_obs, "sug_kat_2021_num_obs.RDS")

