
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



innovation_variance <- sd(df$payoff)*100

mat_n <- rep(NA, nc)
mat_value <- array(NA, c(1, nm, nc))
colnames(mat_value) <- c("model_1", "model_2", "model_3", "model_4",
                         "model_5", "model_6", "model_7", "model_8",
                         "model_9", "model_10", "model_11", "model_12")

mat_par <- array(NA, c(nm, 8, nc))





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
    
    # model 1
    est <- find_the_best_pars(c(.00001, .00001, -1000), c(100, 1, 1000), sym_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 1, t] <- as.vector(unlist(est$value))
    mat_par[1,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 2
    est <- find_the_best_pars(c(0.00001,0.00001,0.00001,-1000), c(100, 1, 1, 1000), asym_rpe_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 2, t] <- as.vector(unlist(est$value))
    mat_par[2,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    # model 3
    est <- find_the_best_pars(c(0.00001,0.00001,0.00001,-1000), c(100, 1, 1, 1000), asym_reward_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 3, t] <- as.vector(unlist(est$value))
    mat_par[3,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 4
    est <- find_the_best_pars(c(0.00001,0.00001,0.00001,-100, -1000), c(100, 1, 1,100,1000), sym_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 4, t] <- as.vector(unlist(est$value))
    mat_par[4,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 5
    est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, -100, -1000), c(100, 1, 1, 1, 100, 1000), asym_rpe_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 5, t] <- as.vector(unlist(est$value))
    mat_par[5,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))  
    
    
    # model 6
    est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, -100, -1000), c(100, 1, 1, 1, 100, 1000), asym_reward_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 6, t] <- as.vector(unlist(est$value))
    mat_par[6,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
    
    
    # model 7
    est <- find_the_best_pars(c(.00001, -1000, 0.00001, 0.00001, 0.00001), c(100, 1000, 2000, 2000, 100), ucb_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 7, t] <- as.vector(unlist(est$value))
    mat_par[7,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 8
    est <- find_the_best_pars(c(.00001, -1000, 0.00001, 0.00001, 0.00001, 0.00001), c(100 , 1000, 2000, 2000, 2000, 100), ucb_asym_rpe_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 8, t] <- as.vector(unlist(est$value))
    mat_par[8,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))     
    
    # model 9
    est <- find_the_best_pars(c(.00001, -100, 0.00001, 0.00001, 0.00001, 0.00001), c(100, 1000, 2000, 2000, 2000, 100), ucb_asym_reward_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 9, t] <- as.vector(unlist(est$value))
    mat_par[9,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))              
    
    
    # model 10
    est <- find_the_best_pars(c(.00001, -1000, 0.00001, 0.00001, 0.00001, 0.00001, -1000), c(100, 1000, 2000, 2000, 100, 1, 1000), ucb_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 10, t]<- as.vector(unlist(est$value))
    mat_par[10,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 11
    est <- find_the_best_pars(c(.00001, -1000, 0.00001, 0.00001, 0.00001, 0.00001, 0.00001, -1000), c(100 , 1000, 2000, 2000, 2000, 100, 1, 1000), ucb_asym_rpe_auto_negLogLik_DE, dat, 1, nt)
    par <- est$parameter
    mat_value[1, 11, t] <- as.vector(unlist(est$value))
    mat_par[11,1:length(as.vector(unlist(par))),t] <- as.vector(unlist(par[1:length(as.vector(unlist(par)))]))
    
    
    # model 12
    est <- find_the_best_pars(c(.00001, -1000, 0.00001, 0.00001, 0.00001, 0.00001, 0.00001, -1000), c(100 , 1000, 2000, 2000, 2000, 100, 1, 1000), ucb_asym_reward_auto_negLogLik_DE, dat, 1, nt)
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


par_ests <- array(NA, c(nm, 8, np, nc))
for (i in 1:length(ids)){
  par_ests[,,i,] <- array(unlist(result[[i]]$p), dim = c(nm, 8, nc))
}



saveRDS(ngll_values, "sug_kat_2021_ngll_values.RDS")
saveRDS(par_ests,"sug_kat_2021_par_est.RDS")
saveRDS(num_obs, "sug_kat_2021_num_obs.RDS")

