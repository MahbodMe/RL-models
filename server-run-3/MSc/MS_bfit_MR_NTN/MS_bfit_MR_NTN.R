


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
source("simulation_models.R")



MS_df <- read.csv("https://github.com/speekenbrink-lab/data/raw/master/Speekenbrink_Konstantinidis_2015.csv")
MS_pe <- readRDS("MS_par_est.RDS")
MS_ngll_df <- readRDS("MS_values.RDS")

df <- MS_df
ids <- unique(df$id2)


MS_pe_mat <- array(NA, dim = c(12,8,length(ids)))
MS_ngll <- matrix(NA, length(ids), 12)

for (i in 1:length(ids)){
  MS_pe_mat[,,i] <- MS_pe[,,ids[i]]
  MS_ngll[i,] <- MS_ngll_df[ids[i],]
}




par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)
m_bic <- matrix(NA,length(ids) , 14)
m_aic <- matrix(NA,length(ids) , 14)


aic_mins <- rep(0, 12)
bic_mins <- rep(0, 12)

for (id in 1:length(ids)){
  m_aic[id,1:12 ] <- MS_ngll[id, ]*2 + 2*par_num
  m_bic[id,1:12 ]<- MS_ngll[id,]*2 + log(100)*par_num
  m_aic[id,13] <- which.min(m_aic[id, 1:12])
  m_bic[id,13] <- which.min(m_bic[id, 1:12])
  aic_mins[which.min(m_aic[id, 1:12])] <- aic_mins[which.min(m_aic[id, 1:12])] + 1
  bic_mins[which.min(m_bic[id, 1:12])] <- bic_mins[which.min(m_bic[id, 1:12])] + 1
  m_aic[id,14] <- id
  m_bic[id,14] <- id
}

auto_pe <- rbind(subset(m_aic, m_aic[,13] %in% c(4:6,10:12)),
                 subset(m_bic, m_bic[,13] %in% c(4:6,10:12)))
auto_pe <- unique(auto_pe[,13:14])


m_aic[,c(4:5,10:12)] <- Inf 
m_bic[,c(4:5,10:12)] <- Inf

aic_mins <- rep(0, 12)
bic_mins <- rep(0, 12)

for (id in 1:length(ids)){
  m_aic[id,13] <- which.min(m_aic[id, 1:12])
  m_bic[id,13] <- which.min(m_bic[id, 1:12])
  aic_mins[which.min(m_aic[id, 1:12])] <- aic_mins[which.min(m_aic[id, 1:12])] + 1
  bic_mins[which.min(m_bic[id, 1:12])] <- bic_mins[which.min(m_bic[id, 1:12])] + 1
  m_aic[id,14] <- id
  m_bic[id,14] <- id
}


norm_pe <- rbind(subset(m_aic, m_aic[,13] %in% c(1:3,6,7:9)),
                 subset(m_bic, m_bic[,13] %in% c(1:3,6,7:9)))


norm_pe <- unique(norm_pe[,13:14])

pe <- rbind(norm_pe, auto_pe)


n_sim <- 100
nm <- 12

seeds <- sample(1:1000, n_sim)


agent_sims <- c("sym_sim", "asym_rpe_sim", "asym_reward_sim", "sym_auto_sim", 
                "asym_rpe_auto_sim", "asym_reward_auto_sim","ucb_sim", 
                "ucb_asym_rpe_sim", "ucb_asym_reward_sim", "ucb_auto_sim",
                "ucb_asym_rpe_auto_sim", "ucb_asym_reward_auto_sim")

rewards_df <- readRDS("MS_rewards_NTN.RDS")


arm <- 4
nt <- 200
innovation_term <- 16

mat_mr <- array(NA, c(12, 12, 4))

colnames(mat_mr) <- c("model_1", "model_2", "model_3", "model_4", "model_5",
                      "model_6", "model_7", "model_8", "model_9", "model_10",
                      "model_11", "model_12")
rownames(mat_mr) <- c("agent_1", "agent_2", "agent_3", "agent_4", "agent_5",
                      "agent_6", "agent_7", "agent_8", "agent_9", "agent_10",
                      "agent_11", "agent_12") 


mod_id <- function(X){
  print(X)
  for (i2 in 1:4){
    set.seed(seeds[X])
    rewards <- rewards_df[,,i2]
    
    par1 <- subset(pe, pe[,1] == 1)
    par1 <- sample(par1[,2],1)
    par1 <- MS_pe_mat[1,1:2,par1]
    
    par2 <- subset(pe, pe[,1] == 2)
    par2 <- sample(par2[,2],1)
    par2 <- MS_pe_mat[2,1:3,par2]
    
    par3 <- subset(pe, pe[,1] == 3)
    par3 <- sample(par3[,2],1)
    par3 <- MS_pe_mat[3,1:3,par3]
    
    
    par4 <- subset(pe, pe[,1] == 4)
    par4 <- sample(par4[,2],1)
    par4 <- MS_pe_mat[4,1:4,par4]
    
    par5 <- subset(pe, pe[,1] == 5)
    par5 <- sample(par5[,2],1)
    par5 <- MS_pe_mat[5,1:5,par5]
    
    par6 <- subset(pe, pe[,1] == 6)
    par6 <- sample(par6[,2],1)
    par6 <- MS_pe_mat[6,1:5,par6]
    
    par7 <- subset(pe, pe[,1] == 7)
    par7 <- sample(par7[,2],1)
    par7 <- MS_pe_mat[7,c(1,4,5),par7]
    par7 <- append(par7, innovation_term, after = 1)
    
    par8 <- subset(pe, pe[,1] == 8)
    par8 <- sample(par8[,2],1)
    par8 <- MS_pe_mat[8,c(1,4:6),par8]
    par8 <- append(par8, innovation_term, after = 1)
    
    
    par9 <- subset(pe, pe[,1] == 9)
    par9 <- sample(par9[,2],1)
    par9 <- MS_pe_mat[9,c(1,4:6),par9]
    par9 <- append(par9, innovation_term, after = 1)
    
    par10 <- subset(pe, pe[,1] == 10)
    par10 <- sample(par10[,2],1)
    par10 <- MS_pe_mat[10,c(1,4:7),par10]
    par10 <- append(par10, innovation_term, after = 1)
    
    
    par11 <- subset(pe, pe[,1] == 11)
    par11 <- sample(par11[,2],1)
    par11 <- MS_pe_mat[11,c(1,4:8),par11]
    par11 <- append(par11, innovation_term, after = 1)
    
    
    par12 <- subset(pe, pe[,1] == 12)
    par12 <- sample(par12[,2],1)
    par12 <- MS_pe_mat[12,c(1,4:8),par12]
    par12 <- append(par12, innovation_term, after = 1)
    
    pars <- list(par1, par2, par3, par4, par5, par6, par7, par8, par9, 
                 par10, par11, par12)
    
    dat <- sim_behav_data_csv(rewards, agent_sims, pars, arm, nt)
    
    # model 1
    est <- find_the_best_pars(c(.0001, .0001), c(100, 1), sym_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,1, i2] <- as.vector(unlist(est[i]))
    }
    
    # model 2
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(10, 1, 1), asym_rpe_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,2, i2] <- as.vector(unlist(est[i]))
    }
    
    # model 3
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(10, 1, 1), asym_reward_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,3, i2] <- as.vector(unlist(est[i]))
    }
    
    
    
    # model 4
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001,-100), c(10, 1, 1,100), sym_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,4, i2] <- as.vector(unlist(est[i]))
    }
    
    
    
    # model 5
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001, 0.0001, -100), c(10, 1, 1, 1, 100), asym_rpe_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,5, i2] <- as.vector(unlist(est[i]))
    }
    
    
    
    # model 6
    est <- find_the_best_pars(c(0.0001,0.0001,0.0001, 0.0001, -100), c(10, 1, 1, 1, 100), asym_reward_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,6, i2] <- as.numeric(unlist(est[i]))
    }
    
    
    # model 7
    est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001), c(10 , 100, 2000, 100, 100), ucb_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,7, i2] <- as.numeric(unlist(est[i]))
    }
    
    
    # model 8
    est <- find_the_best_pars(c(0, -100, 0.0001, 0.0001, 0.0001, 0.0001), c(10 , 100, 2000, 100, 100, 100), ucb_asym_rpe_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,8, i2] <- as.numeric(unlist(est[i]))
    }
    
    
    # model 9
    est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001, 0.0001), c(10 , 100, 2000, 100, 100, 100), ucb_asym_reward_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,9, i2] <- as.numeric(unlist(est[i]))
    }
    
    
    # model 10
    est <- find_the_best_pars(c(0 , -100, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 2000, 100, 100, 1, 100), ucb_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,10, i2] <- as.numeric(unlist(est[i]))
    }
    
    
    # model 11
    est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 2000, 100, 100, 100, 1, 100), ucb_asym_rpe_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,11, i2] <- as.numeric(unlist(est[i]))
    }
    
    
    
    # model 12
    est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 2000, 100, 100, 100, 1, 100), ucb_asym_reward_auto_negLogLik_DE, dat, nm, nt)$value
    
    for (i in 1:nm){
      mat_mr[i,12, i2] <- as.numeric(unlist(est[i]))
    }
  }
  return(mat_mr)
}

seed_index <- rep(1:60, 1)

result <- mclapply(seed_index, mod_id, mc.preschedule = FALSE, mc.cores = 20)

mr <- array(1, dim = c(12, 12, 4, 60))


for (i in 1:length(seed_index)){
  mr[,,,i] <- array(unlist(result[[i]]), dim = c(12, 12, 4))
}

saveRDS(mr, "test.RDS")
mr2 <- array(NA, dim = c(60,12,12,4))

for (i in 1:nm){
  for (i2 in 1:length(seed_index)){
    for (i3 in 1:4){
      mr2[i2,,i,i3]<- mr[i,,i3,i2]
    }
  }
}


colnames(mr2) <- c("model_1", "model_2", "model_3", "model_4", "model_5",
                   "model_6", "model_7", "model_8", "model_9", "model_10",
                   "model_11", "model_12")


saveRDS(mr2, "MS_bfit_NTN_MR_ngll.RDS")
