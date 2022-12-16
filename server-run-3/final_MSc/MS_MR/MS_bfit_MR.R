
print("HELLO")

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
ids <- ids[!ids %in% 1]

MS_pe_mat <- array(NA, dim = c(12,9,length(ids)))
MS_ngll <- matrix(NA, length(ids), 12)

for (i in 1:length(ids)){
  MS_pe_mat[,,i] <- MS_pe[,,ids[i]]
  MS_ngll[i,] <- MS_ngll_df[ids[i],]
}




par_num <- c(3,4,4,5,6,6,5,6,6,7,8,8)
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

auto_pe <- rbind(subset(m_aic, m_aic[,13] %in% c(1,3:5)),
                 subset(m_bic, m_bic[,13] %in% c(1,3:5)))
auto_pe <- unique(auto_pe[,13:14])


m_aic[,c(1,3:5)] <- Inf 
m_bic[,c(1,3:5)] <- Inf

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


norm_pe <- rbind(subset(m_aic, m_aic[,13] %in% c(2,10:12)),
                 subset(m_bic, m_bic[,13] %in% c(2,10:12)))


norm_pe <- unique(norm_pe[,13:14])


m_aic[,c(2,10:12)] <- Inf 
m_bic[,c(2,10:12)] <- Inf

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


norm_pe2 <- rbind(subset(m_aic, m_aic[,13] %in% c(6:9)),
                  subset(m_bic, m_bic[,13] %in% c(6:9)))
norm_pe2 <- unique(norm_pe2[,13:14])

pe <- rbind(norm_pe, auto_pe, norm_pe2)


n_sim <- 110
nm <- 12

seeds <- sample(1:1000, n_sim)


agent_sims <- c("sym_sim", "asym_rpe_sim", "asym_reward_sim", "sym_auto_sim", 
                "asym_rpe_auto_sim", "asym_reward_auto_sim","ucb_sim", 
                "ucb_asym_rpe_sim", "ucb_asym_reward_sim", "ucb_auto_sim",
                "ucb_asym_rpe_auto_sim", "ucb_asym_reward_auto_sim")

rewards_df <- readRDS("MS_rewards")
rewards_df <- rewards_df[,,,4]


arm <- 4
nt <- 200
e <- 16
variance_limit <- e*10

mat_mr <- array(NA, c(12, 12))


colnames(mat_mr) <- c("model_1", "model_2", "model_3", "model_4", "model_5",
                      "model_6", "model_7", "model_8", "model_9", "model_10",
                      "model_11", "model_12")
rownames(mat_mr) <- c("agent_1", "agent_2", "agent_3", "agent_4", "agent_5",
                      "agent_6", "agent_7", "agent_8", "agent_9", "agent_10",
                      "agent_11", "agent_12") 


mod_id <- function(X){
  print(X)
  set.seed(seeds[X])
  rewards <- rewards_df[,,sample(c(1:4),1)]
  
  rw <- rewards
  
  lim_intial <- c(min(rw[,2:5]),max(rw[,2:5]))
  lim_phi <- diff(lim_intial)*10
  lim_phi <- c(-1*lim_phi, lim_phi)
  lim_intial[2] <- lim_intial[1]*(-1)
  lim_inv_temp <- lim_intial

  par1 <- subset(pe, pe[,1] == 1)
  par1 <- sample(par1[,2],1)
  par1 <- MS_pe_mat[1,1:3,par1]
  
  par2 <- subset(pe, pe[,1] == 2)
  par2 <- sample(par2[,2],1)
  par2 <- MS_pe_mat[2,1:4,par2]
  
  par3 <- subset(pe, pe[,1] == 3)
  par3 <- sample(par3[,2],1)
  par3 <- MS_pe_mat[3,1:4,par3]
  
  
  par4 <- subset(pe, pe[,1] == 4)
  par4 <- sample(par4[,2],1)
  par4 <- MS_pe_mat[4,1:5,par4]
  
  par5 <- subset(pe, pe[,1] == 5)
  par5 <- sample(par5[,2],1)
  par5 <- MS_pe_mat[5,1:6,par5]
  
  par6 <- subset(pe, pe[,1] == 6)
  par6 <- sample(par6[,2],1)
  par6 <- MS_pe_mat[6,1:6,par6]
  
  par7 <- subset(pe, pe[,1] == 7)
  par7 <- sample(par7[,2],1)
  par7 <- MS_pe_mat[7,1:6,par7]
  
  par8 <- subset(pe, pe[,1] == 8)
  par8 <- sample(par8[,2],1)
  par8 <- MS_pe_mat[8,1:7,par8]
  
  
  
  par9 <- subset(pe, pe[,1] == 9)
  par9 <- sample(par9[,2],1)
  par9 <- MS_pe_mat[9,1:7,par9]
  
  
  par10 <- subset(pe, pe[,1] == 10)
  par10 <- sample(par10[,2],1)
  par10 <- MS_pe_mat[10,1:8,par10]
  
  
  
  par11 <- subset(pe, pe[,1] == 11)
  par11 <- sample(par11[,2],1)
  par11 <- MS_pe_mat[11,1:9,par11]
  
  
  
  par12 <- subset(pe, pe[,1] == 12)
  par12 <- sample(par12[,2],1)
  par12 <- MS_pe_mat[12,1:9,par12]
  
  
  pars <- list(par1, par2, par3, par4, par5, par6, par7, par8, par9, 
               par10, par11, par12)
  
  dat <- sim_behav_data_csv(rewards, agent_sims, pars, arm, nt)
  
  # model 1
  est <-find_the_best_pars(c(.00001, .00001, lim_intial[1]), c(lim_intial[2], 1, lim_intial[2]), sym_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,1] <- as.vector(unlist(est[i]))
  }
  
  # model 2
  est <-find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial[1]), c(lim_intial[2], 1, 1,  lim_intial[2]), asym_rpe_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,2] <- as.vector(unlist(est[i]))
  }
  
  # model 3
  est <-find_the_best_pars(c(0.00001,0.00001,0.00001,  lim_intial[1]), c(lim_intial[2], 1, 1,  lim_intial[2]), asym_reward_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,3] <- as.vector(unlist(est[i]))
  }
  
  
  
  # model 4
  est <-find_the_best_pars(c(0.00001,0.00001,0.00001,lim_phi[1],  lim_intial[1]), c(lim_intial[2], 1, 1,lim_phi[2],  lim_intial[2]), sym_auto_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,4] <- as.vector(unlist(est[i]))
  }
  
  
  
  # model 5
  est <-find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial[1]), c(lim_intial[2], 1, 1, 1, lim_phi[2],  lim_intial[2]), asym_rpe_auto_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,5] <- as.vector(unlist(est[i]))
  }
  
  
  
  # model 6
  est <- find_the_best_pars(c(0.00001,0.00001,0.00001, 0.00001, lim_phi[1],  lim_intial[1]), c(lim_inv_temp[2], 1, 1, 1, lim_phi[2],  lim_intial[2]), asym_reward_auto_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,6] <- as.vector(unlist(est[i]))
  }
  
  
  # model 7
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001), c(lim_inv_temp[2],  lim_intial[2], variance_limit, e, variance_limit, lim_inv_temp[2]), ucb_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,7] <- as.vector(unlist(est[i]))
  }
  
  
  # model 8
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001), c(lim_inv_temp[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2]), ucb_asym_rpe_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,8] <- as.vector(unlist(est[i]))
  }
  
  
  # model 9
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001), c(lim_inv_temp[2],  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2]), ucb_asym_reward_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,9] <- as.vector(unlist(est[i]))
  }
  
  
  # model 10
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_inv_temp[2],  lim_intial[2], variance_limit, e, variance_limit, lim_inv_temp[2], 1, lim_phi[2]), ucb_auto_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,10] <- as.vector(unlist(est[i]))
  }
  
  
  # model 11
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_inv_temp[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2], 1, lim_phi[2]), ucb_asym_rpe_auto_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,11] <- as.vector(unlist(est[i]))
  }
  
  
  
  # model 12
  est <- find_the_best_pars(c(.00001,  lim_intial[1], 0.00001, (e-.000001), 0.00001, 0.00001, 0.00001, 0.00001, lim_phi[1]), c(lim_inv_temp[2] ,  lim_intial[2], variance_limit, e, variance_limit, variance_limit, lim_inv_temp[2], 1, lim_phi[2]), ucb_asym_reward_auto_negLogLik_DE, dat, nm, nt)$value
  
  for (i in 1:nm){
    mat_mr[i,12] <- as.vector(unlist(est[i]))
  }
  return(mat_mr)
}

Res <- mod_id(1)
print(Res)
seed_index <- rep(1:110, 1)

result <- mclapply(seed_index, mod_id, mc.preschedule = FALSE, mc.cores = 40)

mr <- array(1, dim = c(12, 12, 110))


for (i in 1:length(seed_index)){
  mr[,,i] <- array(unlist(result[[i]]), dim = c(12, 12))
}

saveRDS(mr, "test.RDS")
mr2 <- array(NA, dim = c(110,12,12))

for (i in 1:nm){
  for (i2 in 1:length(seed_index)){
    mr2[i2,,i]<- mr[i,,i2]
    
  }
}


colnames(mr2) <- c("model_1", "model_2", "model_3", "model_4", "model_5",
                   "model_6", "model_7", "model_8", "model_9", "model_10",
                   "model_11", "model_12")


saveRDS(mr2, "MS_bfit_NTS_MR_ngll.RDS")
