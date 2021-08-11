



IGT_ngll <- readRDS("server_runs//IGT_MC_1//IGT_values.RDS")# Negative Log-likelihood values
IGT_pe <- readRDS("server_runs//IGT_MC_1//IGT_par_est.RDS")# Negative Log-likelihood values
IGT_df <- readRDS("server_runs//IGT_MC_1//IGT_mat.RDS")# Negative Log-likelihood values



df <- subset(IGT_df, IGT_df$reward_scheme == 1)

ids <- unique(df$ID)




ngll <- array(NA, dim = c(length(ids), 12))

for (i in 1:length(ids)){
  ngll[i,] <- IGT_ngll[ids[i],]
}


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (id in 1:nrow(ngll)){
  m_aic[id, ] <- ngll[id, ]*2 + 2*par_num
  m_bic[id, ]<- ngll[id, ]*2 + log(100)*par_num
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}




igt_1_mod_mins <- cbind(aic_mins, bic_mins)

df <- subset(IGT_df, IGT_df$reward_scheme == 2)

ids <- unique(df$ID)




ngll <- array(NA, dim = c(length(ids), 12))

for (i in 1:length(ids)){
  ngll[i,] <- IGT_ngll[ids[i],]
}


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (id in 1:nrow(ngll)){
  m_aic[id, ] <- ngll[id, ]*2 + 2*par_num
  m_bic[id, ]<- ngll[id, ]*2 + log(100)*par_num
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}




igt_2_mod_mins <- cbind(aic_mins, bic_mins)


df <- subset(IGT_df, IGT_df$reward_scheme == 3)
ids <- unique(df$ID)




ngll <- array(NA, dim = c(length(ids), 12))

for (i in 1:length(ids)){
  ngll[i,] <- IGT_ngll[ids[i],]
}


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (id in 1:nrow(ngll)){
  m_aic[id, ] <- ngll[id, ]*2 + 2*par_num
  m_bic[id, ]<- ngll[id, ]*2 + log(100)*par_num
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}




igt_3_mod_mins <- cbind(aic_mins, bic_mins)






igt_mod_mins <- cbind(igt_1_mod_mins, igt_2_mod_mins, igt_3_mod_mins)


colnames(igt_mod_mins) <- c("1_aic", "2_aic", "3_aic", 
                           "1_bic", "2_bic", "3_bic")
rownames(igt_mod_mins) <- c("model_1", "model_2", "model_3", "model_4",
                           "model_5", "model_6", "model_7", "model_8",
                           "model_9", "model_10", "model_11", "model_12")






write.csv(igt_mod_mins, "igt_mod_mins.csv")







IGT_ngll <- readRDS("server_runs//IGT_MC_2//IGT_values.RDS")# Negative Log-likelihood values
IGT_pe <- readRDS("server_runs//IGT_MC_2//IGT_par_est.RDS")# Negative Log-likelihood values
IGT_df <- readRDS("server_runs//IGT_MC_1//IGT_mat.RDS")# Negative Log-likelihood values



df <- subset(IGT_df, IGT_df$reward_scheme == 1)

ids <- unique(df$ID)




ngll <- array(NA, dim = c(length(ids), 12))

for (i in 1:length(ids)){
  ngll[i,] <- IGT_ngll[ids[i],]
}


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (id in 1:nrow(ngll)){
  m_aic[id, ] <- ngll[id, ]*2 + 2*par_num
  m_bic[id, ]<- ngll[id, ]*2 + log(100)*par_num
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}




igt_1_mod_mins <- cbind(aic_mins, bic_mins)

df <- subset(IGT_df, IGT_df$reward_scheme == 2)

ids <- unique(df$ID)




ngll <- array(NA, dim = c(length(ids), 12))

for (i in 1:length(ids)){
  ngll[i,] <- IGT_ngll[ids[i],]
}


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (id in 1:nrow(ngll)){
  m_aic[id, ] <- ngll[id, ]*2 + 2*par_num
  m_bic[id, ]<- ngll[id, ]*2 + log(100)*par_num
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}




igt_2_mod_mins <- cbind(aic_mins, bic_mins)


df <- subset(IGT_df, IGT_df$reward_scheme == 3)
ids <- unique(df$ID)




ngll <- array(NA, dim = c(length(ids), 12))

for (i in 1:length(ids)){
  ngll[i,] <- IGT_ngll[ids[i],]
}


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (id in 1:nrow(ngll)){
  m_aic[id, ] <- ngll[id, ]*2 + 2*par_num
  m_bic[id, ]<- ngll[id, ]*2 + log(100)*par_num
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}




igt_3_mod_mins <- cbind(aic_mins, bic_mins)






igt_mod_mins <- cbind(igt_1_mod_mins, igt_2_mod_mins, igt_3_mod_mins)


colnames(igt_mod_mins) <- c("1_aic", "1_bic", "2_aic", 
                             "2_bic", "3_aic", "3_bic" )
rownames(igt_mod_mins) <- c("model_1", "model_2", "model_3", "model_4",
                            "model_5", "model_6", "model_7", "model_8",
                            "model_9", "model_10", "model_11", "model_12")






write.csv(igt_mod_mins, "igt_mod_mins2.csv")
