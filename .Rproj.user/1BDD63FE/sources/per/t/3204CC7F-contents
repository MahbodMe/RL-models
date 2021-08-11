



MS_ngll <- readRDS("server_runs//MS_MC//MS_values.RDS")# Negative Log-likelihood values

MS_df <- read.csv("https://github.com/speekenbrink-lab/data/raw/master/Speekenbrink_Konstantinidis_2015.csv")


df <- subset(MS_df, MS_df$cond == "nts")

ids <- unique(df$id2)




ngll <- array(NA, dim = c(length(ids), 12))

for (i in 1:length(ids)){
  ngll[i,] <- MS_ngll[ids[i],]
}


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (id in 1:nrow(ngll)){
  m_aic[id, ] <- ngll[id, ]*2 + 2*par_num
  m_bic[id, ]<- ngll[id, ]*2 + log(200)*par_num
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}




MS_1_mod_mins <- cbind(aic_mins, bic_mins)

df <- subset(MS_df, MS_df$cond == "ntn")

ids <- unique(df$id2)




ngll <- array(NA, dim = c(length(ids), 12))

for (i in 1:length(ids)){
  ngll[i,] <- MS_ngll[ids[i],]
}


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (id in 1:nrow(ngll)){
  m_aic[id, ] <- ngll[id, ]*2 + 2*par_num
  m_bic[id, ]<- ngll[id, ]*2 + log(200)*par_num
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}




MS_2_mod_mins <- cbind(aic_mins, bic_mins)

df <- subset(MS_df, MS_df$cond == "ts")

ids <- unique(df$id2)



ngll <- array(NA, dim = c(length(ids), 12))

for (i in 1:length(ids)){
  ngll[i,] <- MS_ngll[ids[i],]
}


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (id in 1:nrow(ngll)){
  m_aic[id, ] <- ngll[id, ]*2 + 2*par_num
  m_bic[id, ]<- ngll[id, ]*2 + log(200)*par_num
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}




MS_3_mod_mins <- cbind(aic_mins, bic_mins)


df <- subset(MS_df, MS_df$cond == "tn")

ids <- unique(df$id2)



ngll <- array(NA, dim = c(length(ids), 12))

for (i in 1:length(ids)){
  ngll[i,] <- MS_ngll[ids[i],]
}


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (id in 1:nrow(ngll)){
  m_aic[id, ] <- ngll[id, ]*2 + 2*par_num
  m_bic[id, ]<- ngll[id, ]*2 + log(200)*par_num
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}




MS_4_mod_mins <- cbind(aic_mins, bic_mins)




MS_mod_mins <- cbind(MS_1_mod_mins, MS_2_mod_mins, MS_3_mod_mins, MS_4_mod_mins)


colnames(MS_mod_mins) <- c("1_aic", "1_bic", "2_aic", 
                            "2_bic", "3_aic", "3_bic",
                           "4_aic", "4_bic")
rownames(MS_mod_mins) <- c("model_1", "model_2", "model_3", "model_4",
                            "model_5", "model_6", "model_7", "model_8",
                            "model_9", "model_10", "model_11", "model_12")

write.csv(MS_mod_mins, "MS_mod_mins.csv")

