


m1 <- readRDS("value1.rds")
m2 <- readRDS("value2.rds")
m3 <- readRDS("value3.rds")
m4 <- readRDS("value4.rds")
m5 <- readRDS("value5.rds")


p1 <- readRDS("par1.rds")
p2 <- readRDS("par2.rds")
p3 <- readRDS("par3.rds")
p4 <- readRDS("par4.rds")
p5 <- readRDS("par5.rds")




m_df <- abind(m1[,,1], m2[,,2], m3[,,3], m4[,,4], m5[,,5], along = 3)
p_df <- abind(p1[,,,1], p2[,,,2], p3[,,,3], p4[,,,4], p5[,,,5], along = 4)



par_num <- c(2,3,3,4,5,5,5,6,6,7,7,8)



m_aic <- array(0, c(80,12,5))
u <- 1
min_mod_aic <- rep(0, 12)
count_aic <- matrix(NA, 5*80, 3)
colnames(count_aic) <- c("ID", "Seed", "Model")

for (s in 1:5){
  for (id in 1:80){
    m_aic[id, ,s] <- m_df[id, ,s]*2 + 2*par_num
    i <- which.min(m_aic[id, ,s])
    count_aic[u,] <- c(id, s, i)
    u <- u + 1
    min_mod_aic[i] <- min_mod_aic[i] + 1
  }
  print(s)
}



m_bic <- array(0, c(80,12,5))
u <- 1
min_mod_bic <- rep(0, 12)
count_bic <- matrix(NA, 5*80, 3)
colnames(count_bic) <- c("ID", "Seed", "Model")

for (s in 1:5){
  for (id in 1:80){
    m_bic[id, ,s] <- m_df[id, ,s]*2 + 2*par_num
    i <- which.min(m_bic[id, ,s])
    count_bic[u,] <- c(id, s, i)
    u <- u + 1
    min_mod_bic[i] <- min_mod_bic[i] + 1
  }
  print(s)
}





par_12_1 <- subset(count_bic, count_bic[,3] == 12)
par_12_2 <- subset(count_aic, count_aic[,3] == 12)
par_12 <- as.data.frame(rbind(par_12_1, par_12_2))
par_12 <- par_12[!duplicated(par_12$ID), ]
nrow(par_12)
m_aic[,12,] <- Inf
m_bic[,12,] <- Inf

par_11_1 <- subset(count_bic, count_bic[,3] == 11)
par_11_2 <- subset(count_aic, count_aic[,3] == 11)
par_11 <- as.data.frame(rbind(par_11_1, par_11_2))
par_11 <- par_11[!duplicated(par_11$ID), ]
nrow(par_11)
m_aic[,11,] <- Inf
m_bic[,11,] <- Inf

par_10_1 <- subset(count_bic, count_bic[,3] == 10)
par_10_2 <- subset(count_aic, count_aic[,3] == 10)
par_10 <- as.data.frame(rbind(par_10_1, par_10_2))
par_10 <- par_10[!duplicated(par_10$ID), ]
nrow(par_10)
m_aic[,10,] <- Inf
m_bic[,10,] <- Inf

par_4_1 <- subset(count_bic, count_bic[,3] == 4)
par_4_2 <- subset(count_aic, count_aic[,3] == 4)
par_4 <- as.data.frame(rbind(par_4_1, par_4_2))
par_4 <- par_4[!duplicated(par_4$ID), ]
nrow(par_4)



u <- 1
min_mod_aic <- rep(0, 12)
count_aic <- matrix(NA, 5*80, 3)
colnames(count_aic) <- c("ID", "Seed", "Model")

for (s in 1:5){
  for (id in 1:80){
    i <- which.min(m_aic[id, ,s])
    count_aic[u,] <- c(id, s, i)
    u <- u + 1
    min_mod_aic[i] <- min_mod_aic[i] + 1
  }
  print(s)
}



u <- 1
min_mod_bic <- rep(0, 12)
count_bic <- matrix(NA, 5*80, 3)
colnames(count_bic) <- c("ID", "Seed", "Model")

for (s in 1:5){
  for (id in 1:80){
    i <- which.min(m_bic[id, ,s])
    count_bic[u,] <- c(id, s, i)
    u <- u + 1
    min_mod_bic[i] <- min_mod_bic[i] + 1
  }
  print(s)
}



par_5_1 <- subset(count_bic, count_bic[,3] == 5)
par_5_2 <- subset(count_aic, count_aic[,3] == 5)
par_5 <- as.data.frame(rbind(par_5_1, par_5_2))
par_5 <- par_5[!duplicated(par_5$ID), ]
nrow(par_5)

m_aic[,5,] <- Inf
m_bic[,5,] <- Inf
m_aic[,4,] <- Inf
m_bic[,4,] <- Inf



u <- 1
min_mod_aic <- rep(0, 12)
count_aic <- matrix(NA, 5*80, 3)
colnames(count_aic) <- c("ID", "Seed", "Model")

for (s in 1:5){
  for (id in 1:80){
    i <- which.min(m_aic[id, ,s])
    count_aic[u,] <- c(id, s, i)
    u <- u + 1
    min_mod_aic[i] <- min_mod_aic[i] + 1
  }
  print(s)
}



u <- 1
min_mod_bic <- rep(0, 12)
count_bic <- matrix(NA, 5*80, 3)
colnames(count_bic) <- c("ID", "Seed", "Model")

for (s in 1:5){
  for (id in 1:80){
    i <- which.min(m_bic[id, ,s])
    count_bic[u,] <- c(id, s, i)
    u <- u + 1
    min_mod_bic[i] <- min_mod_bic[i] + 1
  }
  print(s)
}


par_9_1 <- subset(count_bic, count_bic[,3] == 9)
par_9_2 <- subset(count_aic, count_aic[,3] == 9)
par_9 <- as.data.frame(rbind(par_9_1, par_9_2))
par_9 <- par_9[!duplicated(par_9$ID), ]
nrow(par_9)

par_8_1 <- subset(count_bic, count_bic[,3] == 8)
par_8_2 <- subset(count_aic, count_aic[,3] == 8)
par_8 <- as.data.frame(rbind(par_8_1, par_8_2))
par_8 <- par_8[!duplicated(par_8$ID), ]
nrow(par_8)


par_7_1 <- subset(count_bic, count_bic[,3] == 7)
par_7_2 <- subset(count_aic, count_aic[,3] == 7)
par_7 <- as.data.frame(rbind(par_7_1, par_7_2))
par_7 <- par_7[!duplicated(par_7$ID), ]
nrow(par_7)


par_6_1 <- subset(count_bic, count_bic[,3] == 6)
par_6_2 <- subset(count_aic, count_aic[,3] == 6)
par_6 <- as.data.frame(rbind(par_6_1, par_6_2))
par_6 <- par_6[!duplicated(par_6$ID), ]
nrow(par_6)

par_3_1 <- subset(count_bic, count_bic[,3] == 3)
par_3_2 <- subset(count_aic, count_aic[,3] == 3)
par_3 <- as.data.frame(rbind(par_3_1, par_3_2))
par_3 <- par_3[!duplicated(par_3$ID), ]
nrow(par_3)

par_2_1 <- subset(count_bic, count_bic[,3] == 2)
par_2_2 <- subset(count_aic, count_aic[,3] == 2)
par_2 <- as.data.frame(rbind(par_2_1, par_2_2))
par_2 <- par_2[!duplicated(par_2$ID), ]
nrow(par_2)

par_1_1 <- subset(count_bic, count_bic[,3] == 1)
par_1_2 <- subset(count_aic, count_aic[,3] == 1)
par_1 <- as.data.frame(rbind(par_1_1, par_1_2))
par_1 <- par_1[!duplicated(par_1$ID), ]
nrow(par_1)


pars <- rbind(par_1, par_2, par_3, par_4, par_5, par_6, par_7, par_8, par_9,
              par_10, par_11, par_12)
nrow(pars)

write.csv(pars, "pars.csv")
saveRDS(p_df,"para.RDS")

















test <- c(1,2,3,4,6) 
test
test <- append(test, 5, after = 4)
test 
