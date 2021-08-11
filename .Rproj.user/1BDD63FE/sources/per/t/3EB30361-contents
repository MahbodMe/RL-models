


IGT_ngll <- readRDS("server_runs//IGT_bfit_MR_scheme_1//IGT_MR_bfit_scheme_1_ngll.RDS")# Negative Log-likelihood values


IGT_MI_1 <- matrix(NA, 12, 2)

colnames(IGT_MI_1) <- c("AIC_ident_1", "BIC_ident_1")
rownames(IGT_MI_1) <- c("model_1", "model_2", "model_3", "model_4",
                           "model_5", "model_6", "model_7", "model_8",
                           "model_9", "model_10", "model_11", "model_12")

ngll <- IGT_ngll


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (i in 1:12){
  aic_mins <- matrix(0, 12, 1)
  bic_mins <- matrix(0, 12, 1)
  for (id in 1:100){
    m_aic[id, ,i] <- ngll[id, ,i]*2 + 2*par_num
    m_bic[id, ,i]<- ngll[id, ,i]*2 + log(100)*par_num
    aic_mins[which.min(m_aic[id, ,i])] <- aic_mins[which.min(m_aic[id, ,i])] + 1
    bic_mins[which.min(m_bic[id, ,i])] <- bic_mins[which.min(m_bic[id, ,i])] + 1
  }
  IGT_MI_1[i,1] <- aic_mins[i]/sum(aic_mins)
  IGT_MI_1[i,2] <- bic_mins[i]/sum(bic_mins)
}




IGT_ngll <- readRDS("server_runs//IGT_bfit_MR_scheme_2//IGT_MR_bfit_scheme_2_ngll.RDS")# Negative Log-likelihood values


IGT_MI_2 <- matrix(NA, 12, 2)

colnames(IGT_MI_2) <- c("AIC_ident_2", "BIC_ident_2")
rownames(IGT_MI_2) <- c("model_1", "model_2", "model_3", "model_4",
                        "model_5", "model_6", "model_7", "model_8",
                        "model_9", "model_10", "model_11", "model_12")

ngll <- IGT_ngll


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (i in 1:12){
  aic_mins <- matrix(0, 12, 1)
  bic_mins <- matrix(0, 12, 1)
  for (id in 1:100){
    m_aic[id, ,i] <- ngll[id, ,i]*2 + 2*par_num
    m_bic[id, ,i]<- ngll[id, ,i]*2 + log(100)*par_num
    aic_mins[which.min(m_aic[id, ,i])] <- aic_mins[which.min(m_aic[id, ,i])] + 1
    bic_mins[which.min(m_bic[id, ,i])] <- bic_mins[which.min(m_bic[id, ,i])] + 1
  }
  IGT_MI_2[i,1] <- aic_mins[i]/sum(aic_mins)
  IGT_MI_2[i,2] <- bic_mins[i]/sum(bic_mins)
}



IGT_ngll <- readRDS("server_runs//IGT_bfit_MR_scheme_3//IGT_MR_bfit_scheme_3_ngll.RDS")# Negative Log-likelihood values


IGT_MI_3 <- matrix(NA, 12, 2)

colnames(IGT_MI_3) <- c("AIC_ident_3", "BIC_ident_3")
rownames(IGT_MI_3) <- c("model_1", "model_2", "model_3", "model_4",
                        "model_5", "model_6", "model_7", "model_8",
                        "model_9", "model_10", "model_11", "model_12")

ngll <- IGT_ngll


dims <- dim(ngll)


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 1)
bic_mins <- matrix(0, 12, 1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)


for (i in 1:12){
  aic_mins <- matrix(0, 12, 1)
  bic_mins <- matrix(0, 12, 1)
  for (id in 1:100){
    m_aic[id, ,i] <- ngll[id, ,i]*2 + 2*par_num
    m_bic[id, ,i]<- ngll[id, ,i]*2 + log(100)*par_num
    aic_mins[which.min(m_aic[id, ,i])] <- aic_mins[which.min(m_aic[id, ,i])] + 1
    bic_mins[which.min(m_bic[id, ,i])] <- bic_mins[which.min(m_bic[id, ,i])] + 1
  }
  IGT_MI_3[i,1] <- aic_mins[i]/sum(aic_mins)
  IGT_MI_3[i,2] <- bic_mins[i]/sum(bic_mins)
}

IGT_MI <- cbind(IGT_MI_1, IGT_MI_2, IGT_MI_3)
write.csv(IGT_MI, "IGT_MI.csv")

