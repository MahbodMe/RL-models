ngll <- readRDS("server_runs//sugawara_katahira_2021_MC//sug_kat_2021_ngll_values.RDS")# Negative Log-likelihood values
dims <- dim(ngll)
envs <- readRDS("server_runs//sugawara_katahira_2021_MC//sug_kat_2021_num_obs.RDS")
est_p <- readRDS("server_runs//sugawara_katahira_2021_MC//sug_kat_2021_par_est.RDS")


m_bic <- array(NA, dim = dims)
m_aic <- array(NA, dim = dims)
aic_mins <- matrix(0, 12, 4)
bic_mins <- matrix(0, 12, 4)
p_bmod <- matrix(NA, 143, 9)
colnames(p_bmod) <- c("ID", "1_aic", "2_aic", "3_aic", "4_aic", 
                      "1_bic", "2_bic", "3_bic", "4_bic")

par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)

for (i in 1:4){
  for (id in 1:143){
    m_aic[id, ,i] <- ngll[id, ,i]*2 + 2*par_num
    m_bic[id, ,i]<- ngll[id, ,i]*2 + log(envs[id,i])*par_num
    aic_mins[which.min(m_aic[id, ,i]),i] <- aic_mins[which.min(m_aic[id, ,i]),i] + 1
    bic_mins[which.min(m_bic[id, ,i]),i] <- bic_mins[which.min(m_bic[id, ,i]),i] + 1
    p_bmod[id,1] <- id
    p_bmod[id,(i+1)] <- which.min(m_aic[id, ,i])
    p_bmod[id,(i+5)] <- which.min(m_bic[id, ,i])
  }
}




SK_mod_mins <- cbind(aic_mins, bic_mins)
colnames(SK_mod_mins) <- c("1_aic", "2_aic", "3_aic", "4_aic", 
                      "1_bic", "2_bic", "3_bic", "4_bic")
rownames(SK_mod_mins) <- c("model_1", "model_2", "model_3", "model_4",
                           "model_5", "model_6", "model_7", "model_8",
                           "model_9", "model_10", "model_11", "model_12")

write.csv(SK_mod_mins, "SK_mod_mins.csv")

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()



df <- p_bmod[,2:5]


colnames(bic_mins) <- c("1_bic", "2_bic", "3_bic", "4_bic")
rownames(bic_mins) <- c("model_1", "model_2", "model_3", "model_4",
                           "model_5", "model_6", "model_7", "model_8",
                           "model_9", "model_10", "model_11", "model_12")

colnames(aic_mins) <- c("1_aic", "2_aic", "3_aic", "4_aic")
rownames(aic_mins) <- c("model_1", "model_2", "model_3", "model_4",
                        "model_5", "model_6", "model_7", "model_8",
                        "model_9", "model_10", "model_11", "model_12")

install.packages("rcompanion")
library(rcompanion)

test <- rep(1:3,1)
test2 <- rep(5:7,1)
cramerV(bic_mins[1:12,])


hist(mod_2_alpha)
mod_2_alpha <- est_p[6,2,,] - est_p[6,3,,]
e1071::skewness(mod_2_alpha)
t.test(mod_2_alpha, mu = 0, alternative = "less")



mod_4_ac <- est_p[6,5,,] 
hist(mod_4_ac)
e1071::skewness(mod_4_ac)
t.test(mod_4_ac, mu = 0)


