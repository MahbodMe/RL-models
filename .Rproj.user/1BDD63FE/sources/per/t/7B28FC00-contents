

envs <- readRDS("untitled//env.RDS")
m1 <- readRDS("untitled//mat_emp1.RDS")
m2 <- readRDS("untitled//mat_emp2.RDS")
m3 <- readRDS("untitled//mat_emp3.RDS")
m4 <- readRDS("untitled//mat_emp4.RDS")
m5 <- readRDS("untitled//mat_emp5.RDS")
m6 <- readRDS("untitled//mat_emp6.RDS")
m7 <- readRDS("untitled//matß_emp7.RDS")
m8 <- readRDS("untitled//mat_emp8.RDS")
m9 <- readRDS("untitled//mat_emp9.RDS")
m10 <- readRDS("untitled//mat_emp10.RDS")



saveRDS(m_df,"mat_emp_2015.RDS")




m_emp <- readRDS("Payoffs.DynamicIGT")

test <- load("Payoffs.DynamicIGT.Rdata")

hist(sample_log_dist(test, 1000), breaks = 1)


test <- m_emp[1,1,]

for (i in 1:length(test)){
  if (test[i]>8){
    print(test[i])
  }
}

sample_log_dist <- function(l_value, n_sample){
  l_value <- log(l_value)
  mu1 <- mean(l_value)
  s1 <- sd(l_value)
  r_v <- exp(dnorm(n_sample, mu1, s1))
  return(r_v)
}

sample_log_dist(test, 1)
m_dist

colnames(m_df) <- colnames(m1)
rownames(m_df) <- rownames(m1)

par_num <- c(2,3,3,4,5,5,5,6,6,7,7,8)

m_bic <- array(0, c(12,12,882,5))
m_aic <- array(0, c(12,12,882,5))
colnames(m_bic) <- colnames(m1)
rownames(m_bic) <- rownames(m1)
colnames(m_aic) <- colnames(m1)
rownames(m_aic) <- rownames(m1)


count1 <- rep(1:12,1)
acc_dis <- rep(NA, 2)



for (s in 1:5){
  for (i in 1:882){
    print(i)
    aic_min_mod <- rep(NA, 12)
    bic_min_mod <- rep(NA, 12)
    m_aic[,,i,s] <- m_df[,,i,s]*2 + 2*par_num
    m_bic[,,i,s] <- m_df[,,i,s]*2 + log(envs[i,2])*par_num
    for (a in 1:6){
      bic_min_mod[a] <- which.min(m_bic[a,1:12,i,s])
      aic_min_mod[a] <- which.min(m_aic[a,1:12,i,s])
    }
    if (all(aic_min_mod == count1)){
      acc_dis <- append(acc_dis, i)
    }
    if (all(bic_min_mod == count1)){
      acc_dis <- append(acc_dis, i)
    }
    print(bic_min_mod)
  }
}

acc_dis

