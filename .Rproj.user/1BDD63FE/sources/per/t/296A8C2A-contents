


m_df <- readRDS("IGT_values.RDS")# Negative Log-likelihood values
par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)

m_bic <- matrix(NA, 504, 12)
m_aic <- matrix(NA, 504, 12)


for (id in 1:504){
  m_aic[id, ] <- m_df[id, ]*2 + 2*par_num
  m_bic[id, ]<- m_df[id,]*2 + log(100)*par_num    
}


aic_mins <- rep(0, 12)
bic_mins <- rep(0, 12)

for (id in 1:504){
  aic_mins[which.min(m_aic[id, ])] <- aic_mins[which.min(m_aic[id, ])] + 1
  bic_mins[which.min(m_bic[id, ])] <- bic_mins[which.min(m_bic[id, ])] + 1
}



m_emp <- readRDS("IGT_values.RDS")
m_dist <- readRDS("mat_emp.RDS")
env <- readRDS("env.RDS")

f <- c(1,4,5)
f2 <- 7:12
m_df1 <- m_emp[f,f,,]
m_df2 <- m_dist[f,f,,]
m_df3 <- m_emp[f2,f2,,]
m_df4 <- m_dist[f2,f2,,]

par_num1 <- c(2,3,3,4,5,5,5,6,6,7,8,8)
par_num2 <- c(2,3,4,5)
par_num3 <- c(5,6,6,7,8,8)
k <- 6

par_num <-
m_df <- m_emp

m_bic <- array(0, c(k,k,11,40))
m_aic <- array(0, c(k,k,11,40))


count1 <- rep(1:k,1)
acc_dis <- rep(NA, 2)



for (s in 1:40){
  for (i in 1:11){
    print(i)
    aic_min_mod <- rep(NA, k)
    bic_min_mod <- rep(NA, k)
    m_aic[,,i,s] <- m_df[,,i,s]*2 + 2*par_num
    m_bic[,,i,s] <- m_df[,,i,s]*2 + log(env[kf_envs[i],2])*par_num
    for (a in 1:k){
      bic_min_mod[a] <- which.min(m_bic[a,1:k,i,s])
      aic_min_mod[a] <- which.min(m_aic[a,1:k,i,s])
    }
    if (all(aic_min_mod == count1)){
      acc_dis <- append(acc_dis, kf_envs[i])
    }
    if (all(bic_min_mod == count1)){
      acc_dis <- append(acc_dis, kf_envs[i])
    }
    print(bic_min_mod)
  }
}

unique(acc_dis)






id_environments <- append(id_environments, acc_dis[3:length(acc_dis)], after = 2)
id_environments <- unique(id_environments)


id_environments <- acc_dis[3:length(acc_dis)]


id_environments_qlearning <- unique(id_environments_qlearning)

ql_envs <- append(ql_envs, acc_dis[3:length(acc_dis)], after = 2)
length(ql_envs)
ql_envs <- unique(ql_envs)


saveRDS(ql_envs, "id_environments_qlearning")
ql_envs <- readRDS("ql_envs")

saveRDS(kf_envs, "kf_envs")
kf_envs <- readRDS("kf_envs")



kf_envs <- append(kf_envs, acc_dis[3:length(acc_dis)], after = 2)
length(kf_envs)
kf_envs <- unique(kf_envs)



# q = 7 7 236 254 266 287 295 302 340 346 382 394 416 479 628 676  28  46 166 223 311 314 317 326 331 332 335 344 353 379 383 386 389 403 478 505 551 592 595 605 623 638 659 682 694 697 722 724 727 832 836
# kf = 259 548 563 569 575 581 653 668 686 817 536




