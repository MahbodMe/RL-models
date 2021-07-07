

filename <- file.choose()
sc1 <- readRDS(filename)
filename <- file.choose()
sc2 <- readRDS(filename)
filename <- file.choose()
sc3 <- readRDS(filename)
filename <- file.choose()
sc4 <- readRDS(filename)
filename <- file.choose()
sc5 <- readRDS(filename)
filename <- file.choose()
sc6 <- readRDS(filename)
filename <- file.choose()
sc7 <- readRDS(filename)
filename <- file.choose()
sc8 <- readRDS(filename)
filename <- file.choose()
sc9 <- readRDS(filename)


sc1_1 <- sc1[,,1:515]
sc2_2 <- sc2[,,516:1030]
sc3_3 <- sc3[,,1031:1560]
sc4_4 <- sc4[,,1561:2090]
sc5_5 <- sc5[,,2091:2620]
sc6_6 <- sc6[,,2621:3150]
sc7_7 <- sc7[,,3151:3700]
sc8_8 <- sc8[,,3701:4240]
sc9_9 <- sc9[,,4241:4704]




filename <- file.choose()
nsc1 <- readRDS(filename)
filename <- file.choose()
nsc2 <- readRDS(filename)
filename <- file.choose()
nsc3 <- readRDS(filename)
filename <- file.choose()
nsc4 <- readRDS(filename)
filename <- file.choose()
nsc5 <- readRDS(filename)
filename <- file.choose()
nsc6 <- readRDS(filename)
filename <- file.choose()
nsc7 <- readRDS(filename)
filename <- file.choose()
nsc8 <- readRDS(filename)
filename <- file.choose()
nsc9 <- readRDS(filename)



nsc1_1 <- nsc1[,,1:515]
nsc2_2 <- nsc2[,,516:1030]
nsc3_3 <- nsc3[,,1031:1560]
nsc4_4 <- nsc4[,,1561:2090]
nsc5_5 <- nsc5[,,2091:2620]
nsc6_6 <- nsc6[,,2621:3150]
nsc7_7 <- nsc7[,,3151:3700]
nsc8_8 <- nsc8[,,3701:4240]
nsc9_9 <- nsc9[,,4241:4704]






library(abind)
new_df <- abind(sc1_1, sc2_2, sc3_3, sc4_4, sc5_5, sc6_6, sc7_7, sc8_8, sc9_9,
                along = 3)
nnew_df <- abind(nsc1_1, nsc2_2, nsc3_3, nsc4_4, nsc5_5, nsc6_6, nsc7_7, nsc8_8, nsc9_9,
                  along = 3)


par_num <- c(2,3,4,6,6,8)



### AIC

m_aic <- array(0, c(6,6,r))

for (x in 1:r){
  for (i in 1:6){
    m_aic[i, ,x] <- new_df[i, ,x]*2 + 2*par_num
  }
  print(x)
}

# for (i in 1:6){
#   m_aic[i,i,42] <- .0001 
# }

acc_dis <- rep(NA, r)


for (x in 1:x){
  min_mod <- rep(NA, 6)
  for (i in 1:6){
    min_mod[i] <- which.min(m_aic[, i, x])
  }
  print(min_mod)
  des <- rep(1:6, 1)
  if(all(min_mod == des)){
    print(x)
    acc_dis[x] <- x
  }
}


acc_dis <- acc_dis[!is.na(acc_dis)]

acc_dis



nm_aic <- array(0, c(6,6,r))

for (x in 1:r){
  for (i in 1:6){
    nm_aic[i, ,x] <- nnew_df[i, ,x]*2 + 2*par_num
  }
  print(x)
}


nacc_dis <- rep(NA, r)


for (x in 1:x){
  nmin_mod <- rep(NA, 6)
  for (i in 1:6){
    nmin_mod[i] <- which.min(nm_aic[, i, x])
  }
  print(nmin_mod)
  des <- rep(1:6, 1)
  if(all(nmin_mod == des)){
    print(x)
    nacc_dis[x] <- x
  }
}


nacc_dis <- acc_dis[!is.na(acc_dis)]

nacc_dis






### BIC

m_bic <- array(0, c(6,6,r))

for (x in 1:r){
  for (i in 1:6){
    m_bic[i, ,x] <- new_df[i, ,x]*2 + log(rs[x,2])*par_num
  }
  print(x)
}



acc_dis <- rep(NA, r)


for (x in 1:x){
  min_mod <- rep(NA, 6)
  for (i in 1:6){
    min_mod[i] <- which.min(m_bic[, i, x])
  }
  print(min_mod)
  des <- rep(1:6, 1)
  if(all(min_mod == des)){
    print(x)
    acc_dis[x] <- x
  }
}


acc_dis <- acc_dis[!is.na(acc_dis)]

acc_dis



nm_bic <- array(0, c(6,6,r))

for (x in 1:r){
  for (i in 1:6){
    nm_bic[i, ,x] <- nnew_df[i, ,x]*2 + log(rs[x,2])*par_num
  }
  print(x)
}


nacc_dis <- rep(NA, r)


for (x in 1:x){
  nmin_mod <- rep(NA, 6)
  for (i in 1:6){
    nmin_mod[i] <- which.min(nm_bic[, i, x])
  }
  print(nmin_mod)
  des <- rep(1:6, 1)
  if(all(nmin_mod == des)){
    print(x)
    nacc_dis[x] <- x
  }
}


nacc_dis <- acc_dis[!is.na(acc_dis)]

nacc_dis

























results = cbind(sc1_1, sc2_2, sc3_3)



m_bic <- matrix(NA, 8,8)

par_num <- c(2,3,3,4,6,6,6,8)
for (x in 1:x){
  for (i in 1:8){
    m_bic[i,] <- mat[i, ]*2 + 2*par_num
  }
  print(x)
}



acc_dis <- rep(NA, x)

for (x in 1:x){
  min_mod <- rep(NA, 6)
  for (i in 1:6){
    min_mod[i] <- which.min(m_bic[, i, x])
  }
  print(min_mod)
  des <- rep(1:6, 1)
  if(all(min_mod == des)){
    print(x)
    acc_dis[x] <- x
  }
}


rownames(m_bic) <- rep(c("model_1", "model_2", "model_3", "model_4", "model_5", 
                         "model_6", "model_7", "model_8"), 1)
colnames(m_bic) <- rep(c("agent_1", "agent_2", "agent_3", "agent_4", "agent_5", 
                         "agent_6", "model_7", "model_8"), 1)
write.csv(m_bic, "sim2.csv")





sim_behav_data_1 <- function(data, funs, pars, no, nt, envir = parent.frame()){
  
  
  mat <- matrix(0, nrow = length(funs)*nt, ncol = 3)
  
  
  
  
  for (i in 1:length(funs)){
    
    fun <- funs[i]
    par <- as.numeric(unlist(pars[i]))
    
    
    n_par <- length(par)
    
    
    sim <- do.call(fun, c(list(data), par[1:n_par]))
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    mat[x:y ,1] <- rep(i, nt)
    mat[x:y ,2] <- head(sim$choices, nt)
    mat[x:y ,3] <- head(sim$agents_rewards, nt)
    
    
  }
  mats <- as.data.frame(cbind(1:nt, mat))
  colnames(mats) <- c("trial", "id", "deck", "payoff")
  
  return(data_sim = mats)
}


sds_inv <- seq(2,8, by=2)
sds_err <- seq(2,8, by=2)
trials <- seq(30,60, by=10)
arms <- 2


r <- length(sds_inv)*length(sds_err)*length(trials)*length(arms)

s_i <- rep(0, r)
s_e <- rep(0, r)
t_l <- rep(0, r)
a_s <- rep(0, r)

r1 <- 1
for (t in 1:length(trials)){
  for (si in 1:length(sds_inv)){
    for (se in 1:length(sds_err)){
      for (a in 1:length(arms)){
        s_i[r1] <- sds_inv[si]
        s_e[r1] <- sds_err[se]
        t_l[r1] <- trials[t]
        a_s[r1] <- arms
        r1 <- r1 + 1
      }
    }
  }
}

m_0 <- c(-4,4,-12,12,-20,20,-60,60)


agent_sims <- c("rl_simpleQ_symmetric_sim", "rl_simpleQ_asymmetric_sim", 
                "rl_simpleQ_asymmetric_sim2", "rl_sym_auto_sim", "rl_asym_auto_sim", 
                "rl_asym_auto_sim2", "rl_ucb_sim2", "rl_ucb_auto_sim2")


pars <- list(c(.5,.8), c(.3, .6, .7), c(.3, .6, .7), c(.5,.8, .5, .8), 
             c(.3, .6,.8, .5, .8), c(.3, .6,.8, .5, .8), 
             c(0.0, 1000, 16, 16, 1.96, .1),
             c(0.0, 1000, 16, 16, 1.96, .1, .8, .5))


nm <- 8


m <- array(0, c(8,8,r))


rownames(m) <- rep(c("model_1", "model_2", "model_3", "model_4", "model_5", 
                     "model_6", "model_7", "model_8"), 1)
colnames(m) <- rep(c("sim_model_1", "sim_model_2", "sim_model_3", "sim_model_4", "sim_model_5", 
                     "sim_model_6", "sim_model_7", "sim_model_8"), 1)


ns <- rep(0, r)
rs <- matrix(0, r, 7)
colnames(rs) <- c("seed", "trials", "nArms", "innovation", "error", "A1", "A2")

for (k in 57:r){
  
  rewards <- reward_generator(3, t_l[k], a_s[k], s_i[k], s_e[k], m_0[1:a_s[k]])
  rs[k, ] <- c(3, t_l[k], a_s[k], s_i[k], s_e[k], m_0[1:a_s[k]])

  
  dat <- sim_behav_data_1(rewards, agent_sims, pars, a_s[k], t_l[k])
  
  nt <- t_l[k]
  ns[k] <- t_l[k]
  
  u <- 1
  print(u)
  par1 <- find_the_best_pars(c(.0001, .0001), c(100, 1), 
                             rl_sym_negLogLik_DE, dat, nm, nt)$parameter
  
  u <- u+1
  print(u)
  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    value <- rl_sym_negLogLik(as.numeric(unlist(par1[i])), data=dat[x:y,]) 
    arms_2[1, i, k] <- value
  }
  
  par2 <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), 
                             rl_asym_negLogLik_DE, dat, nm, nt)$parameter
  
  print(u)
  u <- u+1
  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_asym_negLogLik(as.numeric(unlist(par2[i])), data=dat[x:y,]) 
    
    arms_2[2, i, k] <- value
  }
  
  
  par3 <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), 
                             rl_asym_negLogLik_DE_2, dat, nm, nt)$parameter
  print(u)
  u <- u+1
  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_asym_negLogLik_2(as.numeric(unlist(par3[i])), data=dat[x:y,]) 
    
    arms_2[3, i, k] <- value
  }
  
  
  par4 <- find_the_best_pars(c(0.0001, 0.0001, .1, .1), c(100, 1, 100 ,1),
                             rl_sym_auto_negLogLik_DE, dat, nm, nt)$parameter
  print(u)
  u <- u+1
  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_sym_auto_negLogLik(as.numeric(unlist(par4[i])), data=dat[x:y,]) 
    
    arms_2[4, i, k] <- value
  }
  
  
  par5 <- find_the_best_pars(c(0.0001, 0.0001, .0001,.1, .1), 
                             c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE, 
                             dat, nm, nt)$parameter
  print(u)
  u <- u+1
  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_asym_auto_negLogLik(as.numeric(unlist(par5[i])), data=dat[x:y,]) 
    
    arms_2[5, i, k] <- value
  }
  
  par6 <- find_the_best_pars(c(0.0001, 0.0001, .0001,.1, .1), 
                             c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE_2,
                             dat, nm, nt)$parameter
  print(u)
  u <- u+1
  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_asym_auto_negLogLik_2(as.numeric(unlist(par6[i])), data=dat[x:y,]) 
    
    arms_2[6, i, k] <- value
  }
  
  
  
  par7 <- find_the_best_pars(c(0,-100,0.001,0.0001, 0.001),
                             c(10,100,1000,100, 50), kf_sm_negLogLik_DE, 
                             dat, nm, nt)$parameter
  
  print(u)
  u <- u+1
  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_ucb_negLogLik_2(as.numeric(unlist(par7[i])), data=dat[x:y,]) 
    
    arms_2[7, i, k] <- value
  }
  
  
  
  par8 <- find_the_best_pars(c(0,-100,0.00001,0.00001, 0.0001, .1, .1),
                             c(10,100,1000,100, 50, 1, 100), kf_sm_auto_negLogLik_DE, 
                             dat, nm, nt)$parameter
  print(u)
  u <- u+1
  for (i in 1 : nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    value <- rl_ucb_auto_negLogLik(as.numeric(unlist(par8[i])), data=dat[x:y,]) 
    arms_2[8, i, k] <- value
  }
  
  
}



saveRDS(arms_2, "test.rds")
m_bic <- array(0, c(8,8,56))



par_num <- c(2,3,3,4,6,6,6,8)



for (x in 1:56){
  for (i in 1:8){
    m_bic[i, ,x] <- arms_2[i, ,x]*2 + log(t_l[r])*par_num
  }
}



m_bic <- mat_bic[-1,-1,-1]

rownames(m_bic) <- rep(c("model_1", "model_2", "model_3", "model_4", "model_5", 
                     "model_6", "model_7", "model_8"), 1)
colnames(m_bic) <- rep(c("sim_model_1", "sim_model_2", "sim_model_3", "sim_model_4", "sim_model_5", 
                     "sim_model_6", "sim_model_7", "sim_model_8"), 1)


m_bic <- m_bic %>%
  filter(model_1 == 1)Z


acc_dis <- rep(NA, 56)

for (x in 1:56){
  min_mod <- rep(0, 8)
  for (i in 1:8){
    min_mod[i] <- which.min(m_bic[1:8, i, x])
    print(which.min(m_bic[1:8, i, x]))
  }
  des <- rep(1:8, 1)
  if(all(min_mod == des)){
    print(x)
    acc_dis[x] <- x
  }
}


acc_dis <- acc_dis[!is.na(acc_dis)]



write.csv(m,"summary1.csv", row.names = FALSE)






