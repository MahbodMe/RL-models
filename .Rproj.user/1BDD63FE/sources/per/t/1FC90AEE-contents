

pars2 <- c(par1, par2, par3, par4, par5, par6, par7, par8)


x <- sim_behav_data_1(rewards, c("rl_simpleQ_symmetric_sim","rl_simpleQ_asymmetric_sim")
                      , list(c(.5,3),c(.3, .6, .7)))

x <- yyy(rewards, 1, rl_simpleQ_symmetric_sim, c(.5,3))



agent_sims <- c("rl_simpleQ_symmetric_sim", "rl_simpleQ_asymmetric_sim", 
                "rl_simpleQ_asymmetric_sim2", "rl_sym_auto_sim", "rl_asym_auto_sim", 
                "rl_asym_auto_sim2", "rl_ucb_sim2", "rl_ucb_auto_sim2")


pars <- list(c(.5, 1), c(.3, .6, 1), c(.3, .6, 1), c(.5,1, .5, .8), 
             c(.3, .6, 1, .5, .8), c(.3, .6, 1, .5, .8), 
             c(0.0, 1000, 16, 16, 1.96, 1),
             c(0.0, 1000, 16, 16, 1.96, 1, .8, .5))



pars <- list(c(.5,.8), c(.3, .6, .7), c(.3, .6, .7), c(.5,.8, 2, 1), 
             c(.3, .6,.8, 2, 1), c(.3, .6,.8, 2, 1), 
             c(0.0, 1000, 16, 16, 1.96, .1), 
             c(0.0, 1000, 16, 16, 1.96, .1, 2, 1))

arm
nt


dat <- sim_behav_data_1(rewards, agent_sims, pars, arm, nt)



rl_simpleQ_symmetric_sim(rewards,.5,.8)
for (i in 1:8){
  
  x <- (i-1)*200 + 1
  y <- i*200
  print(sum(dat$payoff[x:y]))
  
}

find_the_best_pars <- function(para, fun, df, nm, nt){
  
  
  model <- rep("NA", nm)
  parameter <- rep(0, nm)
  value <- rep(0, nm)
  
  for (i in 1:nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    print(x)
    print(y)
    
    est <- optim(para, fun, data=df[x:y,])
    
    
    model[i] <- i
    parameter[i] <- list(est$par)
    value[i]<- est$value
    
  }
  
  
  return(list(model = model, parameter = parameter, value = value))
}


find_the_best_pars(c(.3, .6,.8, .5, .8), rl_asym_sim2, df_sim_1, 1, 200)


find_the_best_pars(c(.3, .5), rl_sym_negLogLik, df_sim_1, 8, 200)




find_the_best <- function(para, fun, df, nm, nt){
  
  
  model <- rep("NA", nm)
  parameter <- rep(0, nm)
  value <- rep(0, nm)
  
  for (i in 1:nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    print(typeof(para))
    print(typeof(fun))
    
    
    est <- optim(para, fun, data=df[x:y,])
    
    
    model[i] <- i
    parameter[i] <- list(est$par)
    value[i]<- est$value
    
  }
  
  
  return(list(model = model, parameter = parameter, value = value))
}


find_the_best(c(.0001, .0001), rl_sym_negLogLik, dat, 1, 200)

find_the_best_pars(c(.3, .5), rl_sym_negLogLik, dat, 8, 200)

find_the_best_pars <- function(lpar, upar, fun, df, nm, nt){
  
  
  df_set <- rep("NA", nm)
  parameter <- rep(0, nm)
  
  
  for (i in 1:nm){
    
    x <- (i-1)*nt + 1
    y <- i*nt
    
    
    est_DE <- DEoptim(fun,lower=as.double(lpar),
                      upper=as.double(upar),data=df[x:y,],
                      control=DEoptim.control(trace=FALSE))
    
    
    df_set[i] <- i
    parameter[i] <- list(est_DE$optim$bestmem)
    
  }
  
  
  return(list( parameter = parameter, df_set = df_set))
}

find_the_best_pars(c(.001, .001), c(100, 1), rl_sym_negLogLik, df_sim_1, 8, 200)

find_the_best_pars(lpar=c(.001, .) , upar=, fun= , df= , nm= , nt=)

nm <- 8

rl_sym_negLogLik(c(.5,.5), dat)

optim(c(.5,.5,.5), rl_asym_negLogLik, data=dat[1:200,])


nm <- 8
nt <- 200
mat <- matrix(NA, 8, 8)


par1 <- find_the_best_pars(c(.0001, .0001), c(100, 1), rl_sym_negLogLik_DE, dat, nm, nt)$parameter

for (i in 1 : nm){
  
  x <- (i-1)*nt + 1
  y <- i*nt
  value <- rl_sym_negLogLik(as.numeric(unlist(par1[i])), data=dat[x:y,]) 
  print(value)
  mat[1, i] <- value
}


par2 <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), rl_asym_negLogLik_DE, dat, nm, nt)$parameter

for (i in 1 : nm){
  
  x <- (i-1)*nt + 1
  y <- i*nt
  
  value <- rl_asym_negLogLik(as.numeric(unlist(par2[i])), data=dat[x:y,]) 
  print(value)
  mat[2, i] <- value
}


par3 <- find_the_best_pars(c(0.0001,0.0001,0.0001), c(100, 1, 1), rl_asym_negLogLik_DE_2, dat, nm, nt)$parameter

for (i in 1 : nm){
  
  x <- (i-1)*nt + 1
  y <- i*nt
  
  value <- rl_asym_negLogLik_2(as.numeric(unlist(par3[i])), data=dat[x:y,]) 
  print(value)
  mat[3, i] <- value
}


par4 <- find_the_best_pars(c(0.0001, 0.0001, .1, .1), c(100, 1, 100 ,1), rl_sym_auto_negLogLik_DE, dat, nm, nt)$parameter

for (i in 1 : nm){
  
  x <- (i-1)*nt + 1
  y <- i*nt
  
  value <- rl_sym_auto_negLogLik(as.numeric(unlist(par4[i])), data=dat[x:y,]) 
  print(value)
  mat[4, i] <- value
}


par5 <- find_the_best_pars(c(0.0001, 0.0001, .0001,.1, .1), 
                           c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE, 
                           dat, nm, nt)$parameter

for (i in 1 : nm){
  
  x <- (i-1)*nt + 1
  y <- i*nt
  
  value <- rl_asym_auto_negLogLik(as.numeric(unlist(par5[i])), data=dat[x:y,]) 
  print(value)
  mat[5, i] <- value
}

par6 <- find_the_best_pars(c(0.0001, 0.0001, .0001,.1, .1), 
                           c(100, 1, 1, 100 ,1), rl_asym_auto_negLogLik_DE_2, 
                           dat, nm, nt)$parameter

for (i in 1 : nm){
  
  x <- (i-1)*nt + 1
  y <- i*nt
  
  value <- rl_asym_auto_negLogLik_2(as.numeric(unlist(par6[i])), data=dat[x:y,]) 
  print(value)
  mat[6, i] <- value
}



par7 <- find_the_best_pars(c(0,-100,0.001,0.0001, 0.001),
                           c(10,100,1000,100, 50), kf_sm_negLogLik_DE, 
                           dat, nm, nt)$parameter

for (i in 1 : nm){
  
  x <- (i-1)*nt + 1
  y <- i*nt
  
  value <- rl_ucb_negLogLik_2(as.numeric(unlist(par7[i])), data=dat[x:y,]) 
  print(value)
  mat[7, i] <- value
}
ƒ


par8 <- find_the_best_pars(c(0,-100,0.00001,0.00001, 0.0001, .1, .1),
                           c(10,100,100,100, 50, 1, 100), kf_sm_auto_negLogLik_DE, 
                           dat, nm, nt)$parameter

for (i in 1 : nm){
  
  x <- (i-1)*nt + 1
  y <- i*nt
  
  value <- rl_ucb_auto_negLogLik(as.numeric(unlist(par8[i])), data=dat[x:y,]) 
  print(value)
  mat[8, i] <- value
}

mat <- as.data.frame(mat)

colnames(mat) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5",
                   "Model 6", "Model 7", "Model nm")

write.csv(mat,"summary1.csv", row.names = FALSE)






