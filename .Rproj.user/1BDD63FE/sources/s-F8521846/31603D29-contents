


m1 <- readRDS("mat_emp10.rds")


par_num <- c(2,3,3,4,5,5,5,6,6,7,8,8)

par_num <- c(2,3,3,4,5,5)

par_num <- c(5,6,6,7,8,8)
par_num <- c(2,3,4,5)


m_df <- m1[c(1,2,4,5),c(1,2,4,5),581:714,5]




m_aic <- array(0, c(4,4,133))
m_bic <- array(0, c(4,4,133))


for (s in 1:1){
  for (i in 1:133){
    for (a in 1:4){
      m_aic[a, ,i] <- m_df[a,,i]*2 + 2*par_num
      m_bic[a, ,i] <- m_df[a,,i]*2 + log(rs[s,2])*par_num
    }
  }
}



acc_dis <- rep(NA, 133)

for (i in 1:133){
  aic_min_mod <- rep(NA, 4)
  bic_min_mod <- rep(NA, 4)
  for (a in 1:4){
    bic_min_mod[a] <- which.min(m_bic[a,,i])
    aic_min_mod[a] <- which.min(m_aic[a,,i])
  }
  per <- rep(1:4, 1)
  if (all(aic_min_mod == per)){
    print(i)
    acc_dis[i] <- i
  }
  if (all(bic_min_mod == per)){
    print(i)
    acc_dis[i] <- i
  }
  print(aic_min_mod)
}



nacc_dis <- acc_dis[!is.na(acc_dis)]

nacc_dis





for (x in 1:56){
  min_mod <- rep(0, 6)
  for (i in 1:6){
    min_mod[i] <- which.min(m_bic[i,, x])
    print(which.min(m_bic[i,,x]))
  }
  des <- rep(1:6, 1)
  if(all(min_mod == des)){
    print(x)
    acc_dis[x] <- x
  }
}




t <- 163
t <- 205
t <- 581 + 97
t <- 662

rewards <- reward_generator(rs[t,1], rs[t,2], rs[t,3], rs[t,4], rs[t,5], 
                            rs[t,6:(5+arm)])


fun <- sym_negLogLik_DE
df <- dat

find_the_best_pars2 <- function(var){
  lpar <- as.double(unlist(var[1]))
  upar <- as.double(unlist(var[2]))
  nm <- as.double(unlist(var[4]))
  nt <-as.double(unlist(var[5]))
  value <- rep("NA", nm)
  parameter <- rep(0, nm)
  for (i in 1:nm){
    x <- (i-1)*nt + 1
    y <- i*nt
    est_DE <- DEoptim(fun,lower=as.double(lpar),
                      upper=as.double(upar),data=df[x:y,],
                      control=DEoptim.control(trace=FALSE))
    value[i] <- list(est_DE$optim$bestval)
    parameter[i] <- list(est_DE$optim$bestmem)
  }
  return(list(parameter = parameter, value = value))
}

arm <<-8


find_the_best_pars <- function(lpar, upar, fun, df, nm, nt){
  print(typeof(lpar))
  print(typeof(upar))
  print(typeof(data))
  print(typeof(nm))
  print(typeof(nt))
  value <- rep("NA", nm)
  parameter <- rep(0, nm)
  for (i in 1:nm){
    x <- (i-1)*nt + 1
    y <- i*nt
    est_DE <- DEoptim(fun,lower=as.double(lpar),
                      upper=as.double(upar),data=df[x:y,],
                      control=DEoptim.control(trace=FALSE))
    value[i] <- list(est_DE$optim$bestval)
    parameter[i] <- list(est_DE$optim$bestmem)
  }
  return(list(parameter = parameter, value = value))
}

est <- find_the_best_pars(c(.0001, .0001), c(100, 1), sym_negLogLik_DE, dat, nm, nt)$value
est <- find_the_best_pars2(list(c(.0001, .0001), c(100, 1), nm, nt))$value
