


install.packages("R.matlab")

library(R.matlab)


cross_dsads <- readMat("exp1_1.mat")

cross_dsads$triggers






filename <- file.choose()
m_v <- 0
m_v <- readRDS("mat_value.rds")
filename <- file.choose()
m_p <- readRDS(filename)
filename <- file.choose()
m_n <- readRDS(filename)

read.csv()



par_num <- c(2,3, 3,4,5,5,5,7)



### AIC

np
m_aic <- array(0, c(np,8,5))

for (x in 1:5){
  for (i in 1:np){
    m_aic[i, ,x] <- m_v[i, ,x]*2 + 2*par_num
  }
  print(x)
}


best_mod_aic <- rep(0, 8)

pos_neg <- rep(0,2)

for (x in 5:5){
  for (i in 2:np){
    p_holder <- rep(0, 8)
    p_holder[which.min(m_aic[i,,x])] <- 1
    best_mod_aic <- best_mod_aic + p_holder
    if (which.min(m_aic[i,,x])==2){
      if (m_p[2,2,i,x] < m_p[2,3,i,x]){
        pos_neg[2] <- pos_neg[2] + 1
      } else {
        pos_neg[1] <- pos_neg[1] + 1
      }
    }
    if (which.min(m_aic[i,,x])==3){
      if (m_p[3,2,i,x] < m_p[3,3,i,x]){
        pos_neg[2] <- pos_neg[2] + 1
      } else {
        pos_neg[1] <- pos_neg[1] + 1
      }
    }
    if (which.min(m_aic[i,,x])==5){
      if (m_p[5,2,i,x] < m_p[5,3,i,x]){
        pos_neg[2] <- pos_neg[2] + 1
      } else {
        pos_neg[1] <- pos_neg[1] + 1
      }
    }
    if (which.min(m_aic[i,,x])==6){
      if (m_p[6,2,i,x] < m_p[6,3,i,x]){
        pos_neg[2] <- pos_neg[2] + 1
      } else {
        pos_neg[1] <- pos_neg[1] + 1
      }
    }
  }
  print(x)
}

best_mod_aic




m_bic <- array(0, c(np,8,5))

for (x in 1:5){
  for (i in 1:np){
    m_bic[i, ,x] <- m_v[i, ,x]*2 + log(m_n[i])*par_num
  }
  print(x)
}



best_mod_bic <- rep(0, 8)

for (x in 1:1){
  for (i in 2:np){
    p_holder <- rep(0, 8)
    p_holder[which.min(m_bic[i,,x])] <- 1
    best_mod_bic <- best_mod_bic + p_holder
    if (which.min(m_bic[i,,x])==2){
      if (m_p[2,2,i,x] < m_p[2,3,i,x]){
        pos_neg[2] <- pos_neg[2] + 1
      } else {
        pos_neg[1] <- pos_neg[1] + 1
      }
    }
    if (which.min(m_bic[i,,x])==3){
      if (m_p[3,2,i,x] < m_p[3,3,i,x]){
        pos_neg[2] <- pos_neg[2] + 1
      } else {
        pos_neg[1] <- pos_neg[1] + 1
      }
    }
    if (which.min(m_bic[i,,x])==5){
      if (m_p[5,2,i,x] < m_p[5,3,i,x]){
        pos_neg[2] <- pos_neg[2] + 1
      } else {
        pos_neg[1] <- pos_neg[1] + 1
      }
    }
    if (which.min(m_bic[i,,x])==6){
      if (m_p[6,2,i,x] < m_p[6,3,i,x]){
        pos_neg[2] <- pos_neg[2] + 1
      } else {
        pos_neg[1] <- pos_neg[1] + 1
      }
    }
  }
  print(x)
}

best_mod_bic
