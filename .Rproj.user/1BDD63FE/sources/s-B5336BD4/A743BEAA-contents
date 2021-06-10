
# Generating fake data


sim_behav_data <- function(data, num_sims, fun, par){
  
  n_fun <- length(formals(fun))
  
  mat <- matrix(0, nrow = num_sims*nrow(data), ncol = 3)
  
  for (i in 1:num_sims){
    if (n_fun==3){
      sim <- fun(data, par[1], par[2])
    }
    if (n_fun==4){
      sim <- fun(data, par[1], par[2], par[3])
    }
    if (n_fun==5){
      sim <- fun(data, par[1], par[2], par[3], par[4])
    }
    if (n_fun==6){
      sim <- fun(data, par[1], par[2], par[3], par[4], par[5])
    }
    if (n_fun==7){
      sim <- fun(data, par[1], par[2], par[3], par[4], par[5], par[6])
    }
    if (n_fun==8){
      sim <- fun(data, par[1], par[2], par[3], par[4], par[5], par[6], par[7])
    }
    if (n_fun==9){
      sim <- fun(data, par[1], par[2], par[3], par[4], par[5], par[6], par[7],
                 par[8])
    }
    
    x <- (i-1)*nrow(data) + 1
    
    print(x)
    print(i*nrow(data))
    mat[x:(i*nrow(data)) ,1] <- rep(i, nrow(data))
    mat[x:(i*nrow(data)) ,2] <- head(sim$choices, -1)
    mat[x:(i*nrow(data)) ,3] <- head(sim$agents_rewards, -1)
    print("hello")
  }
  mats <- as.data.frame(cbind(1:(1000), mat))
  colnames(mats) <- c("trial", "id", "deck", "payoff")

  return(data_sim = mats)
}

df_sim_1 <- sim_behav_data(rewards, 10, rl_simpleQ_symmetric_sim, c(.5,3))
