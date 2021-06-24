# Generating fake data


sim_behav_data <- function(data, num_sims, fun, par){
  
  
  mat <- matrix(0, nrow = num_sims*nrow(data), ncol = 3)
  
  num_par <- length(formals(fun)) - 1
  
  
  for (i in 1:num_sims){
    
    
    sim <- do.call(fun, c(list(data), par[1:num_par]))
    print(typeof(par))
    
    x <- (i-1)*nrow(data) + 1
    
    mat[x:(i*nrow(data)) ,1] <- rep(i, nrow(data))
    mat[x:(i*nrow(data)) ,2] <- head(sim$choices, -1)
    mat[x:(i*nrow(data)) ,3] <- head(sim$agents_rewards, -1)
    
  }
  mats <- as.data.frame(cbind(1:(200), mat))
  colnames(mats) <- c("trial", "id", "deck", "payoff")
  
  return(data_sim = mats)
}

df_sim_1 <- sim_behav_data(rewards, 1, rl_simpleQ_symmetric_sim, c(.5,3))


sum(df_sim_1$payoff[1:200])

df_sim_2 <- sim_behav_data(rewards, 1, rl_ucb_sim_expB, 
                           c(0.0, 1000, 16, 16, 1, 1.96))

df_sim_2 <- sim_behav_data(rewards, 10, rl_ucb_sim_expB, 
                           c(0, 1000, 16, 16, .8, 1.96))

sum(df_sim_2$payoff[1:200])



sim_behav_data2 <- function(data, num_sims, fun, par){

  mat <- matrix(0, nrow = num_sims*nrow(data), ncol = 3)
  
  num_par <- length(formals(fun)) - 1
  
  
  for (i in 1:num_sims){
    
    
    sim <- do.call(fun, c(list(data), par[1:num_par]))
    
    
    x <- (i-1)*nrow(data) + 1

    
    
    mat[x:(i*nrow(data)) ,1] <- rep(i, nrow(data))
    mat[x:(i*nrow(data)) ,2] <- head(sim$choice, nrow(data))
    mat[x:(i*nrow(data)) ,3] <- head(sim$reward ,nrow(data))
    
  }
  mats <- as.data.frame(cbind(1:(200), mat))
  colnames(mats) <- c("trial", "id", "deck", "payoff")
  
  return(data_sim = mats)
}


df_sim_3 <- sim_behav_data2(rewards[,-1], 1, rl_ucb_sim, c(0, 1000, 16, 16, 1.96))

sum(df_sim_3$payoff[1:200])


print(length(mat[x:(1*nrow(data)) ,2]))
print(length(head(sim$reward ,200)))


data <- rewards
num_sims <- 1 
fun <- rl_ucb_sim
par <- c(0, 1000, 16, 16, 1.96)
