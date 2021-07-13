# Generating fake data




# the following function takes an environment (data), a list of models (funs)  
# and a list of parameters for those models (pars); and generates a data set
# for each of the models 


#

sim_behav_data_csv <- function(data, funs, pars, no, nt, envir = parent.frame()){
  mat <- matrix(0, nrow = length(funs)*nt, ncol = 3)
  for (i in 1:length(funs)){
    fun <- funs[i]
    par <- as.numeric(unlist(pars[i]))
    sim <- do.call(fun, c(list(data), par[1:length(par)]))
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

df_sim <- sim_behav_data_csv(rewards, c("ucb_sim"), list(c(0, 1000, 16,16, 1.96)), 4, 200)

df_sim2 <- sim_behav_data_1(rewards, c("sym_sim"), list(c(1, .5)), 4, 200)



sim_behav_data_1 <- function(data, funs, pars, no, nt, envir = parent.frame()){
  mat <- matrix(0, nrow = length(funs)*nt, ncol = 3)
  for (i in 1:length(funs)){
    fun <- funs[i]
    par <- as.numeric(unlist(pars[i]))
    n_par <- length(par)
    sim <- do.call(fun, c(list(data), par[1:n_par]))
    print(sim$choices)
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


sim_behav_data_3d <- function(data, funs, pars, no, nt, envir = parent.frame()){
  df <- array(0, c(nt,4,length(funs)))
  for (i in 1:length(funs)){
    fun <- funs[i]
    print(pars[1])
    par <- as.numeric(unlist(pars[i]))
    print(par)
    sim <- do.call(fun, c(list(data), par[1:length(par)]))
    df[ ,1 , i] <- rep(1:nt, 1)
    df[ ,2 ,i] <- rep(i, nt)
    df[ ,3 ,i]<- head(sim$choices, nt)
    df[ ,4 ,i] <- head(sim$agents_rewards, nt)
  }
  colnames(df) <- c("trial", "id", "deck", "payoff")
  
  return(data_sim = df)
}

df_sim <- sim_behav_data(rewards, c("sym_sim"), list(c(1, .5)), 4, 80)

