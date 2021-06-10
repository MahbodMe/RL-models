



### Symmetric Simple Q learning model + unceraitnty (UCB) (Maarten's code)


library(nnet)


rl_ucb_sim <- function(rewards,m0,v0,sigma_xi_sq,sigma_epsilon_sq,beta) {
  nt <- nrow(rewards)
  no <- ncol(rewards)
  m <- matrix(m0,ncol=no,nrow=nt+1)
  v <- matrix(v0,ncol=no,nrow=nt+1)
  choice <- rep(0,nt)
  reward <- rep(0.0,nt)
  for(t in 1:nt) {
    # choose the option with the highest UCB, breaking ties at random

    choice[t] <- which.is.max(m[t,] + beta*sqrt(v[t,] + sigma_xi_sq))

    reward[t] <- rewards[t,choice[t]]
    # Kalman updates:
    kt <- rep(0,4)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
  }
  return(list(m=m,v=v,choice=choice, reward=reward, total_rewards = sum(reward)))
}


sim_9 <- rl_ucb_sim(rewards[,-1] , 0.0, 1000, 16, 16, 1.96)
