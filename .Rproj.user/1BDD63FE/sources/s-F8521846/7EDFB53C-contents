





reward_generator_binary <- function(seed, nt, rewards_best_first, probs_of_winning){
  no <- 2
  set.seed(seed) # set the random seed to replicate these results exactly
  mu <- matrix(0.0,nrow=nt,ncol=no) # to store the time-varying means
  for(t in 1:nt) {
    for(i in 1:no) {
      p <- probs_of_winning[i]
      p <- c(p, 1-p)
      mu[t,i] <- sample(rewards_best_first, size = 1, prob = p)
    }
  }
  rewards <- as.data.frame(cbind(1:nt,mu))
  colnames(rewards) <- c("trial",paste0("A",1:no))
  
  return(rewards)
}

reward_generator_binary(2, 24, c(.5,-.5), c(.75, .25))


plot_rewards <- function(data){
  return(
    data %>%
      gather(key=option,value=data,-trial) %>%
      ggplot(aes(x=trial,y=data,colour=option)) + geom_line()
  )
}

plot_rewards(reward_generator_binary(2, 24, c(.5,-.5), c(.75, .25)))
