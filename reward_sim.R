
### the following function generates rewards 

reward_generator <- function(seed, nt, no, error_term, innovation_term, mu0){
  
  set.seed(seed) # set the random seed to replicate these results exactly
  mu <- matrix(0.0,nrow=nt,ncol=no) # to store the time-varying means
  mu[1,] <- mu0 # initialize the means for t=1
  for(t in 2:nt) {
    mu[t,] <- mu[t-1,] + rnorm(no,mean=0,sd=innovation_term) # compute mean at t based on mean at t-1
  }
  rewards <- mu + rnorm(length(mu),mean=0,sd=error_term) # generate the stochastic rewards
  mu <- as.data.frame(cbind(1:nt,mu))
  rewards <- as.data.frame(cbind(1:nt,rewards))
  colnames(rewards) <- colnames(mu) <- c("trial",paste0("A",1:no))
  
  return(rewards)
}


### the following function plots rewards 

plot_rewards <- function(data){
  return(
    data %>%
      gather(key=option,value=data,-trial) %>%
      ggplot(aes(x=trial,y=data,colour=option)) + geom_line()
  )
}



rewards <- reward_generator(3, 200, 2, 4, 4, c(-20, 20))
plot_rewards(rewards)
