set.seed(33)

mu <- matrix(0.0, nrow=200, ncol=4)
mu[1,] <- c(-10, 5, 10, -5)


for (t in 2:200){mu[t,] <- mu[t-1,] + rnorm(4, mean =0, sd=2)
  }

rewards <- mu + rnorm(length(mu), mean =0, sd=4)
mu <- as.data.frame(cbind(1:200,mu))
rewards <- as.data.frame(cbind(1:200,rewards))
colnames(rewards) <- colnames(mu) <- c("trial",paste0("Arm ",1:4))

library(dplyr)
library(tidyr)
library(ggplot2)
mu %>%
  gather(key=option,value=mu,-trial) %>%
  ggplot(aes(x=trial,y=mu,colour=option)) + geom_line()


set.seed(330) # set the random seed to replicate these results exactly
mu <- matrix(0.0,nrow=200,ncol=4) # to store the time-varying means
mu[1,] <- c(-30, -10, 10, 30) # initialize the means for t=1
for(t in 2:200) {
  mu[t,] <- mu[t-1,] + rnorm(4,mean=0,sd=4) # compute mean at t based on mean at t-1
}
rewards <- mu + rnorm(length(mu),mean=0,sd=4) # generate the stochastic rewards
mu <- as.data.frame(cbind(1:200,mu))
rewards <- as.data.frame(cbind(1:200,rewards))
colnames(rewards) <- colnames(mu) <- c("trial",paste0("arm ",1:4))


mu %>%
  gather(key=option,value=mu,-trial) %>%
  ggplot(aes(x=trial,y=mu,colour=option)) + geom_line() + labs( title = "Reward at each trail (Stable Condition)", x = "Trail", y = "Reward")+ ylim(-100, 100)


set.seed(300) # set the random seed to replicate these results exactly
mu <- matrix(0.0,nrow=200,ncol=4) # to store the time-varying means
mu[1,] <- c(-30, -10, 10, 30) # initialize the means for t=1
for(t in 2:200) {
  if (t<101){
    mu[t,] <- mu[t-1,] + rnorm(4,mean=0,sd=4) # compute mean at t based on mean at t-1
  } else {
    mu[t,] <- mu[t-1,] + rnorm(4,mean=0,sd=8) # compute mean at t based on mean at t-1
  }
}
rewards <- mu + rnorm(length(mu),mean=0,sd=4) # generate the stochastic rewards
mu <- as.data.frame(cbind(1:200,mu))
rewards <- as.data.frame(cbind(1:200,rewards))
colnames(rewards) <- colnames(mu) <- c("trial",paste0("arm ",1:4))


mu %>%
  gather(key=option,value=mu,-trial) %>%
  ggplot(aes(x=trial,y=mu,colour=option)) + geom_line() + labs( title = "Reward at each trail (Varible Condition)", x = "Trail", y = "Reward") + ylim(-100, 100)


