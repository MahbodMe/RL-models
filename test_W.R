

kalman_filter <- function(choice, reward, noption, mu0, sigma0_sq, sigma_xi_sq, sigma_epsilon_sq) {
  nt <- length(choice)
  no <- noption
  m <- matrix(mu0,ncol=no,nrow=nt+1)
  v <- matrix(sigma0_sq,ncol=no,nrow=nt+1)
  for(t in 1:nt) {
    kt <- rep(0,no)
    kt[choice[t]] <- (v[t,choice[t]] + sigma_xi_sq)/(v[t,choice[t]] + sigma_xi_sq + sigma_epsilon_sq)
    m[t+1,] <- m[t,] + kt*(reward[t] - m[t,])
    v[t+1,] <- (1-kt)*(v[t,] + sigma_xi_sq)
  }
  return(list(m=m,v=v))
}

softmax_choice_prob <- function(m,gamma) {
  prob <- exp(gamma*m)
  prob <- prob/rowSums(prob) # normalize
  return(prob)
}

kf_sm_lik <- function(data, par) {
  gamma <- par[1]
  choice <- df_sim_1$deck
  reward <- df_sim_1$payoff
  m <- kalman_filter(choice,reward,4,0,1000,16,16)$m
  p <- softmax_choice_prob(m,gamma)
  pchoice <- p[cbind(1:nrow(df_sim_1),choice)]
  lik <- prod(pchoice)
  return(lik)
}

kf_sm_lik(df_sim_1, c(3))


gamma <- seq(0,2, length=200)
out <- rep(NA,length=length(gamma))
for (i in 1:length(gamma)) out[i] <- kf_sm_lik(c(gamma[i]), df_sim_1)
plot(gamma,out,ylab="likelihood",xlab=expression(gamma))



gamma <- seq(0,2,length=200)
out <- rep(NA,length=length(gamma))
for(i in 1:length(gamma)) out[i] <- kf_sm_lik(gamma[i],df_sim_1)
plot(gamma,out,ylab="likelihood",xlab=expression(gamma))
