

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

kf_sm_logLik <- function(par,data) {
  gamma <- par[1]
  choice <- data$deck
  reward <- data$payoff
  m <- kalman_filter(choice,reward,4,0,1000,16,16)$m
  p <- softmax_choice_prob(m,gamma)
  lik <- p[cbind(1:nrow(data),choice)]
  logLik <- sum(log(lik))*-1
  return(logLik)
}

out <- rep(NA,length=length(gamma))
for(i in 1:length(gamma)) out[i] <- kf_sm_logLik(gamma[i],tdat)
plot(gamma,out,ylab="log-likelihood",xlab=expression(gamma))

gamma[which.max(out)]

kf_sm_negLogLik <- function(par,data) {
  -kf_sm_lik(par,data)
}


optim(par=.35,fn=kf_sm_logLik,data=tdat)

optim(par=.35,fn=kf_sm_logLik,method="Brent",lower=0,upper=100,data=tdat)




