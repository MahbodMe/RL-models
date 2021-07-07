


mat_sim <- matrix(0, nrow = 8, ncol =1)
rownames(mat_sim) <- c("agent_1","agent_2","agent_3","agent_4",
                       "agent_5","agent_6","agent_7","agent_8")

for (k in 1:r){
  k
  it <<- rs[k,4]
  arm <<- rs[k,3]
  nt <- rs[k,2]
  rewards <- reward_generator(rs[k,1], rs[k,2], rs[k,3], rs[k,4], rs[k,5], 
                              rs[k,6:(5+arm)])
  
  plot_rewards(rewards)
  

for (n in 1:100){
  sim <- c(rl_simpleQ_symmetric_sim(rewards, .5, .8)$agents_total_rewards,
           rl_simpleQ_asymmetric_sim(rewards, .6, .2, .8)$agents_total_rewards,
           rl_simpleQ_asymmetric_sim2(rewards, .6, .2, .8)$agents_total_rewards,
           rl_sym_auto_sim(rewards, .5, .8, .02, 1)$agents_total_rewards,
           rl_asym_auto_sim(rewards, .6, .2, .8, .02, 1)$agents_total_rewards,
           rl_asym_auto_sim2(rewards, .6, .2, .8, .02, 1)$agents_total_rewards,
           rl_ucb_sim2(rewards , 0.0, 1000, 16, 16, 1.96, .8)$agents_total_rewards,
           rl_ucb_auto_sim2(rewards , 0.0, 1000, 16, 16,
                                       1.96,.8, 0.5, .8)$agents_total_rewards)

  print(n)
  mat_sim[,1] <- c(mat_sim[,1] + sim)
  
}

mat_sim[,1] <- mat_sim[,1]/100

coln(mat_sim)
  
write.csv(mat_sim, "sim.csv")


