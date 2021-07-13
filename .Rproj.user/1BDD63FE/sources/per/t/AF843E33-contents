




rewards <- reward_generator(3, 80, 4, 16,16,c(4,-4,16,-16))
plot_rewards(rewards)


mat_sim <- matrix(0, nrow = 12, ncol =1)
rownames(mat_sim) <- c("agent_1","agent_2","agent_3","agent_4",
                       "agent_5","agent_6","agent_7","agent_8",
                       "agent_9", "agent_10", "agent_11", "agent_12")


for (n in 1:100){
  sim <- c(sym_sim(rewards, .5, .8)$agents_total_rewards,
           asym_rpe_sim(rewards, .6, .2, .8)$agents_total_rewards,
           asym_reward_sim(rewards, .6, .2, .8)$agents_total_rewards,
           sym_auto_sim(rewards, .5, .8, .02, 1)$agents_total_rewards,
           asym_rpe_auto_sim(rewards, .6, .2, .8, .02, 1)$agents_total_rewards,
           asym_reward_auto_sim(rewards, .6, .2, .8, .02, 1)$agents_total_rewards,
           ucb_sim(rewards , 0.0, 1000, 16, 16, 1.96)$agents_total_rewards,
           ucb_asym_rpe_sim(rewards, 0, 1000, 16, 10, 22, 1.96)$agents_total_rewards,
           ucb_asym_reward_sim(rewards, 0, 1000, 16, 10, 22, 1.96)$agents_total_rewards,
           ucb_auto_sim(rewards , 0.0, 1000, 16, 16, 1.96, 0.5, .8)$agents_total_rewards,
           ucb_asym_rpe_auto_sim(rewards, 0, 1000, 16, 12, 22, 1.96, .8, 1)$agents_total_rewards,
           ucb_asym_reward_auto_sim(rewards, 0, 1000, 16, 12, 22, 1.96, .8, 1)$agents_total_rewards)
  
  print(n)
  mat_sim[,1] <- c(mat_sim[,1] + sim)
}

mat_sim[,1] <- mat_sim[,1]/100

mat_sim
  
write.csv(mat_sim, "sim.csv")


