

sims <- 0
for (n in 1:1000){
  sim_5 <- rl_asym_auto_sim(rewards, .6, .2, 3, .002, 1)
  
  sims <- sims + sim_5$agents_total_rewards
  print(sim_5$agents_total_rewards)
}
print(sims/1000)