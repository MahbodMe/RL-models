


mat_sim <- matrix(0, nrow = 9, ncol =2)
mat_sim[,1] <- c(1,2,3,4,5,6,7,8,9)


for (n in 1:1000){
  sim <- c(rl_simpleQ_symmetric_sim(rewards, .5, 3)$agents_total_rewards,
           rl_simpleQ_asymmetric_sim(rewards, .6, .2, 3)$agents_total_rewards,
           rl_simpleQ_asymmetric_sim2(rewards, .6, .2, 3)$agents_total_rewards,
           rl_sym_auto_sim(rewards, .5, 3, .02, 1)$agents_total_rewards,
           rl_asym_auto_sim(rewards, .6, .2, 3, .02, 1)$agents_total_rewards,
           rl_asym_auto_sim2(rewards, .6, .2, 3, .02, 1)$agents_total_rewards,
           rl_ucb_sim_expB(rewards , 0.0, 1000, 16, 16, 1, 1.96)$agents_total_rewards,
           rl_ucb_sim_expB_choiceTrace(rewards , 0.0, 1000, 16, 16, 1,
                                       1.96, 0.02, 1)$agents_total_rewards,
           rl_ucb_sim2(rewards[,-1] , 0.0, 1000, 16, 16, 1.96, .8)$total_rewards
          
           )

  print(n)
  mat_sim[,2] <- c(mat_sim[,2] + sim)
  
}

mat_sim[,2] <- mat_sim[,2]/1000

mat_sim
