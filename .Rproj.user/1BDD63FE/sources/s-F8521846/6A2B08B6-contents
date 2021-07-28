mod_id <- function(seed){
  for (s in seed:seed){
    seed_now <- seeds2[seed]
    for (t in 1:length(kf_envs)){
      print(counter)
      counter <<- counter + 1
      
      
      l <- kf_envs[t]
      innovation_term <<- rs[l,5]
      arm <<- rs[l,3]
      nt <- rs[l,2]
      
      rewards <- reward_generator(rs[l,1], rs[l,2], rs[l,3], rs[l,4], rs[l,5], 
                                  rs[l,6:(5+rs[l,3])])
      
      set.seed(seed_now)
      
      
      par7 <- c(sample_norm_dist(par_df[7,2,,],1), sample_log_dist(par_df[7,3,,],1), 
                sample_log_dist(par_df[7,4,,],1),  sample_log_dist(par_df[7,5,,],1))
      par7 <- append(par7, innovation_term, after = 2)
      
      
      par8 <- c(sample_norm_dist(par_df[8,2,,],1),
                sample_log_dist(par_df[8,3,,],1), sample_log_dist(par_df[8,4,,],1),
                sample_log_dist(par_df[8,5,,],1), sample_log_dist(par_df[8,6,,],1))
      par8 <- append(par8, innovation_term, after = 2)
      
      
      
      par9 <- c(sample_norm_dist(par_df[9,2,,],1),
                sample_log_dist(par_df[9,3,,],1), sample_log_dist(par_df[9,4,,],1),
                sample_log_dist(par_df[9,5,,],1), sample_log_dist(par_df[9,6,,],1))
      par9 <- append(par9, innovation_term, after = 2)
      
      
      par10 <- c(sample_norm_dist(par_df[10,2,,],1),
                 sample_log_dist(par_df[10,3,,],1), sample_log_dist(par_df[10,4,,],1),
                 sample_log_dist(par_df[10,5,,],1), sample_beta_dist(par_df[10,6,,],1),
                 sample_norm_dist(par_df[10,7,,],1))
      par10 <- append(par10, innovation_term, after = 2)
      
      
      par11 <- c(sample_norm_dist(par_df[11,2,,],1),
                 sample_log_dist(par_df[11,3,,],1), sample_log_dist(par_df[11,4,,],1),
                 sample_log_dist(par_df[11,5,,],1), sample_log_dist(par_df[11,6,,],1),
                 sample_beta_dist(par_df[11,7,,],1), sample_norm_dist(par_df[11,8,,],1))
      par11 <- append(par11, innovation_term, after = 2)
      
      par12 <- c(sample_norm_dist(par_df[12,2,,],1),
                 sample_log_dist(par_df[12,3,,],1), sample_log_dist(par_df[12,4,,],1),
                 sample_log_dist(par_df[12,5,,],1), sample_log_dist(par_df[12,6,,],1),
                 sample_beta_dist(par_df[12,7,,],1), sample_norm_dist(par_df[12,8,,],1))
      par12 <- append(par12, innovation_term, after = 2)
      
      
      
      pars <- list(par7, par8, par9, par10, par11, par12)
      
      
      dat <- sim_behav_data_csv(rewards, agent_sims, pars, arm, nt)
      
      est <- c(1,2,3,4,5,6)
      # model 7
      # est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001), c(10 , 100, 2000, 100, 100), ucb_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        mat_value[i, 1, t, s] <- as.vector(unlist(est[i]))
        test <- TRUE
      }
      
      
      # model 8
      # est <- find_the_best_pars(c(0, -100, 0.0001, 0.0001, 0.0001, 0.0001), c(10 , 100, 2000, 100, 100, 100), ucb_asym_rpe_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        x <- (i-1)*nt + 1
        y <- i*nt
        mat_value[i, 2, t, s] <<- as.vector(unlist(est[i]))
      }
      
      
      # model 9
      # est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001, 0.0001), c(10 , 100, 2000, 100, 100, 100), ucb_asym_reward_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        x <- (i-1)*nt + 1
        y <- i*nt
        mat_value[i, 3, t, s] <<- as.vector(unlist(est[i]))
      }
      
      
      # model 10
      # est <- find_the_best_pars(c(0 , -100, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 2000, 100, 100, 1, 100), ucb_auto_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        x <- (i-1)*nt + 1
        y <- i*nt
        mat_value[i, 4, t, s] <<- as.vector(unlist(est[i]))
      }
      
      
      # model 11
      # est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 2000, 100, 100, 100, 1, 100), ucb_asym_rpe_auto_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        x <- (i-1)*nt + 1
        y <- i*nt
        mat_value[i, 5, t, s] <<- as.vector(unlist(est[i]))
      }
      
      
      
      # model 12
      # est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 2000, 100, 100, 100, 1, 100), ucb_asym_reward_auto_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        x <- (i-1)*nt + 1
        y <- i*nt
        mat_value[i, 6, t, s] <<- as.vector(unlist(est[i]))
      }
      
    }
  }
}


mat_value <- array(NA, c(2,6,2,2)) 
nm <- 2
test <- FALSE
mod_id <- function(seed){
  test <<- TRUE
}
mod_id <- function(seed){
  test <<- TRUE
  for (s in seed:seed){
    seed_now <- seeds2[seed]
    for (t in 1:length(2)){

      est <- c(1,2,3,4,5,6)
      # model 7
      # est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001), c(10 , 100, 2000, 100, 100), ucb_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        mat_value[i, 1, t, s] <<- as.vector(unlist(est[i]))
        test <- TRUE
      }
      
      
      # model 8
      # est <- find_the_best_pars(c(0, -100, 0.0001, 0.0001, 0.0001, 0.0001), c(10 , 100, 2000, 100, 100, 100), ucb_asym_rpe_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        mat_value[i, 2, t, s] <<- as.vector(unlist(est[i]))
      }
      
      
      # model 9
      # est <- find_the_best_pars(c(0, -100, 0.00001, 0.0001, 0.0001, 0.0001), c(10 , 100, 2000, 100, 100, 100), ucb_asym_reward_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        mat_value[i, 3, t, s] <<- as.vector(unlist(est[i]))
      }
      
      
      # model 10
      # est <- find_the_best_pars(c(0 , -100, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 2000, 100, 100, 1, 100), ucb_auto_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        mat_value[i, 4, t, s] <<- as.vector(unlist(est[i]))
      }
      
      
      # model 11
      # est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 2000, 100, 100, 100, 1, 100), ucb_asym_rpe_auto_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        mat_value[i, 5, t, s] <<- as.vector(unlist(est[i]))
      }
      
      
      
      # model 12
      # est <- find_the_best_pars(c(0 , -100, 0.00001, 0.00001, 0.0001, 0.0001, 0.0001, -100), c(10 , 100, 2000, 100, 100, 100, 1, 100), ucb_asym_reward_auto_negLogLik_DE, dat, nm, nt)$value
      
      for (i in 1:nm){
        mat_value[i, 6, t, s] <<- as.vector(unlist(est[i]))
      }
      
    }
  }
}



seed_index <- rep(1:2, 1)
mclapply(seed_index, mod_id, mc.preschedule = FALSE, mc.cores = 2)

saveRDS(mat_value, "mat_dist.RDS")
