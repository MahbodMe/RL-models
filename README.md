# MSc Thesis project

### 12 RL models

| Models | Learning Rate | Autocorrlation | Decision Rule | Model name in code | Number of Free Parameters | 
|---------------|-----------|-----------|-----------|-----------|-----------|
|Model 1| Q_learning (Symmetric) | NA | SoftMax | sym_sim()| 2 |
|Model 2| Q_learning (Asymmetric-RPE) | NA | SoftMax | asym_rpe_sim()| 3 |
|Model 3| Q_learning (Asymmetric-Reward) | NA | SoftMax | asym_reward_sim()| 3 |
|Model 4| Q_learning (Symmetric) | Choice Trace | SoftMax | sym_auto_sim()| 4 |
|Model 5| Q_learning (Asymmetric-Reward) | Choice Trace | SoftMax | asym_rpe_auto_sim()| 5 |
|Model 6| Q_learning (Asymmetric-Reward) |Choice TraceNA | SoftMax | asym_reward_auto_sim()| 5 |
|Model 7| Kalman filter (Symmetric) | NA | UCB + Exploration Bonus | ucb_sim()| 5 |
|Model 8| Kalman filter (Asymmetric-RPE) | NA | UCB + Exploration Bonus | ucb_asym_rpe_sim()| 6 |
|Model 9| Kalman filter (Asymmetric-Reward) | NA | UCB + Exploration Bonus | ucb_asym_reward_sim()| 6 |
|Model 10| Kalman filter (Symmetric) | Choice Trace | UCB + Exploration Bonus | ucb_sim()| 7 |
|Model 11| Kalman filter (Asymmetric-RPE) | Choice Trace | UCB + Exploration Bonus | ucb_asym_rpe_sim()| 8 |
|Model 12| Kalman filter (Asymmetric-Reward) | Choice Trace | UCB + Exploration Bonus | ucb_asym_reward_sim()| 8 |



