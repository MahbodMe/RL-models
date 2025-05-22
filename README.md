# MSc Thesis Project: Asymmetric Belief Updating in Reinforcement Learning

This repository contains the full codebase and manuscript for my MSc thesis, completed at University College London under the supervision of Maarten Speekenbrink. The project investigates whether observed learning biases in instrumental tasks reflect real asymmetries in belief updating or arise from modeling artifacts, such as initial expectations or decision autocorrelation.

## Key Contributions

* Simulated and fit **12 reinforcement learning models** across Q-learning and Kalman filter families.
* Evaluated models using AIC, BIC, likelihood ratio tests, and binomial comparisons.
* Used the **DEoptim** package for global parameter estimation across all models.
* Analyzed **multiple open behavioral datasets**, including those used in prior studies on learning biases.
* Demonstrated that several observed asymmetries in past literature can be accounted for by **expectation priors** and **autocorrelated choice strategies**, rather than true asymmetric learning.

## RL Models Implemented

| Model | Learning Rate                     | Autocorrelation | Decision Rule | Function Name             | Free Parameters |
| ----- | --------------------------------- | --------------- | ------------- | ------------------------- | --------------- |
| 1     | Q-learning (Symmetric)            | –               | Softmax       | sym\_sim()                | 2               |
| 2     | Q-learning (Asymmetric-RPE)       | –               | Softmax       | asym\_rpe\_sim()          | 3               |
| 3     | Q-learning (Asymmetric-Reward)    | –               | Softmax       | asym\_reward\_sim()       | 3               |
| 4     | Q-learning (Symmetric)            | Choice Trace    | Softmax       | sym\_auto\_sim()          | 4               |
| 5     | Q-learning (Asymmetric-RPE)       | Choice Trace    | Softmax       | asym\_rpe\_auto\_sim()    | 5               |
| 6     | Q-learning (Asymmetric-Reward)    | Choice Trace    | Softmax       | asym\_reward\_auto\_sim() | 5               |
| 7     | Kalman Filter (Symmetric)         | –               | UCB + Bonus   | ucb\_sim()                | 5               |
| 8     | Kalman Filter (Asymmetric-RPE)    | –               | UCB + Bonus   | ucb\_asym\_rpe\_sim()     | 6               |
| 9     | Kalman Filter (Asymmetric-Reward) | –               | UCB + Bonus   | ucb\_asym\_reward\_sim()  | 6               |
| 10    | Kalman Filter (Symmetric)         | Choice Trace    | UCB + Bonus   | ucb\_sim()                | 7               |
| 11    | Kalman Filter (Asymmetric-RPE)    | Choice Trace    | UCB + Bonus   | ucb\_asym\_rpe\_sim()     | 8               |
| 12    | Kalman Filter (Asymmetric-Reward) | Choice Trace    | UCB + Bonus   | ucb\_asym\_reward\_sim()  | 8               |

Model code is implemented across `Q_learning_models.R`, `Kf_UCB_models.R`, and parameter estimation functions in `par_est_Q_learning.R` and `par_est_UCB.R` using **DEoptim**.

## Data Simulation

Simulation functions generate behavior from model predictions using custom reward environments. Example environment and behavioral data generation functions are provided.

## Key Findings

* **Asymmetries** observed in human data can often be **explained by initialization or autocorrelation**.
* The best-fitting models across datasets were often symmetric once expectation priors were appropriately accounted for.
* Model recovery analysis showed significant **overlap between models**, supporting a **more cautious interpretation** of asymmetric learning claims.

## Repository Contents

* `MSc_thesis.pdf`: Full manuscript
* `Q_learning_models.R`, `Kf_UCB_models.R`: Agent simulations
* `par_est_Q_learning.R`, `par_est_UCB.R`: DEoptim parameter estimation
* `figures/`, `results/`, `analysis/`: Evaluation outputs and comparison metrics
* `data/`: Behavioral datasets and simulated environments

## Citation

If using this work for reference or teaching:

> Mehrvarz, M. (2020). *Asymmetric Belief Updating in Instrumental Learning*. MSc Thesis, University College London. Supervised by Maarten Speekenbrink.

---

This project is made available for academic and educational use only. No publication was submitted, but the modeling infrastructure may be useful to others.
