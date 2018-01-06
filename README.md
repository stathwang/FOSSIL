# FOSSIL
Fusing Similarity Models with Markov Chains for Sparse Sequential Recommendation (He and McAuley, 2016)
https://arxiv.org/pdf/1609.09152.pdf

This repository contains my implementation of FOSSIL in R that predicts a user's next purchase. The algorithm uses a similarity-based method to reduce sparsity in modeling user preferences as well as higher order Markov chains to consider dynamic user preferences across multiple time steps.

FOSSIL models both user preference (long term) and sequential dynamics (short term). Previously, there have been two main attempts to model sequential dynamics:

- Factorized Markov Chains (FMC) that factorizes the item-item transition matrix that fails to give personalized recommendations
- Factorized Personalized Markov Chains (FPMC) that 
