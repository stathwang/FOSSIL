# FOSSIL
Fusing Similarity Models with Markov Chains for Sparse Sequential Recommendation (He and McAuley, 2016)
https://arxiv.org/pdf/1609.09152.pdf

This repository contains my implementation of FOSSIL in R that predicts a user's next purchase based on the past purchase history. The algorithm uses a similarity-based method to reduce sparsity in modeling user preferences as well as higher order Markov chains to smooth user preferences across multiple time steps.

FOSSIL models both long-term **user preference** (matrix factorization) and short-term **sequential dynamics** (markov chains). Previously, there had been several attempts to model sequential dynamics and subsequently combine it with matrix factorization:

- Factorized Markov Chains (FMC) that factorizes the item-item transition matrix (non-personalized)
- Tensor Factorization (TF; Rendle et al. 2008) where a dataset is represented by a tensor and its entries capture the likelihood that users transition from one item to another
- Factorized Personalized Markov Chains (FPMC; Rendle et al. 2010) that factorizes the user-item matrix and considers Markov chains of first order.

FOSSIL is similar in spirit to FPMC but takes an inspiration from FISM (Factored Item Similarity Models for Top-N Recommender Systems; Kabbur et al. 2013) to factorize item-item matrix in order to reduce sparsity by not having to explicitly model users and relaxes the first order Markov chain assumption of FPMC to higher orders.

Since here we are interested in top-k recommendation (typically k = 10), the algorithm minimizes the S-BPR (Sequential Bayesian Personalized Ranking) loss using stochastic gradient descent, one training sample at a time, to optimize the model parameters. S-BPR uses a sigmoid function to characterize the probability that a true item is ranked higher than a false item given a user and the model parameters, assuming independence of users and time steps.

Run the *preprocess.R* script on *movielens_trunc.csv* file and then the *fossil.R* on the training set produced. I'm going to upload the python version of the algorithm as well as functions for evaluation metrics soon, so stay tuned.

