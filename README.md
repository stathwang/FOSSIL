# FOSSIL
Fusing Similarity Models with Markov Chains for Sparse Sequential Recommendation (He and McAuley, 2016)
https://arxiv.org/pdf/1609.09152.pdf

This repository contains my implementation of FOSSIL in R that predicts a user's next purchase. The algorithm uses a similarity-based method to reduce sparsity in modeling user preferences and considers higher order Markov chains to smooth user preferences across multiple time steps.

A typical recommender system models one or more of the following:
- user preference (long term)
- sequential dynamics (short term)
- geography
