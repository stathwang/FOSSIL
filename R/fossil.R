library(plyr)
library(data.table)
library(Matrix)
library(doMC)
library(pryr)
library(hashmap)

registerDoMC(detectCores()-1)

# FOSSIL: Factorized Sequential Prediction with Item Similarity Models
# https://arxiv.org/pdf/1609.09152.pdf

# Score either a true or a false product (prod) given user_id, user_prods, and the model parameters
prod_score <- function(user_id, user_prods, prod, 
                       V, H, bias, eta_bias, eta, 
                       alpha, mc_order) {
  
  long_term_dynamics <- (length(user_prods) ^ (-alpha)) * apply(V[user_prods,,drop=FALSE], 2, sum) # vector of length k
  min_order <- min(mc_order, length(user_prods))
  
  rev_user_prods <- rev(user_prods[-(1:(length(user_prods) - min_order))])
  if (min_order == length(user_prods)) rev_user_prods <- rev(user_prods)
  
  if (length(user_id) == 0) {
    short_term_dynamics <- matrix((eta_bias + apply(eta, 2, mean))[1:min_order], nrow = 1) %*% V[rev_user_prods,,drop=FALSE]
  } else {
    short_term_dynamics <- matrix((eta_bias + eta[user_id,])[1:min_order], nrow = 1) %*% V[rev_user_prods,,drop=FALSE]
  }
  
  if (length(prod) != 0) {
    return(bias[prod] + apply((long_term_dynamics + short_term_dynamics) * H[prod,], 1, sum))
  } else {
    return(bias + (long_term_dynamics + short_term_dynamics) %*% t(H))
  }
}

# Train the model parameters
# dat2: a data.table with column names usr, prod (duplicates rows allowed)
fossil <- function(dat2, k_dim = 32, mc_order = 1, 
                   alpha = 0.5, reg = 0, init_sigma = 1,
                   learning_rate = 0.5, learning_rate_decay = 1,
                   maxiters = 100, maxtime = Inf,
                   seed = 123) {
  
  starttime <- Sys.time()
  
  # n_users: total number of users
  # n_prods: total number of products
  n_users <- uniqueN(dat2[,usr])
  n_prods <- uniqueN(dat2[,prod])
  
  # Initialize the model parameters
  set.seed(seed)
  V <- init_sigma * matrix(rnorm(n_prods * k_dim), nrow = n_prods, ncol = k_dim)
  H <- init_sigma * matrix(rnorm(n_prods * k_dim), nrow = n_prods, ncol = k_dim)
  eta <- init_sigma * matrix(rnorm(n_users * mc_order), nrow = n_users, ncol = mc_order)
  eta_bias <- vector('numeric', length = mc_order)
  bias <- vector('numeric', length = n_prods)
  
  # Training starts here...
  iters <- 1
  avg_cost <- vector('numeric', maxiters)
  current_absolute_cost <- vector('numeric', maxiters)
  current_delta <- vector('numeric', maxiters)
  #avg_recall <- vector('numeric', maxiters)
  #avg_sps <- vector('numeric', maxiters)
  #avg_uc <- vector('numeric', maxiters)
  #avg_bbs <- vector('numeric', maxiters)

  while (iters <= maxiters && Sys.time() - starttime < 60*maxtime) {
    
    # Pick a random training sample: (user_id, true_prod, false_prod)
    #cat('Picking a random training sample\n')
    user <- sample(all_users, 1)
    user_id <- usr2idx[[user]]
    user_prod_ids <- dat2[usr == user, prod_idx]
    
    rand <- sample(1:length(user_prod_ids), 1)
    
    false_prod <- sample(1:n_prods, 1)
    #false_prod <- sample(unique(dat2[,prod_idx]), 1)
    while (false_prod %in% user_prod_ids[1:rand]) {
      false_prod <- sample(1:n_prods, 1)
      #false_prod <- sample(unique(dat2[,prod_idx]), 1)
    }
    
    user_prods <- user_prod_ids[1:rand]
    
    # Learning rate decay
    if (iters %% 10 == 0) {
      learning_rate <- learning_rate * learning_rate_decay
    }
    
    # Update model parameters using stochastic gradient descent one training sample at a time
    true_prod <- user_prods[length(user_prods)]
    user_prods <- user_prods[1:(length(user_prods)-1)]
    min_order <- min(mc_order, length(user_prods))
    
    long_term_dynamics <- (length(user_prods) ^ (-alpha)) * apply(V[user_prods,,drop=FALSE], 2, sum)
    
    rev_user_prods <- rev(user_prods[-(1:(length(user_prods) - min_order))])
    if (min_order == length(user_prods)) rev_user_prods <- rev(user_prods)
    
    short_term_dynamics <- matrix((eta_bias + eta[user_id,])[1:min_order], nrow = 1) %*% V[rev_user_prods,,drop=FALSE]
    
    # Compute absolute error and delta (sigmoid of error)
    #cat('Computing the error\n')
    x_true <- prod_score(user_id, user_prods, true_prod, V, H, bias, eta_bias, eta, alpha, mc_order)
    x_false <- prod_score(user_id, user_prods, false_prod, V, H, bias, eta_bias, eta, alpha, mc_order)
    absolute_error <- x_false - x_true
    delta <- 1 / (1 + exp(-min(10, max(-10, absolute_error))))
    
    # Compute the updates
    # long_term_dynamics + short_term_dynamics = personalized weighting factor
    V_update <- learning_rate * (delta * (length(user_prods) ^ (-alpha)) * (H[true_prod,,drop=FALSE] - H[false_prod,,drop=FALSE])[rep(1, length(user_prods)),] - reg * V[user_prods,,drop=FALSE]) # matrix
    V_update2 <- learning_rate * delta * outer((eta_bias + eta[user_id,])[1:min_order], H[true_prod,] - H[false_prod,]) # matrix
    H_true_up <- learning_rate * (delta * (long_term_dynamics + short_term_dynamics) - reg * H[true_prod,])
    H_false_up <- learning_rate * (-delta * (long_term_dynamics + short_term_dynamics) - reg * H[false_prod,])
    bias_true_up <- learning_rate * (delta - reg * bias[true_prod])
    bias_false_up <- learning_rate * (-delta - reg * bias[false_prod])
    eta_bias_up <- learning_rate * (delta * apply((V[rev_user_prods,,drop=FALSE] * (H[true_prod,,drop=FALSE] - H[false_prod,,drop=FALSE])[rep(1, length(rev_user_prods)),]), 1, sum) - reg * eta_bias[1:min_order])
    eta_up <- learning_rate * (delta * apply((V[rev_user_prods,,drop=FALSE] * (H[true_prod,,drop=FALSE] - H[false_prod,,drop=FALSE])[rep(1, length(rev_user_prods)),]), 1, sum) - reg * eta[user_id, 1:min_order])
    
    # Update the model parameters
    #cat('Updating the model parameters\n')
    V[user_prods,] <- V[user_prods,] + V_update
    V[rev_user_prods,] <- V[rev_user_prods,] + V_update2
    H[true_prod,] <- H[true_prod,] + H_true_up
    H[false_prod,] <- H[false_prod] + H_false_up
    bias[true_prod] <- bias[true_prod] + bias_true_up
    bias[false_prod] <- bias[false_prod] + bias_false_up
    eta_bias[1:min_order] <- eta_bias[1:min_order] + eta_bias_up
    eta[user_id, 1:min_order] <- eta[user_id, 1:min_order] + eta_up
    
    current_absolute_cost[iters] <- absolute_error
    current_delta[iters] <- delta
    avg_cost[iters] <- sum(c(current_absolute_cost[1:iters], absolute_error)) / iters
    
    cat('User: ', user, '\n')
    cat('User ID: ', user_id, '\n')
    cat('True Product ID: ', true_prod, '\n')
    cat('False Product ID: ', false_prod, '\n')
    cat('Iteration: ', iters, '\n')
    cat('Average Error: ', avg_cost[iters], '\n')
    cat('Time Progressed: ', Sys.time() - starttime, '\n')
    cat('\n\n')
    
    iters <- iters + 1
  }
  
  return(list(avg_cost = avg_cost,
              current_absolute_cost = current_absolute_cost,
              current_delta = current_delta,
              V = V, 
              H = H, 
              bias = bias, 
              eta_bias = eta_bias,
              eta = eta,
              alpha = alpha,
              mc_order = mc_order))
}

# Recommend top k products for every user
# users: a vector of users to recommend products
top_k <- function(dat2, users, V, H, bias, eta_bias, eta, alpha, mc_order, 
                  top_k = 10, excluded_prods = NULL, parallel = FALSE) {
  
  dat_trunc <- dat2[usr %in% users]
  dat_trunc <- split(dat_trunc, by = 'usr')
  recommend <- llply(dat_trunc, function(x) {
    user <- unique(x[,usr])
    uid <- unique(x[,usr_idx])
    viewed_prod_ids <- x[,prod_idx]
    scores <- prod_score(user_id = uid, user_prods = viewed_prod_ids, prod = NULL, 
                         V = V, H = H, bias = bias, 
                         eta_bias = eta_bias, eta = eta, 
                         alpha = alpha, mc_order = mc_order)
    #scores[,viewed_prod_ids] <- -Inf
    if (length(excluded_prods) != 0) {
      excluded_prod_ids <- prod2idx[[excluded_prods]]
      scores[,excluded_prod_ids] <- -Inf
    }
    ranked_prod_ids <- order(scores, decreasing = TRUE)[1:top_k]
    output <- data.table(usr = user,
                         usr_idx = uid,
                         prod = idx2prod[[ranked_prod_ids]],
                         prod_idx = ranked_prod_ids,
                         score = scores[ranked_prod_ids])
    return(output)
  }, .parallel = parallel)
  return(rbindlist(recommend))
}

# Split data into train, dev, test
# for every user, put the last 5 percent of products in test
# and the next last 15 percent of remaining products in dev
split_data <- function(dat2, test_prop = 0.05, dev_prop = 0.15, seed = 123, parallel = FALSE) {
  set.seed(seed)
  dat_split <- llply(unique(dat2[,usr]), function(u) {
    temp <- dat2[usr == u]
    test_idx <- tail(seq_len(temp[,.N]), floor(temp[,.N] * test_prop))
    dev_idx <- tail(setdiff(seq_len(temp[,.N]), test_idx), floor(length(setdiff(seq_len(temp[,.N]), test_idx)) * dev_prop / (1-test_prop)))
    return(list(test = temp[test_idx], dev = temp[dev_idx], train = temp[setdiff(seq_len(temp[,.N]), union(test_idx, dev_idx))]))
  }, .parallel = parallel)
  
  test <- rbindlist(llply(dat_split, function(u) u$test))
  dev <- rbindlist(llply(dat_split, function(u) u$dev))
  train <- rbindlist(llply(dat_split, function(u) u$train))
  return(list(train = rbindlist(llply(dat_split, function(x) x$dev), 
              dev = rbindlist(llply(dat_split, function(x) x$dev), 
              test = rbindlist(llply(dat_split, function(x) x$test)
}

# Metrics to evaluate on a devset for every epoch in training:
# test: either a dev or a test set
# recall, ndcg, sps, user coverage, item coverage, blockbuster share
evaluate <- function(rec, test, excluded_prods = NULL, parallel = FALSE) {
  eval_metrics <- llply(unique(test[,usr]), function(u) {
    test_prod_ids <- test[usr == u, prod_idx]
    rec_prod_ids <- rec[usr == u, prod_idx]
    correct <- intersect(rec_prod_ids, test_prod_ids)
    recall <- length(correct) / length(unique(test_prod_ids))
    sps <- 1 * (test_prod_ids[1] %in% correct)
    uc <- 1 * (recall > 0)
    bbs <- ifelse(length(correct) == 0, 0, length(intersect(correct, pop_prods[,prod_idx])) / length(correct))
    return(list(user = u, 
                recall = recall, 
                num_correct = length(correct),
                num_test = length(unique(test_prod_ids)),
                sps = sps, 
                user_coverage = uc,
                blockbusters_share = bbs))
  }, .parallel = parallel)
  eval_metrics <- rbindlist(eval_metrics)
  avg_recall <- mean(eval_metrics$recall)
  avg_sps <- mean(eval_metrics$sps)
  avg_uc <- mean(eval_metrics$user_coverage)
  avg_bbs <- mean(eval_metrics$blockbusters_share)
  return(list(avg_recall = avg_recall,
              avg_sps = avg_sps,
              avg_uc = avg_uc,
              avg_bbs = avg_bbs,
              eval_metrics = eval_metrics))
}

