library(plyr)
library(data.table)
library(Matrix)
library(doMC)
library(pryr)
library(hashmap)

options(scipen = 15)

# Load the dataset: MovieLens 1M
dat <- fread("data/movielens_trunc.csv")
dat2 <- dat[, .(usr, prod)]

# Remove users with low rating activity and rare items
set.seed(321)
min_usr <- 1
min_prod <- 5
dat2 <- dat2[usr %in% dat2[, .N, usr][N > min_usr, usr]]
dat2 <- dat2[prod %in% dat2[, .N, prod][N > min_prod, prod]]
dat2 <- dat2[usr %in% dat2[, .N, usr][N > min_usr, usr]]

# Optional: randomly sample 1000 users
# dat2 <- dat2[usr %in% sample(unique(dat2[, usr]), 1000, replace = FALSE)]

# Split data into train, dev, test
# for every user, put the last 5 percent of products in test
# and the next last 15 percent of remaining products in dev
split_data <- function(dat2, test_prop = 0.05, dev_prop = 0.15, seed = 123, parallel = FALSE) {
  set.seed(seed)
  dat_trunc <- split(dat2, by = 'usr')
  dat_split <- llply(dat_trunc, function(x) {
    n <- x[,.N]
    test_idx <- tail(seq_len(n), floor(n*test_prop))
    dev_idx <- tail(setdiff(seq_len(n), test_idx), floor(length(setdiff(seq_len(n), test_idx)) * dev_prop / (1-test_prop)))
    return(list(test = x[test_idx],
                dev = x[dev_idx],
                train = x[setdiff(seq_len(n), c(dev_idx, test_idx))]))
  }, .parallel = parallel)
  return(dat_split)
}

dat_split <- split_data(dat2, test_prop = 0.05, dev_prop = 0.15, seed = 321, parallel = TRUE)

train <- rbindlist(llply(dat_split, function(x) x$train))
dev <- rbindlist(llply(dat_split, function(x) x$dev))
test <- rbindlist(llply(dat_split, function(x) x$test))

# rm(dat_split)
# rm(dat2)

# Look-up hashmaps for users and products
all_prods <- unique(train[,prod])
prod2idx <- hashmap(key = all_prods, values = 1:length(all_prods))
idx2prod <- hashmap(key = 1:length(all_prods), values = all_prods)

all_users <- unique(train[,usr])
usr2idx <- hashmap(key = all_users, values = 1:length(all_users))
idx2usr <- hashmap(key = 1:length(all_users), values = all_users)

train[, usr_idx := usr2idx[[usr]]]
train[, prod_idx := prod2idx[[prod]]]

# Optional: train should now contain 4 columns usr, prod, usr_idx, prod_idx
# head(train)

# Retrieve the top 1% of products as popular products
pop_prods <- train[, .N, by = .(prod, prod_idx)][order(-N)][1:floor(.N * 0.01)]

# If a product in dev or test set does not appear in training, make sure to assign a fixed prod_idx of zero
# and add that prod_idx to product hashmaps. Alternatively here, remove all products that do not appear in training
dev[, usr_idx := usr2idx[[usr]]]
dev[, prod_idx := prod2idx[[prod]]]
dev <- dev[!is.na(prod_idx)]

test[, usr_idx := usr2idx[[usr]]]
test[, prod_idx := prod2idx[[prod]]]
test <- test[!is.na(prod_idx)]
