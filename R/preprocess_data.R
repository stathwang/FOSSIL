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
dat2 <- dat2[usr %in% dat2[, .N, usr][N > 1, usr]]
dat2 <- dat2[prod %in% dat2[, .N, prod][N > 5, prod]]
dat2 <- dat2[usr %in% dat2[, .N, usr][N > 1, usr]]

# Optional: randomly sample 1000 users
# dat2 <- dat2[usr %in% sample(unique(dat2[, usr]), 1000, replace = FALSE)]

# Split the dataset into train, dev, and test set
dat_split <- split_data(dat2, test_prop = 0.01, dev_prop = 0.10, 
                        seed = 321, parallel = TRUE)
train <- dat_split$train
dev <- dat_split$dev
test <- dat_split$test

# Create look-up user and product hashmaps/dictionaries
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

# rm(dat_split)
# rm(dat2)

# If a product in dev or test set does not appear in training, make sure to assign a fixed prod_idx of zero
# and add that prod_idx to product hashmaps. Alternatively here, remove all products that do not appear in training
dev[, usr_idx := usr2idx[[usr]]]
dev[, prod_idx := prod2idx[[prod]]]
dev <- dev[!is.na(prod_idx)]

test[, usr_idx := usr2idx[[usr]]]
test[, prod_idx := prod2idx[[prod]]]
test <- test[!is.na(prod_idx)]

