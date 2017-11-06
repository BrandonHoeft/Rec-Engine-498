library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(pryr) # memory usage functions
library(Matrix)
library(recommenderlab)
library(recosystem)
library(devtools)
library(SlopeOne)
library(SVDApproximation)
library(data.table)
library(RColorBrewer)


colTypes = c("character", "character", "integer", "character",  
  "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer",
  "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer",
  "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer",
  "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer")

user_ratings = read.csv("/Users/haydude/Development/mspa/498 - Capstone/data/webActivityRated.csv", header = TRUE)
items = read.csv("/Users/haydude/Development/mspa/498 - Capstone/data/items.csv", header = TRUE)

mcmratings = data_frame(user=user_ratings$VisitorId, item=user_ratings$Parent, rating=user_ratings$WeightedRating)
mcmratings$item = as.numeric(mcmratings$item)
mcmratings$user = as.numeric(as.factor(mcmratings$user))
str(mcmratings)
visualize_ratings(ratings_table = mcmratings)


 
sparse_mcm = sparseMatrix(i = as.integer(as.factor(user_ratings$VisitorId)), 
                          j = as.integer(as.factor(user_ratings$Parent)),
                          x = user_ratings$WeightedRating)



plot(density(user_ratings$AnyRating))
plot(density(user_ratings$WeightedRating))

ggplot(user_ratings, aes(x=AnyRating)) + geom_density() + xlim(0,10)

sparse_r <- sparseMatrix(i = as.integer(as.factor(user_ratings$VisitorId)), 
                         j = as.integer(as.factor(user_ratings$Parent)),
                         x = user_ratings$WeightedRating)

# can rename the matrix row and column labels with unique VisitorId and Parent names. 
dimnames(sparse_r) <- list(sort(unique(user_ratings$VisitorId)),
                           sort(unique(user_ratings$Parent)))

methods(class = "realRatingMatrix")


real_r <- as(sparse_r, "realRatingMatrix")
real_r

real_r_non_missing_percent <- round(sum(rowCounts(real_r)) / (nrow(real_r) * ncol(real_r)), 5) * 100
real_r_non_missing_percent

getRatingMatrix(real_r[1:100, 1:10])

# define parameters for model evaluation scheme.
?evaluationScheme
train_proportion <- .80
use_n_test_records_per_user <- 5 # use 5 records per test user to learn model.
good_rating <- 2.5 # a good rating threshold for a binary classifier?

#real_r[rowCounts(real_r) > 7, ]

# Create the evaluation scheme for fitting and testing the recommender algorithm.
set.seed(123)
model_train_scheme <- real_r[rowCounts(real_r) >= use_n_test_records_per_user, ] %>%
  evaluationScheme(method = 'split', # single train/test partition
                   train = train_proportion, # random sample proportion.
                   given = use_n_test_records_per_user, 
                   goodRating = good_rating,
                   k = 1)

# Fit a UBCF recommender system algorithm. 
# Building a Recommender System with R by Gorakala and Usuelli. Ch.4 pp 84
model_params <- list(method = "cosine",
                     nn = 25, # find each user's 25 most similar users.
                     sample = FALSE, # already did this.
                     # centered cosine similarity > raw cosine similarity.
                     # https://youtu.be/h9gpufJFF-0?list=PLPEu1YFt_zElULdcZBwiDOtOiNLXErWos&t=332
                     normalize = "center")

# Model1: centered cosine similarity.
model1 <- getData(model_train_scheme, "train") %>% 
  Recommender(method = "UBCF", parameter = model_params)

model1_pred <- predict(model1, getData(model_train_scheme, "known"), type = "ratings")


