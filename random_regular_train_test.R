library(aws.s3) 
library(readr) # faster alternatives to base read. methods. 
library(lubridate) # date wrangling. 
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # data viz. 
library(pryr) # mem_used() and object_size() functions to manage/understand memory usage.


# specify keys as environment variables so I can read my s3 object(s) from AWS.
# Your unique access key/secret needs to be passed before running the queries below. 
#Sys.setenv("AWS_ACCESS_KEY_ID" = "",
#           "AWS_SECRET_ACCESS_KEY" = "")


# Import Data and wrangle it ---------------------------------------------------

# items: part number, parent, catalogue, attributes/values.
items <- s3read_using(FUN = read_table2, 
                      object = "obfuscatedItems_10_17_17.txt", 
                      col_names = TRUE,
                      col_types = "cciciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii",
                      bucket = "pred498team5")

# web user click data from Feb - Mar 2017
users_febmar17 <- s3read_using(FUN = read_csv, 
                               col_names = TRUE,
                               col_types = "ccici",
                               object = "obfuscatedWebActivity7124.csv", 
                               bucket = "pred498team5")

# web user click data from April - June 2017
users_aprmayjun17 <- s3read_using(FUN = read_csv, 
                                  col_names = TRUE,
                                  col_types = "ccici",
                                  object = "obfuscatedWebActivity7127.csv", 
                                  bucket = "pred498team5")

# web user click data from July - August 2017
users_julaug17 <- s3read_using(FUN = read_csv, 
                               col_names = TRUE,
                               col_types = "ccici",
                               object = "obfuscatedWebActivity7129.csv", 
                               bucket = "pred498team5")


users <- bind_rows(users_febmar17, users_aprmayjun17, users_julaug17)

user_items <- users %>%
  left_join(items, by = "PartNumber") %>%
  mutate(ActionDate = ymd(ActionDate), # parse into a date format.
         ActionId_label = factor(ActionId, # create labels
                                 labels = c("add to order", "select Part",
                                            "select Part detail",
                                            "print detail",
                                            "save CAD drawing detail",
                                            "print CAD drawing detail"))) %>% 
  select(-starts_with("Val"), -starts_with("Attr"), -starts_with("Catalog"))

remove(users, items, users_febmar17, users_aprmayjun17, users_julaug17)

user_items <- user_items %>%
  mutate(action_rating = if_else(ActionId_label == "select Part", 1, 
                                 if_else(ActionId_label == "select Part detail", 2, 3)))


# Pre-process the ratings scheme -----------------------------------------------

user_ratings <- user_items %>%
  select(VisitorId, Parent, ActionDate, action_rating) %>%
  # create group window for each Visitor's individual session(day) with a Parent# 
  group_by(VisitorId, Parent, ActionDate) %>%
  # Step1: w/in window, keep row(s) of max action_rating for that specific session.
  filter(action_rating == max(action_rating)) %>%
  # Step2: sum up all max action_ratings to account for >1 max actions with diff. 
  # PartNumbers of that Parent# during session. An additive weight w/in session of a Parent#.
  summarize(session_action_rating = sum(action_rating)) %>%
  # change group window to roll up next aggregations to Visitor:Parent, across all sessions. 
  group_by(VisitorId, Parent) %>%
  # Step3: per Parent per Visitor, sum the session_action_rating across all sessions (days).
  summarize(total_rating = sum(session_action_rating)) %>%
  ungroup() %>%
  # Step4: ratings transformation candidates
  mutate(total_rating_sqrt = sqrt(total_rating),
         total_rating_logn = log(total_rating),
         total_rating_max10 = ifelse(total_rating > 10, 10, total_rating)) %>%
  arrange(VisitorId, total_rating)


# Convert User-Item ratings into a sparse matrix object ------------------------

library(Matrix)
# sparse ratings matrix.
# https://stackoverflow.com/questions/28430674/create-sparse-matrix-from-data-frame?noredirect=1&lq=1
# the VisitorId and Parent need to be 1 based indices when creating a matrix. 
# Per ?factor, the levels of a factor are by default sorted.
sparse_r <- sparseMatrix(i = as.integer(as.factor(user_ratings$VisitorId)), 
                         j = as.integer(as.factor(user_ratings$Parent)),
                         x = user_ratings$total_rating_max10)

# can rename the matrix row and column labels with unique VisitorId and Parent names. 
dimnames(sparse_r) <- list(sort(unique(user_ratings$VisitorId)),
                           sort(unique(user_ratings$Parent)))

all.equal(levels(as.factor(user_ratings$VisitorId)), 
          sort(unique(user_ratings$VisitorId))) 

all.equal(levels(as.factor(user_ratings$Parent)), 
          sort(unique(user_ratings$Parent))) 


# RecommenderLab ---------------------------------------------------------------
library(recommenderlab)


# coerce our sparseMatrix, sparse_r into a realRatingMatrix object
real_r <- as(sparse_r, "realRatingMatrix")
real_r

# How many ratings are there per user
summary(rowCounts(real_r))
# How many visitors have rated each item?
summary(colCounts(real_r))


# Random Sample 33% of the users.
set.seed(2017)
keep_index <- sample(seq_len(nrow(real_r)), 
                     size = nrow(real_r) * 0.33, replace = FALSE)
real_r_sampled_rows <- real_r[keep_index, ]
real_r_sampled_rows

# Random Sample 75% of the parent items. 
set.seed(2017)
keep_col_index <- sample(seq_len(ncol(real_r)), 
                         size = ncol(real_r) * 0.75, replace = FALSE)
real_r_sampled_row_cols <- real_r_sampled_rows[, keep_col_index]
real_r_sampled_row_cols

# Updated row and column count summaries
summary(rowCounts(real_r_sampled_row_cols))
summary(colCounts(real_r_sampled_row_cols))
summary(rowMeans(real_r_sampled_row_cols))
summary(colMeans(real_r_sampled_row_cols))




# Keep users with a high number of ratings. Median or Above.
real_r_filtered_rows <- real_r_sampled_row_cols[rowCounts(real_r_sampled_row_cols) >= 50, ] 
nrow(real_r_filtered_rows) - nrow(real_r)
real_r_filtered_rows
# Updated row and column count summaries
summary(rowCounts(real_r_filtered_rows))
summary(colCounts(real_r_filtered_rows))

# Keep Parent items with a high number of ratings, > 1st Quartile.
real_r_filtered_rows_cols <- real_r_filtered_rows[, colCounts(real_r_filtered_rows) > 5]

# Updated row and column count summaries
summary(rowCounts(real_r_filtered_rows_cols))
summary(colCounts(real_r_filtered_rows_cols))
summary(rowMeans(real_r_filtered_rows_cols))
summary(colMeans(real_r_filtered_rows_cols))
real_r_filtered_rows_cols


# Design a Model Evaluation Scheme ---------------------------------------------
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

set.seed(2017)
train_scheme <- evaluationScheme(real_r_filtered_rows_cols,
                                 method = "split", # random train/test scheme
                                 train = 0.75,
                                 k = 1,
                                 given = 5,  # how many records for a test user will learn the model? 
                                 goodRating = 3) # threshold for classification. Just above Median Total_rating_max10.
train_scheme


# Build single best  UBCF recommender ------------------------------------------
random_model <- Recommender(getData(train_scheme, "train"), method = "RANDOM")

random_predictions <- predict(random_model, 
                            getData(train_scheme, "known"), 
                            type = "topNList", 
                            n = 20)

random_accuracy_by_user <- calcPredictionAccuracy(random_predictions,
                                                getData(train_scheme, "unknown"),
                                                given = 5,
                                                goodRating = 3,
                                                byUser = TRUE)

random_accuracy_by_user <- as.data.frame(random_accuracy_by_user) %>%
  mutate(VisitorId = row.names(.)) %>%
  select(VisitorId, everything())

fn <- sum(random_accuracy_by_user$FN)
tn <- sum(random_accuracy_by_user$TN)
tp <- sum(random_accuracy_by_user$TP)
fp <- sum(random_accuracy_by_user$FP)


#         TP * TN - FP * FN
# MCC = -----------------------------------------------------
#        [(TP + FP) * (FN + TN) * (FP + TN) * (TP + FN)]^(1/2)

mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (fn + tn) * (fp + tn) * (tp + fn))

summary(random_accuracy_by_user$TP)
calcPredictionAccuracy(random_predictions,
                       getData(train_scheme, "unknown"),
                       given = 5,
                       goodRating = 3,
                       byUser = FALSE)


random_accuracy_performance <- random_accuracy_by_user %>%
  select(VisitorId, TP, precision) %>%
  rename(test_precision = precision)

s3save(random_accuracy_performance, bucket = "pred498finalmodel", object = "random_test_recommendations_performance.Rdata")
#s3load("random_test_recommendations_performance.Rdata", bucket = "pred498finalmodel")


s3load("ubcf_test_recommendations_performance.Rdata", bucket = "pred498finalmodel")
