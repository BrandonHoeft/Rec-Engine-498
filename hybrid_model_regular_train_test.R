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




# Build a Hybrid Recommender System of UBCF, IBCF, popular, re-recommended ------

hybrid_rec_model <- HybridRecommender(
  Recommender(getData(train_scheme, "train"), 
              method = "UBCF", 
              parameter = list(method = "cosine", 
                               nn = 50,
                               normalize = "center")),
  Recommender(getData(train_scheme, "train"), 
              method = "IBCF",
              parameter = list(method = "pearson", 
                               k = 350,
                               normalize = "center")),
  Recommender(getData(train_scheme, "train"), method = "POPULAR"),
  Recommender(getData(train_scheme, "train"), method = "RERECOMMEND"),
  weights = c(.3, .3, .1, .3)
)
#s3save(hybrid_rec_model, bucket = "pred498finalmodel", object = "hybrid_rec_model.Rdata")
#s3load("hybrid_rec_model.Rdata", bucket = "pred498finalmodel")

# Fit testing records.
hybrid_predictions <- predict(hybrid_rec_model, 
                             getData(train_scheme, "known"), 
                             type = "topNList", 
                             n = 20)
#s3save(hybrid_predictions, bucket = "pred498finalmodel", object = "hybrid_predictions.Rdata")
#s3load("hybrid_predictions.Rdata", bucket = "pred498finalmodel")
str(hybrid_predictions)
hybrid_accuracy_by_user <- calcPredictionAccuracy(hybrid_predictions,
                                                  getData(train_scheme, "unknown"),
                                                  given = 5,
                                                  goodRating = 3,
                                                  byUser = TRUE)

hybrid_accuracy_by_user <- as.data.frame(hybrid_accuracy_by_user) %>%
  mutate(VisitorId = row.names(.)) %>%
  select(VisitorId, everything())

summary(hybrid_accuracy_by_user$TP)

calcPredictionAccuracy(hybrid_predictions,
                       getData(train_scheme, "unknown"),
                       given = 5,
                       goodRating = 3,
                       byUser = FALSE)



# Build a hybrid UBCF recommender with a implicit rating latent factor model (BEST) ---------

hybrid_rec_model2 <- HybridRecommender(
  Recommender(getData(train_scheme, "train"), 
              method = "UBCF", 
              parameter = list(method = "cosine", 
                               nn = 50,
                               normalize = "center")),
  Recommender(getData(train_scheme, "train"), 
              method = "ALS_implicit",
              parameter = list(lambda = 0.1,
                               alpha = 10,
                               n_factors = 100,
                               n_iterations = 10,
                               min_item_nr = 1,
                               seed = 2017)), # default parameters
  weights = c(.5, .5)
)


# Fit testing records.
hybrid2_predictions <- predict(hybrid_rec_model2, 
                              getData(train_scheme, "known"), 
                              type = "topNList", 
                              n = 20)
s3save(hybrid2_predictions, bucket = "pred498finalmodel", object = "hybrid2_predictions.Rdata")
#s3load("hybrid2_predictions.Rdata", bucket = "pred498finalmodel")

str(hybrid2_predictions)
hybrid2_accuracy_by_user <- calcPredictionAccuracy(hybrid2_predictions,
                                                  getData(train_scheme, "unknown"),
                                                  given = 5,
                                                  goodRating = 3,
                                                  byUser = TRUE)

hybrid2_accuracy_by_user <- as.data.frame(hybrid2_accuracy_by_user) %>%
  mutate(VisitorId = row.names(.)) %>%
  select(VisitorId, everything())
summary(hybrid2_accuracy_by_user$TP)

# Obtain Top N predictions vs. actual purchases last 2-months ------------------
hybrid2_test_recommendations <- data.frame(VisitorId = character(length = nrow(getData(train_scheme, "unknown"))),
                                   rec1 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rec2 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rec3 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rec4 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rec5 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rec6 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rec7 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rec8 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rec9 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rec10 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rec11 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rec12 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rec13 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rec14 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rec15 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rec16 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rec17 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rec18 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rec19 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rec20 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rating1 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rating2 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rating3 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rating4 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rating5 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rating6 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rating7 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rating8 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rating9 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rating10 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rating11 = integer(length = nrow(getData(train_scheme, "unknown"))), 
                                   rating12 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rating13 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rating14 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rating15 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rating16 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rating17 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rating18 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rating19 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   rating20 = integer(length = nrow(getData(train_scheme, "unknown"))),
                                   stringsAsFactors=FALSE) 

for(i in 1:nrow(getData(train_scheme, "unknown"))) {
  hybrid2_test_recommendations$VisitorId[i] <- dimnames(getData(train_scheme, "unknown"))[[1]][i]
  hybrid2_test_recommendations$rec1[i]  <- hybrid2_predictions@items[[i]][1]
  hybrid2_test_recommendations$rec2[i]  <- hybrid2_predictions@items[[i]][2]
  hybrid2_test_recommendations$rec3[i]  <- hybrid2_predictions@items[[i]][3]
  hybrid2_test_recommendations$rec4[i]  <- hybrid2_predictions@items[[i]][4]
  hybrid2_test_recommendations$rec5[i]  <- hybrid2_predictions@items[[i]][5]
  hybrid2_test_recommendations$rec6[i]  <- hybrid2_predictions@items[[i]][6]
  hybrid2_test_recommendations$rec7[i]  <- hybrid2_predictions@items[[i]][7]
  hybrid2_test_recommendations$rec8[i]  <- hybrid2_predictions@items[[i]][8]
  hybrid2_test_recommendations$rec9[i]  <- hybrid2_predictions@items[[i]][9]
  hybrid2_test_recommendations$rec10[i] <- hybrid2_predictions@items[[i]][10]
  hybrid2_test_recommendations$rec11[i] <- hybrid2_predictions@items[[i]][11]
  hybrid2_test_recommendations$rec12[i] <- hybrid2_predictions@items[[i]][12]
  hybrid2_test_recommendations$rec13[i] <- hybrid2_predictions@items[[i]][13]
  hybrid2_test_recommendations$rec14[i] <- hybrid2_predictions@items[[i]][14]
  hybrid2_test_recommendations$rec15[i] <- hybrid2_predictions@items[[i]][15]
  hybrid2_test_recommendations$rec16[i] <- hybrid2_predictions@items[[i]][16]
  hybrid2_test_recommendations$rec17[i] <- hybrid2_predictions@items[[i]][17]
  hybrid2_test_recommendations$rec18[i] <- hybrid2_predictions@items[[i]][18]
  hybrid2_test_recommendations$rec19[i] <- hybrid2_predictions@items[[i]][19]
  hybrid2_test_recommendations$rec20[i] <- hybrid2_predictions@items[[i]][20]
  hybrid2_test_recommendations$rating1[i] <- hybrid2_predictions@ratings[[i]][1]
  hybrid2_test_recommendations$rating2[i] <- hybrid2_predictions@ratings[[i]][2]
  hybrid2_test_recommendations$rating3[i] <- hybrid2_predictions@ratings[[i]][3]
  hybrid2_test_recommendations$rating4[i] <- hybrid2_predictions@ratings[[i]][4]
  hybrid2_test_recommendations$rating5[i] <- hybrid2_predictions@ratings[[i]][5]
  hybrid2_test_recommendations$rating6[i] <- hybrid2_predictions@ratings[[i]][6]
  hybrid2_test_recommendations$rating7[i] <- hybrid2_predictions@ratings[[i]][7]
  hybrid2_test_recommendations$rating8[i] <- hybrid2_predictions@ratings[[i]][8]
  hybrid2_test_recommendations$rating9[i] <- hybrid2_predictions@ratings[[i]][9]
  hybrid2_test_recommendations$rating10[i] <- hybrid2_predictions@ratings[[i]][10]
  hybrid2_test_recommendations$rating11[i] <- hybrid2_predictions@ratings[[i]][11]
  hybrid2_test_recommendations$rating12[i] <- hybrid2_predictions@ratings[[i]][12]
  hybrid2_test_recommendations$rating13[i] <- hybrid2_predictions@ratings[[i]][13]
  hybrid2_test_recommendations$rating14[i] <- hybrid2_predictions@ratings[[i]][14]
  hybrid2_test_recommendations$rating15[i] <- hybrid2_predictions@ratings[[i]][15]
  hybrid2_test_recommendations$rating16[i] <- hybrid2_predictions@ratings[[i]][16]
  hybrid2_test_recommendations$rating17[i] <- hybrid2_predictions@ratings[[i]][17]
  hybrid2_test_recommendations$rating18[i] <- hybrid2_predictions@ratings[[i]][18]
  hybrid2_test_recommendations$rating19[i] <- hybrid2_predictions@ratings[[i]][19]
  hybrid2_test_recommendations$rating20[i] <- hybrid2_predictions@ratings[[i]][20]
}

# extract the item names from hybrid2_predictions object. Need to replace the 
# predicted item indices with there obfuscation item label (line 371)
item_labels <- as.character(hybrid2_predictions@itemLabels)
item_labels_df <- data.frame(parent_index = 1:length(item_labels),
                             parent_rec = item_labels)

# convert top 20 rec's data from wide format to long format.
top20_rec_tidy <- hybrid2_test_recommendations %>% 
  select(-starts_with("rating")) %>% # drop predicted ratings columns
  gather(key = rec_number, value = parent_index, -VisitorId) %>%
  # strip text and keep the number.
  mutate(rec_number = as.numeric(gsub("[^0-9]", "", rec_number))) %>%
  # replace parent index with the actual item lable.
  left_join(item_labels_df, by = 'parent_index') %>%
  select(-parent_index)
  
top20_ratings_tidy <- hybrid2_test_recommendations %>% 
  select(-starts_with("rec")) %>% # drop predicted records columns.
  gather(key = rating_number, value = predicted_rating, -VisitorId) %>%
  mutate(rec_number = as.numeric(gsub("[^0-9]", "", rating_number)))

test_recommendations <- top20_rec_tidy %>%
  inner_join(top20_ratings_tidy, by = c('VisitorId', 'rec_number')) %>%
  select(-rating_number) %>%
  rename(predicted_parent = parent_rec) %>%
  arrange(VisitorId, rec_number)


# CHECK: do values from the hybrid2_predictions S4 object for user1 match the parent
# labels for the first user in the test_recommendations dataset.
dimnames(getData(train_scheme, "unknown"))[[1]][1] # user "706937327430"
user1_predicted_items <- hybrid2_predictions@items[[1]]
user1_predicted_items <- hybrid2_predictions@itemLabels[user1_predicted_items]
user1_predicted_items
user1_predicted_ratings <- hybrid2_predictions@ratings[[1]]
user1_predicted_ratings

# data frame extraction predicted items match!
all.equal(user1_predicted_items, 
          as.character(test_recommendations[test_recommendations$VisitorId == '706937327430', ]$predicted_parent))

# data frame extraction predicted ratings match!
all.equal(user1_predicted_ratings, 
          test_recommendations[test_recommendations$VisitorId == '706937327430', ]$predicted_rating)


