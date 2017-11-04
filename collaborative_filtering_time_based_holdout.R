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

# Summarize the dates ----------------------------------------------------------
summary(user_items$ActionDate)

# Identify users who made purchases before July 2017 and in/after July 2017 ----
user_activity_bw_periods <- user_items %>%
  mutate(pre_July17_activity = ifelse(ActionDate < mdy('7/1/2017'), "before_jul17", "after_jul17")) %>%
  group_by(VisitorId, pre_July17_activity) %>%
  distinct(Parent) %>% # only keep distinct parent per visitor.
  summarize(unique_parent_count = n()) %>%
  ungroup() %>%
  spread(key = pre_July17_activity, value = unique_parent_count)

summary(user_activity_bw_periods$before_jul17)
summary(user_activity_bw_periods$after_jul17)

# Keep users interacting with > 1st quartile count of different Parent items
#   pre-July 2017 (Training period) and in/after-July 2017 (testing period)
keep_visitor_index <- user_activity_bw_periods %>%
  filter(before_jul17 > 19 & after_jul17 > 13) %>%
  select(VisitorId) %>%
  mutate(VisitorId = as.character(VisitorId))
keep_visitor_index <- keep_visitor_index$VisitorId

# Split the data into 6-months vs 1-month --------------------------------------
user_items_first_6month <- user_items %>%
  filter(ActionDate < mdy('7/1/2017'), # Feb - Jun used for training.
         VisitorId %in% keep_visitor_index) # Visitors with enough different Parent interactions pre July17

user_items_lastmonth <- user_items %>%
  filter(ActionDate >= mdy('7/1/2017'), # Jul - Aug used for testing.
         VisitorId %in% keep_visitor_index) # Visitors with enough different Parent interactions during/after July17


# Pre-process the ratings scheme -----------------------------------------------

# For a time-based Training dataset.
user_ratings_first_6month <- user_items_first_6month %>%
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


# For a time-based Testing dataset.

user_ratings_lastmonth <- user_items_lastmonth %>%
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
training_matrix <- sparseMatrix(i = as.integer(as.factor(user_ratings_first_6month$VisitorId)), 
                         j = as.integer(as.factor(user_ratings_first_6month$Parent)),
                         x = user_ratings_first_6month$total_rating_max10)

# can rename the matrix row and column labels with unique VisitorId and Parent names. 
dimnames(training_matrix) <- list(sort(unique(user_ratings_first_6month$VisitorId)),
                           sort(unique(user_ratings_first_6month$Parent)))

all.equal(levels(as.factor(user_ratings_first_6month$VisitorId)), 
          sort(unique(user_ratings_first_6month$VisitorId))) 

all.equal(levels(as.factor(user_ratings_first_6month$Parent)), 
          sort(unique(user_ratings_first_6month$Parent))) 



testing_matrix <- sparseMatrix(i = as.integer(as.factor(user_ratings_lastmonth$VisitorId)), 
                               j = as.integer(as.factor(user_ratings_lastmonth$Parent)),
                               x = user_ratings_lastmonth$total_rating_max10)

# can rename the matrix row and column labels with unique VisitorId and Parent names. 
dimnames(testing_matrix) <- list(sort(unique(user_ratings_lastmonth$VisitorId)),
                                  sort(unique(user_ratings_lastmonth$Parent)))

all.equal(levels(as.factor(user_ratings_lastmonth$VisitorId)), 
          sort(unique(user_ratings_lastmonth$VisitorId))) 

all.equal(levels(as.factor(user_ratings_lastmonth$Parent)), 
          sort(unique(user_ratings_lastmonth$Parent))) 

# RecommenderLab ---------------------------------------------------------------
library(recommenderlab)
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

# coerce our training sparseMatrix, train_matrix into a realRatingMatrix object
train_r <- as(training_matrix, "realRatingMatrix")
train_r

# How many ratings are there per user
summary(rowCounts(train_r))
# How many visitors have rated each item?
summary(colCounts(train_r))

# Random Sample 50% of the users.
set.seed(2017)
keep_index <- sample(seq_len(nrow(train_r)), 
                     size = nrow(train_r) * 0.50, replace = FALSE)
train_r_sampled_rows <- train_r[keep_index, ]
train_r_sampled_rows

# Random Sample 75% of the parent items. 
set.seed(2017)
keep_col_index <- sample(seq_len(ncol(train_r)), 
                         size = ncol(train_r) * 0.75, replace = FALSE)
train_r_sampled_row_cols <- train_r_sampled_rows[, keep_col_index]
train_r_sampled_row_cols
# Updated row and column count summaries
summary(rowCounts(train_r_sampled_row_cols))
summary(colCounts(train_r_sampled_row_cols))
summary(rowMeans(train_r_sampled_row_cols))
summary(colMeans(train_r_sampled_row_cols))


# Keep users with a high number of ratings
train_r_filtered_rows <- train_r_sampled_row_cols[rowCounts(train_r_sampled_row_cols) >= 50, ] # around median from above.
nrow(train_r_filtered_rows) - nrow(train_r)
train_r_filtered_rows
# Updated row and column count summaries
summary(rowCounts(train_r_filtered_rows))
summary(colCounts(train_r_filtered_rows))

# Keep Parent items with a high number of ratings. Above 1st Quartile. 
training <- train_r_filtered_rows[, colCounts(train_r_filtered_rows) > 5]


# Updated row and column count summaries for training matrix. 
summary(rowCounts(training))
summary(colCounts(training))
summary(rowMeans(training))
summary(colMeans(training))

str(training)
# These are the Visitors in our training dataset. Need to keep only these ones in our testing dataset.
dimnames(training)[[1]] # users
dimnames(training)[[2]] # parent items.


# coerce our testing sparseMatrix, testing_matrix into a realRatingMatrix object
test_r <- as(testing_matrix, "realRatingMatrix")
test_r

str(test_r)
training_users <- dimnames(training)[[1]]
training_items <- dimnames(training)[[2]]
testing <- test_r[dimnames(test_r)[[1]] %in% training_users, 
                  dimnames(test_r)[[2]] %in% training_items]


# Updated row and column count summaries for testing matrix. 
summary(rowCounts(testing))
summary(colCounts(testing))
summary(rowMeans(testing))
summary(colMeans(testing))

# need to delete test users with not enough ratings in the last month. 
# Filter out the test users without 20 or more different items rated in last month. 
# this is around the 1st quartile of rowcounts in the test_r matrix.
testing <- test_r[rowCounts(test_r) >= 20, ]
testing

# Fit Best User-based CF model, get test predictions -------------------------------
# ubcf_cosine_50nn won out vs other UBCF models in separate script (collab_filtering_smaller_samples.R)

# Fit the top performing ubcf_cosine_50nn model as single model. 
ubcf_model <- Recommender(data = training, 
                          method = "UBCF", 
                          parameter = list(method = "cosine",
                                           nn = 50,
                                           normalize = "center"))

ubcf_model

# Recommend new Movies to the same users, but based on their unseen activity 
# from 1-month later. 
top20_predictions <- predict(object = ubcf_model, 
                             newdata = testing, 
                             type = "topNList", 
                             n = 20)
