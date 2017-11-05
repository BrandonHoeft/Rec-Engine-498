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
set.seed(2017)
keep_visitor_index <- user_activity_bw_periods %>%
  filter(before_jul17 > 19 & after_jul17 > 13) %>%
  select(VisitorId) %>%
  mutate(VisitorId = as.character(VisitorId)) %>%
  sample_frac(.25) # sample a 25% subset of these records to model with. 

keep_visitor_index <- keep_visitor_index$VisitorId

# Split the data into 5-months vs 2-month --------------------------------------
# each dataset will have the same VisitorIds in them.
user_items_first_5month <- user_items %>%
  filter(ActionDate < mdy('7/1/2017'), # Feb - Jun used for training.
         VisitorId %in% keep_visitor_index) # Visitors with enough different Parent interactions pre July17

# which users had the most click activity in the first 5 months? 
total_clicks_summary <- user_items_first_5month %>%
  group_by(VisitorId) %>%
  summarize(total_clicks_first_5month = sum(ActionCount),
            different_parents_first_5month = length(unique(Parent))) %>%
  arrange(desc(different_parents_first_5month))


user_items_last_2month <- user_items %>%
  filter(ActionDate >= mdy('7/1/2017'), # Jul - Aug used for testing.
         VisitorId %in% keep_visitor_index) # Visitors with enough different Parent interactions during/after July17

# Pre-process the ratings scheme -----------------------------------------------

# For a time-based Training dataset.
user_ratings_first_5month <- user_items_first_5month %>%
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

user_ratings_last_2month <- user_items_last_2month %>%
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





# Convert User-Item ratings for training into a sparse matrix  -----------------

library(Matrix)
# sparse ratings matrix.
# https://stackoverflow.com/questions/28430674/create-sparse-matrix-from-data-frame?noredirect=1&lq=1
# the VisitorId and Parent need to be 1 based indices when creating a matrix. 
# Per ?factor, the levels of a factor are by default sorted.
training_matrix <- sparseMatrix(i = as.integer(as.factor(user_ratings_first_5month$VisitorId)), 
                                j = as.integer(as.factor(user_ratings_first_5month$Parent)),
                                x = user_ratings_first_5month$total_rating_max10)

# can rename the matrix row and column labels with unique VisitorId and Parent names. 
dimnames(training_matrix) <- list(sort(unique(user_ratings_first_5month$VisitorId)),
                                  sort(unique(user_ratings_first_5month$Parent)))

all.equal(levels(as.factor(user_ratings_first_5month$VisitorId)), 
          sort(unique(user_ratings_first_5month$VisitorId))) 

all.equal(levels(as.factor(user_ratings_first_5month$Parent)), 
          sort(unique(user_ratings_first_5month$Parent))) 




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

# Keep Parent items with a high number of ratings. Above median.
training <- train_r[, colCounts(train_r) > 9]
training_items <- dimnames(training)[[2]]
training


# Get the Item-based CF model, get test predictions -------------------------------

# Fit Taylor's best IBCF model on the first 5-months training records. takes about 30+ min to run.
#ibcf_model <- Recommender(data = training, 
#                          method = "IBCF", 
#                          parameter = list(method = "cosine",
#                                           k = 25,
#                                           normalize = "center"))
#s3save(ibcf_model, bucket = "pred498team5", object = "ibcf_model.Rdata")
s3load("ibcf_model.Rdata", bucket = "pred498team5")

ibcf_model
str(ibcf_model)

# Get the Item-based CF model, get test predictions -------------------------------

# Fit Taylor's best IBCF model on the first 5-months training records. takes about 30+ min to run.
#ibcf_model <- Recommender(data = training, 
#                          method = "IBCF", 
#                          parameter = list(method = "cosine",
#                                           k = 25,
#                                           normalize = "center"))
#s3save(ibcf_model, bucket = "pred498team5", object = "ibcf_model.Rdata")
s3load("ibcf_model.Rdata", bucket = "pred498team5")

ibcf_model
str(ibcf_model)

# Fit Best User-based CF model, get test predictions -------------------------------
# ubcf_cosine_50nn won out vs other UBCF models in separate script (collab_filtering_smaller_samples.R)

# Fit the top performing ubcf_cosine_50nn model as single model. 
ubcf_model <- Recommender(data = training, 
                          method = "UBCF", 
                          parameter = list(method = "cosine",
                                           nn = 50,
                                           normalize = "center"))
ubcf_model


# Build a Hybrid Recommender System of UBCF, IBCF, popular, re-recommended ------


hybrid_rec_model <- HybridRecommender(
  ubcf_model,
  ibcf_model,
  Recommender(train, method = "RERECOMMEND"),
  Recommender(train, method = "POPULAR"),
  weights = c(.25, .25, .25, .25)
)







# Recommend new parent items to the same users, based on their original information.
# We'll evaluate predictions against the items they rated in the last 2_months. 
ibcf_top20_predictions <- predict(object = ibcf_model, 
                                  newdata = training, 
                                  type = "topNList", 
                                  n = 20)
s3save(ibcf_top20_predictions, bucket = "pred498team5", object = "ibcf_top20_predictions.Rdata")
#s3load("ibcf_top20_predictions.Rdata", bucket = "pred498team5")
str(ibcf_top20_predictions)



# Obtain Results, determine Top N predictions vs. actual purchases last 2-months ----------------
test_recommendations <- data.frame(VisitorId = character(length = nrow(training)),
                                   rec1 = integer(length = nrow(training)),
                                   rec2 = integer(length = nrow(training)), 
                                   rec3 = integer(length = nrow(training)), 
                                   rec4 = integer(length = nrow(training)), 
                                   rec5 = integer(length = nrow(training)), 
                                   rec6 = integer(length = nrow(training)), 
                                   rec7 = integer(length = nrow(training)), 
                                   rec8 = integer(length = nrow(training)), 
                                   rec9 = integer(length = nrow(training)), 
                                   rec10 = integer(length = nrow(training)), 
                                   rec11 = integer(length = nrow(training)), 
                                   rec12 = integer(length = nrow(training)),
                                   rec13 = integer(length = nrow(training)),
                                   rec14 = integer(length = nrow(training)),
                                   rec15 = integer(length = nrow(training)),
                                   rec16 = integer(length = nrow(training)),
                                   rec17 = integer(length = nrow(training)),
                                   rec18 = integer(length = nrow(training)),
                                   rec19 = integer(length = nrow(training)),
                                   rec20 = integer(length = nrow(training)),
                                   stringsAsFactors=FALSE) 

for(i in 1:nrow(training)) {
  test_recommendations$VisitorId[i] <- dimnames(training)[[1]][i]
  test_recommendations$rec1[i]  <- ibcf_top20_predictions@items[[i]][1]
  test_recommendations$rec2[i]  <- ibcf_top20_predictions@items[[i]][2]
  test_recommendations$rec3[i]  <- ibcf_top20_predictions@items[[i]][3]
  test_recommendations$rec4[i]  <- ibcf_top20_predictions@items[[i]][4]
  test_recommendations$rec5[i]  <- ibcf_top20_predictions@items[[i]][5]
  test_recommendations$rec6[i]  <- ibcf_top20_predictions@items[[i]][6]
  test_recommendations$rec7[i]  <- ibcf_top20_predictions@items[[i]][7]
  test_recommendations$rec8[i]  <- ibcf_top20_predictions@items[[i]][8]
  test_recommendations$rec9[i]  <- ibcf_top20_predictions@items[[i]][9]
  test_recommendations$rec10[i] <- ibcf_top20_predictions@items[[i]][10]
  test_recommendations$rec11[i] <- ibcf_top20_predictions@items[[i]][11]
  test_recommendations$rec12[i] <- ibcf_top20_predictions@items[[i]][12]
  test_recommendations$rec13[i] <- ibcf_top20_predictions@items[[i]][13]
  test_recommendations$rec14[i] <- ibcf_top20_predictions@items[[i]][14]
  test_recommendations$rec15[i] <- ibcf_top20_predictions@items[[i]][15]
  test_recommendations$rec16[i] <- ibcf_top20_predictions@items[[i]][16]
  test_recommendations$rec17[i] <- ibcf_top20_predictions@items[[i]][17]
  test_recommendations$rec18[i] <- ibcf_top20_predictions@items[[i]][18]
  test_recommendations$rec19[i] <- ibcf_top20_predictions@items[[i]][19]
  test_recommendations$rec20[i] <- ibcf_top20_predictions@items[[i]][20]
}


# convert data from wide format to long format.
test_recommendations_tidy <- test_recommendations %>% 
  gather(key = rec_number, value = parent_index, -VisitorId)

# prepare the item labels so they can be mapped to the item indices in the predicted recommendations
# ibcf_top20_predictions S4 object.
item_labels <- as.character(ibcf_top20_predictions@itemLabels)
item_labels_df <- data.frame(parent_index = 1:length(item_labels),
                             parent_rec = item_labels)

# left join the parent item label to test_recommendations_tidy.
test_recommendations_update <- test_recommendations_tidy %>%
  left_join(item_labels_df, by = 'parent_index') %>%
  mutate(rec_number = as.numeric(gsub("[^0-9]", "", rec_number))) %>% # strip off the string.
  arrange(VisitorId, rec_number)

# values from the ibcf_top20_predictions S4 object for user1 match the parent
# labels for the first user in the test_recommendations_update dataset.
user1 <- ibcf_top20_predictions@items[[1]]
items_user1 <- ibcf_top20_predictions@itemLabels[user1]
items_user1

all.equal(items_user1, 
          as.character(test_recommendations_update[test_recommendations_update$VisitorId == '1000434402046', ]$parent_rec))


# Time to determine which of the top 20 recommendations for each user in the 
# test dataset was a true positive vs. a false positive
test_users_true_positives <- user_ratings_last_2month %>%
  filter(total_rating_max10 >= 3) %>% # this was threshold used for 'Good Rating'
  select(VisitorId, Parent, total_rating_max10) %>%
  rename(known_rating = total_rating_max10) %>%
  mutate(known_parent = Parent) %>% # we'll want this as a sep. column after joining. lines 363-366
  arrange(VisitorId, desc(known_rating))

str(test_users_true_positives)

# left join the true positives from the last 2_months of test data to our
# predictions of top 20 recommendations for each visitor. Also left join each
# user's first 5_month's summary of total clicks and different parent items data. 
test_recommendations_update <- test_recommendations_update %>%
  left_join(test_users_true_positives, by = c('VisitorId' = 'VisitorId',
                                              'parent_rec' = 'Parent')) %>%
  left_join(total_clicks_summary, by = 'VisitorId')

ibcf_test_recommendations <- test_recommendations_update %>%
  group_by(VisitorId) %>%
  mutate(test_true_positives = sum(!is.na(known_parent)),
         test_precision = round(test_true_positives / n(), 2)) %>%
  ungroup() %>%
  arrange(desc(test_true_positives))
s3save(ibcf_test_recommendations, bucket = "pred498team5", object = "ibcf_test_recommendations.Rdata")
#s3load("ibcf_test_recommendations.Rdata", bucket = "pred498team5")

ibcf_test_precision_summary <- ibcf_test_recommendations %>%
  distinct(VisitorId, test_precision) %>%
  summarize(min = min(test_precision),
            first_quartile = quantile(test_precision, 0.25),
            median = median(test_precision),
            mean = mean(test_precision),
            third_quartile = quantile(test_precision, 0.75),
            max = max(test_precision))

s3save(ibcf_test_precision_summary, bucket = "pred498team5", object = "ibcf_test_precision_summary.Rdata")
#s3load("ibcf_test_precision_summary.Rdata", bucket = "pred498team5")
