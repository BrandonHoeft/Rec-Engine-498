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

# Fit Different SVD and Latent Factor Models-----------------------------------
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

set.seed(2017)
train_scheme <- evaluationScheme(real_r_filtered_rows_cols,
                                method = "split", # random train/test scheme
                                train = 0.75,
                                k = 1,
                                given = 5,  # how many records for a test user will learn the model? 
                                goodRating = 3) # threshold for classification. Just above Median Total_rating_max10.

latent_factor_algorithms <- list(
  "ALS1" = list(name = "ALS",
                param = list(normalize = "center",
                             lambda = 0.1,
                             n_factors = 100,
                             seed = 2017)),
  "ALS2" = list(name = "ALS",
                param = list(normalize = "center",
                             lambda = 0.2, # regularization parameter.
                             n_factors = 250,
                             seed = 2017)),
  "ALS3" = list(name = "ALS",
                param =  list(normalize = "center",
                              lambda = 0.3,
                              n_factors = 1000,
                              seed = 2017)),
  "ALS_implicit1" = list(name = "ALS_implicit",
                             param = list(lambda = 0.1,
                                          alpha = 8, # How confident are we of implicit feedback
                                          n_factors = 100,
                                          min_item_nr = 3,
                                          seed = 2017)),
  "ALS_implicit2" = list(name = "ALS_implicit",
                         param = list(lambda = 0.2,
                                      alpha = 5, # How confident are we of implicit feedback
                                      n_factors = 250,
                                      min_item_nr = 1,
                                      seed = 2017)),
  "ALS_implicit3" = list(name = "ALS_implicit",
                         param = list(lambda = 0.3,
                                      alpha = 5, # How confident are we of implicit feedback
                                      n_factors = 1000,
                                      min_item_nr = 1,
                                      seed = 2017))
)

latent_factor_results <- evaluate(train_scheme,
                                  latent_factor_algorithms,
                                  type = "topNList",
                                  n = c(5, 10, 20, 50))

#3save(latent_factor_results, bucket = "pred498team5", object = "latent_factor_results_11717.Rdata")
s3load("latent_factor_results_11717.Rdata", bucket = "pred498team5")
class(latent_factor_results)
latent_factor_results
avg(latent_factor_results)
plot(latent_factor_results, annotate=TRUE)
plot(latent_factor_results, "prec/rec", annotate=TRUE)


# Fit best latent factor Model on single train/test ----------------------------
# ALS Implicit1 was the best performing model across different sizes of N.

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
latent_factor_model <- getData(train_scheme, "train") %>%
  Recommender(method = "ALS_implicit", parameter = list(lambda = 0.1,
                                                alpha = 8, # How confident are we of implicit feedback
                                                n_factors = 100,
                                                min_item_nr = 3,
                                                seed = 2017))

latent_factor_predictions <- predict(latent_factor_model, 
                            getData(train_scheme, "known"), 
                            type = "topNList", 
                            n = 20)
s3save(latent_factor_predictions, bucket = "pred498finalmodel", object = "latent_factor_predict_object.Rdata")
#s3load("latent_factor_predict_object.Rdata", bucket = "pred498finalmodel")

latent_factor_accuracy_by_user <- calcPredictionAccuracy(latent_factor_predictions,
                                                getData(train_scheme, "unknown"),
                                                given = 5,
                                                goodRating = 3,
                                                byUser = TRUE)

latent_factor_accuracy_by_user <- as.data.frame(latent_factor_accuracy_by_user) %>%
  mutate(VisitorId = row.names(.)) %>%
  select(VisitorId, everything())

fn <- sum(latent_factor_accuracy_by_user$FN)
tn <- sum(latent_factor_accuracy_by_user$TN)
tp <- sum(latent_factor_accuracy_by_user$TP)
fp <- sum(latent_factor_accuracy_by_user$FP)


#         TP * TN - FP * FN
# MCC = -----------------------------------------------------
#        [(TP + FP) * (FN + TN) * (FP + TN) * (TP + FN)]^(1/2)

mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (fn + tn) * (fp + tn) * (tp + fn))




# Obtain Top N predictions vs. actual purchases last 2-months ------------------
latent_factor_test_recommendations <- data.frame(VisitorId = character(length = nrow(getData(train_scheme, "unknown"))),
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
  latent_factor_test_recommendations$VisitorId[i] <- dimnames(getData(train_scheme, "unknown"))[[1]][i]
  latent_factor_test_recommendations$rec1[i]  <- latent_factor_predictions@items[[i]][1]
  latent_factor_test_recommendations$rec2[i]  <- latent_factor_predictions@items[[i]][2]
  latent_factor_test_recommendations$rec3[i]  <- latent_factor_predictions@items[[i]][3]
  latent_factor_test_recommendations$rec4[i]  <- latent_factor_predictions@items[[i]][4]
  latent_factor_test_recommendations$rec5[i]  <- latent_factor_predictions@items[[i]][5]
  latent_factor_test_recommendations$rec6[i]  <- latent_factor_predictions@items[[i]][6]
  latent_factor_test_recommendations$rec7[i]  <- latent_factor_predictions@items[[i]][7]
  latent_factor_test_recommendations$rec8[i]  <- latent_factor_predictions@items[[i]][8]
  latent_factor_test_recommendations$rec9[i]  <- latent_factor_predictions@items[[i]][9]
  latent_factor_test_recommendations$rec10[i] <- latent_factor_predictions@items[[i]][10]
  latent_factor_test_recommendations$rec11[i] <- latent_factor_predictions@items[[i]][11]
  latent_factor_test_recommendations$rec12[i] <- latent_factor_predictions@items[[i]][12]
  latent_factor_test_recommendations$rec13[i] <- latent_factor_predictions@items[[i]][13]
  latent_factor_test_recommendations$rec14[i] <- latent_factor_predictions@items[[i]][14]
  latent_factor_test_recommendations$rec15[i] <- latent_factor_predictions@items[[i]][15]
  latent_factor_test_recommendations$rec16[i] <- latent_factor_predictions@items[[i]][16]
  latent_factor_test_recommendations$rec17[i] <- latent_factor_predictions@items[[i]][17]
  latent_factor_test_recommendations$rec18[i] <- latent_factor_predictions@items[[i]][18]
  latent_factor_test_recommendations$rec19[i] <- latent_factor_predictions@items[[i]][19]
  latent_factor_test_recommendations$rec20[i] <- latent_factor_predictions@items[[i]][20]
  latent_factor_test_recommendations$rating1[i] <- latent_factor_predictions@ratings[[i]][1]
  latent_factor_test_recommendations$rating2[i] <- latent_factor_predictions@ratings[[i]][2]
  latent_factor_test_recommendations$rating3[i] <- latent_factor_predictions@ratings[[i]][3]
  latent_factor_test_recommendations$rating4[i] <- latent_factor_predictions@ratings[[i]][4]
  latent_factor_test_recommendations$rating5[i] <- latent_factor_predictions@ratings[[i]][5]
  latent_factor_test_recommendations$rating6[i] <- latent_factor_predictions@ratings[[i]][6]
  latent_factor_test_recommendations$rating7[i] <- latent_factor_predictions@ratings[[i]][7]
  latent_factor_test_recommendations$rating8[i] <- latent_factor_predictions@ratings[[i]][8]
  latent_factor_test_recommendations$rating9[i] <- latent_factor_predictions@ratings[[i]][9]
  latent_factor_test_recommendations$rating10[i] <- latent_factor_predictions@ratings[[i]][10]
  latent_factor_test_recommendations$rating11[i] <- latent_factor_predictions@ratings[[i]][11]
  latent_factor_test_recommendations$rating12[i] <- latent_factor_predictions@ratings[[i]][12]
  latent_factor_test_recommendations$rating13[i] <- latent_factor_predictions@ratings[[i]][13]
  latent_factor_test_recommendations$rating14[i] <- latent_factor_predictions@ratings[[i]][14]
  latent_factor_test_recommendations$rating15[i] <- latent_factor_predictions@ratings[[i]][15]
  latent_factor_test_recommendations$rating16[i] <- latent_factor_predictions@ratings[[i]][16]
  latent_factor_test_recommendations$rating17[i] <- latent_factor_predictions@ratings[[i]][17]
  latent_factor_test_recommendations$rating18[i] <- latent_factor_predictions@ratings[[i]][18]
  latent_factor_test_recommendations$rating19[i] <- latent_factor_predictions@ratings[[i]][19]
  latent_factor_test_recommendations$rating20[i] <- latent_factor_predictions@ratings[[i]][20]
}


# extract the item names from predictions object. Need to replace the 
# predicted item indices with there obfuscation item label
item_labels <- as.character(latent_factor_predictions@itemLabels)
item_labels_df <- data.frame(parent_index = 1:length(item_labels),
                             parent_rec = item_labels)

# convert top 20 rec's data from wide format to long format.
top20_rec_tidy <- latent_factor_test_recommendations %>% 
  select(-starts_with("rating")) %>% # drop predicted ratings columns
  gather(key = rec_number, value = parent_index, -VisitorId) %>%
  # strip text and keep the number.
  mutate(rec_number = as.numeric(gsub("[^0-9]", "", rec_number))) %>%
  # replace parent index with the actual item lable.
  left_join(item_labels_df, by = 'parent_index') %>%
  select(-parent_index)

top20_ratings_tidy <- latent_factor_test_recommendations %>% 
  select(-starts_with("rec")) %>% # drop predicted records columns.
  gather(key = rating_number, value = predicted_rating, -VisitorId) %>%
  mutate(rec_number = as.numeric(gsub("[^0-9]", "", rating_number)))

# MOST IMPORTANT. THESE ARE OUR FINAL TOP 20 PREDICTIONS!!!!!!!
test_recommendations <- top20_rec_tidy %>%
  inner_join(top20_ratings_tidy, by = c('VisitorId', 'rec_number')) %>%
  select(-rating_number) %>%
  rename(predicted_parent = parent_rec) %>%
  arrange(VisitorId, rec_number)


# CHECK: do values from the latent factor predictions S4 object for user1 match the parent
# labels for the first user in the test_recommendations dataset.
dimnames(getData(train_scheme, "unknown"))[[1]][1] # user "706937327430"
user1_predicted_items <- latent_factor_predictions@items[[1]]
user1_predicted_items <- latent_factor_predictions@itemLabels[user1_predicted_items]
user1_predicted_items
user1_predicted_ratings <- latent_factor_predictions@ratings[[1]]
user1_predicted_ratings

# data frame extraction predicted items match!
all.equal(user1_predicted_items, 
          as.character(test_recommendations[test_recommendations$VisitorId == '706937327430', ]$predicted_parent))

# data frame extraction predicted ratings match!
all.equal(user1_predicted_ratings, 
          test_recommendations[test_recommendations$VisitorId == '706937327430', ]$predicted_rating)


# Identify True positives vs. False Positives from predicted records -----------
# Need to convert the unknown RealRatingMatrix into a dataframe (VisitorId, Parent, Rating)
# https://stackoverflow.com/questions/15849641/how-to-convert-a-sparse-matrix-into-a-matrix-of-index-and-value-of-non-zero-elem
temp <-as(getData(train_scheme, "unknown"), "matrix")
temp <- as(temp, "sparseMatrix")
sparseToVector <- function(x) {as.data.frame(summary(x))}

unknown_test_data_df <- sparseToVector(temp) %>%
  filter(!is.na(x)) # should have only 126,929 rows reflecting the 126,929 ratings in getData(train_scheme, "unknown")

head(item_labels_df)
visitor_labels_df <- data.frame(visitor_index =1:length(dimnames(getData(train_scheme, "unknown"))[[1]]),
                                VisitorId = dimnames(getData(train_scheme, "unknown"))[[1]])
head(visitor_labels_df)

unknown_test_data_df <- unknown_test_data_df %>%
  left_join(visitor_labels_df, by = c('i' = 'visitor_index')) %>%
  left_join(item_labels_df, by = c('j' = 'parent_index')) %>%
  rename(known_rating = x) 

# convert factor to character. More accurate for joining.
unknown_test_data_df$VisitorId <- sapply(unknown_test_data_df$VisitorId, as.character)
unknown_test_data_df$parent_rec <- sapply(unknown_test_data_df$parent_rec, as.character)

# MOST IMPORTANT. THESE ARE OUR FINAL TOP 20 PREDICTIONS with column for True Positive Identification!!!!!!!
# left join the true positives from the unknown test data to our
# predictions of top 20 recommendations for each visitor.

# convert factor to character. More accurate for joining.
test_recommendations$predicted_parent <- sapply(test_recommendations$predicted_parent, as.character)

test_recommendations_final <- test_recommendations %>%
  left_join(unknown_test_data_df, by = c('VisitorId' = 'VisitorId',
                                         'predicted_parent' = 'parent_rec')) %>%
  rename(true_positive_rating = known_rating) %>%
  select(-i, -j)
s3save(test_recommendations_final, bucket = "pred498finalmodel", object = "latent_factor_test_recommendations_final.Rdata")
#s3load("latent_factor_test_recommendations_final.Rdata", bucket = "pred498finalmodel")

# Confirm that the unknown_test_data_df is actually only returning the rows of the 
# unknown records and not leaking any of the 5 records used to train new data. CONFIRMED. 
rowCounts(real_r_filtered_rows_cols["706937327430",]) # 51 items rated for this user.
unknown_test_data_df %>%
  filter(VisitorId == '706937327430') # 46 row counts for this test record. 5 were used to train the model. These 46 were the unknown values available for predicting. 

# 2nd Most important. Summary statistics for each test user on their TP count and Precision calculation. 
# Calculate TP counts and Precision for each of the test users.
test_recommendations_performance <- test_recommendations_final %>%
  group_by(VisitorId) %>%
  summarize(TP = sum(!is.na(true_positive_rating)),
            test_precision = round(TP / n(), 2)) %>%
  ungroup() %>%
  arrange(desc(TP))
s3save(test_recommendations_performance, bucket = "pred498finalmodel", object = "latent_factor_test_recommendations_performance.Rdata")
#s3load("latent_factor_test_recommendations_performance.Rdata", bucket = "pred498finalmodel")

summary(test_recommendations_performance$test_precision)
summary(test_recommendations_performance$TP)
