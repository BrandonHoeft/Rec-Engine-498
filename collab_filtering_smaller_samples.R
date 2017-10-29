library(aws.s3) 
library(readr) # faster alternatives to base read. methods. 
library(lubridate) # date wrangling. 
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # data viz. 
library(pryr) # mem_used() and object_size() functions to manage/understand memory usage.


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
recommenderRegistry$get_entries(dataType = "realRatingMatrix")


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




# Keep users with a high number of ratings
real_r_filtered_rows <- real_r_sampled_row_cols[rowCounts(real_r_sampled_row_cols) >= 50, ] # around median from above.
nrow(real_r_filtered_rows) - nrow(real_r)
real_r_filtered_rows
# Updated row and column count summaries
summary(rowCounts(real_r_filtered_rows))
summary(colCounts(real_r_filtered_rows))

# Keep Parent items with a high number of ratings
real_r_filtered_rows_cols <- real_r_filtered_rows[, colCounts(real_r_filtered_rows) > 5]

# Updated row and column count summaries
summary(rowCounts(real_r_filtered_rows_cols))
summary(colCounts(real_r_filtered_rows_cols))
summary(rowMeans(real_r_filtered_rows_cols))
summary(colMeans(real_r_filtered_rows_cols))
real_r_filtered_rows_cols



# Fit Models Individually ------------------------------------------------------

set.seed(2017)
ubcf_scheme1 <- evaluationScheme(real_r_filtered_rows_cols,
                                method = "split", # random train/test scheme
                                train = 0.75,
                                k = 1,
                                given = -30,  # how many records for a test user will learn the model? Give all but 30. 
                                goodRating = 3) # threshold for classification. Just above Median Total_rating_max10.
ubcf_scheme1

ubcf_algorithms1 <- list(
  "ubcf_cosine_25nn" = list(name = "UBCF",
                            param = list(method = "cosine", 
                                         nn = 25,
                                         normalize = "center")),
  "ubcf_pearson_25nn" = list(name = "UBCF",
                             param = list(method = "pearson", 
                                          nn = 25,
                                          normalize = "center"))
)



model1 <- getData(ubcf_scheme1, "train") %>% 
  Recommender(method = "UBCF", parameter = ubcf_algorithms1$ubcf_cosine_25nn$param)

model2 <- getData(ubcf_scheme1, "train") %>% 
  Recommender(method = "UBCF", parameter = ubcf_algorithms1$ubcf_pearson_25nn$param)

# Apply model to data
model1_pred <- predict(model1, getData(ubcf_scheme1, "known"), type = "topNList", n = 5)

model2_pred <- predict(model2, getData(ubcf_scheme1, "known"), type = "topNList", n = 5)

# Evaluate Predictions on unknown test data 

model1_accuracy = calcPredictionAccuracy(model1_pred, 
                                         getData(ubcf_scheme1, "unknown"),
                                         given = -30,
                                         goodRating = 3,
                                         byUser = FALSE)

model2_accuracy = calcPredictionAccuracy(model2_pred, 
                                         getData(ubcf_scheme1, "unknown"),
                                         given = -30,
                                         goodRating = 3,
                                         byUser = FALSE)

rbind(model1_accuracy, model2_accuracy)




# Fit Models Iteratively -------------------------------------------------------

# given = 5, nearest neighbor to 25, 50. 
set.seed(2017)
#ubcf_scheme <- evaluationScheme(real_r_filtered_rows_cols,
#                                method = "split", # random train/test scheme
#                                train = 0.75,
#                                k = 1,
#                                given = 5,  # how many records for a test user will learn the model? 
#                                goodRating = 3) # threshold for classification. Just above Median Total_rating_max10.
#s3save(ubcf_scheme, bucket = "pred498team5", object = "ubcf_scheme.Rdata")
#s3load("ubcf_scheme.Rdata", bucket = "pred498team5")

ubcf_algorithms_list <- list(
  "ubcf_cosine_25nn" = list(name = "UBCF",
                            param = list(method = "cosine", 
                                         nn = 25,
                                         normalize = "center")),
  "ubcf_cosine_50nn" = list(name = "UBCF",
                            param = list(method = "cosine", 
                                         nn = 50,
                                         normalize = "center")),
  "ubcf_pearson_25nn" = list(name = "UBCF",
                             param = list(method = "pearson", 
                                          nn = 25,
                                          normalize = "center")),
  "ubcf_pearson_50nn" = list(name = "UBCF",
                             param = list(method = "pearson", 
                                          nn = 50,
                                          normalize = "center")),
  "popular_recommender" = list(name = "POPULAR", param = NULL),
  "random_recommender" = list(name = "RANDOM", param = NULL)
)

#ubcf_results <- evaluate(ubcf_scheme, 
#                         ubcf_algorithms_list, 
#                         type = "topNList", 
#                         n = c(5, 10, 15, 20, 25, 50))
#s3save(ubcf_results, bucket = "pred498team5", object = "ubcf_results.Rdata")
#s3load("ubcf_results.Rdata", bucket = "pred498team5")

avg(ubcf_results)
plot(ubcf_results, annotate=TRUE)
plot(ubcf_results, "prec/rec", annotate=TRUE)

ubcf_results
ubcf_results[["ubcf_pearson_25nn"]]


