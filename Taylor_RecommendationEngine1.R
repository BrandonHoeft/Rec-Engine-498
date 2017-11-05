library(aws.s3) 
library(readr) # faster alternatives to base read. methods. 
library(lubridate) # date wrangling. 
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # data viz. 
library(pryr) # mem_used() and object_size() functions to manage/understand memory usage.
library(knitr)
# my global options defined for each code chunk.
knitr::opts_chunk$set(fig.width=8, fig.height=6, echo=FALSE, warning=FALSE, message=FALSE, comment = '')


# specify personal account keys as environment variables so I can read my s3 object(s) from AWS. 

# DO NOT DO NOT DO NOT DO NOT DO NOT SAVE KEY in code or render in output!!!! Could compromise AWS account. 
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAIRUBKOJVXNTTSBMA",
          "AWS_SECRET_ACCESS_KEY" = "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")

# items: part number, parent, catalogue, attributes/values.
items <- s3read_using(FUN = read_table2, 
                      object = "obfuscatedItems_10_17_17.txt", 
                      col_names = TRUE,
                      col_types = "cciciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii",
                      bucket = "pred498engine")

# users: click data from the company website for a random day of user's selected, their activity for past 3 months and click summaries of how they interacted with parts. 

# web user click data from Feb - Mar 2017
users_febmar17 <- s3read_using(FUN = read_csv, 
                               col_names = TRUE,
                               col_types = "ccici",
                               object = "obfuscatedWebActivity7124.csv", 
                               bucket = "pred498engine")

# web user click data from April - June 2017
users_aprmayjun17 <- s3read_using(FUN = read_csv, 
                                  col_names = TRUE,
                                  col_types = "ccici",
                                  object = "obfuscatedWebActivity7127.csv", 
                                  bucket = "pred498engine")

# web user click data from July - August 2017
users_julaug17 <- s3read_using(FUN = read_csv, 
                               col_names = TRUE,
                               col_types = "ccici",
                               object = "obfuscatedWebActivity7129.csv", 
                               bucket = "pred498engine")

#These 3 datasets of user click activity covering different time periods between February - August 2017 need to be unioned, or combined, into a single dataset. 

users <- bind_rows(users_febmar17, users_aprmayjun17, users_julaug17)

'/
The only information were particularly interested in from the **items** dataset is the Parent column, 
and left joining that to the matching PartNumber in the users data. Building a recommender system using the Parent 
family instead of the specific PartNumber will be discussed later. The items dataset has a lot of columns that describe attributes 
of each PartNumber and Parent. For purposes of a collaborative filtering model, we dont need to know these, any columns that include 
*"Attr"*, *"Val"*, or *"Catalog"* in its name are dropped. 
/'

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

#check
all.equal(users[1:3], user_items[1:3])

# We can remove the other datasets we don't need anymore to save available RAM. 
remove(users, items, users_febmar17, users_aprmayjun17, users_julaug17)

#create **action_rating**, which was derived from the **ActionId_label** of every user interaction. 


user_items <- user_items %>%
  mutate(action_rating = if_else(ActionId_label == "select Part", 1, 
                                 if_else(ActionId_label == "select Part detail", 2, 3)))

### Creating a weighted Total Rating per item

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

s3save(user_ratings, object="user_ratings.Rdata", bucket = "pred498engine")

### Setting up a User Ratings Matrix

library(Matrix)
user_ratings <- new.env()
s3load(object="user_ratings.Rdata", bucket = "pred498engine", envir = user_ratings)
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


#check
all.equal(levels(as.factor(user_ratings$VisitorId)), 
          sort(unique(user_ratings$VisitorId))) 

all.equal(levels(as.factor(user_ratings$Parent)), 
          sort(unique(user_ratings$Parent))) 


#shrink matrix to deal with memory problems

## Sample the data

#To improve computation time for this very large matrix, we'll take a random sample of 33% of the user rows in the realRatingMatrix.


# Random Sample 33% of the users.
set.seed(2017)
keep_index <- sample(seq_len(nrow(real_r)), 
size = nrow(real_r) * 0.33, replace = FALSE)
real_r_sampled_rows <- real_r[keep_index, ]
real_r_sampled_rows

#also randomly sample 75% of the different Parent categories, just to help improve the computation time a little bit by having fewer columns to make predictions on.


# Random Sample 75% of the parent items. 
set.seed(2017)
keep_col_index <- sample(seq_len(ncol(real_r)), 
size = ncol(real_r) * 0.75, replace = FALSE)
real_r_sampled_row_cols <- real_r_sampled_rows[, keep_col_index]
real_r_sampled_row_cols

library(recommenderlab)
methods(class = "realRatingMatrix")

# coerce our sparseMatrix, sparse_r into a realRatingMatrix object
real_r <- as(sparse_r, "realRatingMatrix")
real_r

# Keep users with a high number of ratings
real_r_filtered_rows <- real_r_sampled_row_cols[rowCounts(real_r_sampled_row_cols) >= 50, ] # around median from above.

real_r_non_missing_percent <- round(sum(rowCounts(real_r)) / (nrow(real_r) * ncol(real_r)), 5) * 100

# Keep Parent items with a high number of ratings
real_r_filtered_rows_cols <- real_r_filtered_rows[, colCounts(real_r_filtered_rows) > 5]

#new dimensions of reduced set
real_r_filtered_rows_cols

real_r_filtered_non_missing_percent <- round(sum(rowCounts(real_r_filtered_rows_cols)) / (nrow(real_r_filtered_rows_cols) * ncol(real_r_filtered_rows_cols)), 2) * 100

'/
#item-item cosine similarity
# Create a helper function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

#Create a placeholder dataframe listing item vs. item
/'

#------------------------------------------------------------------------------
# Item Based Collaborative Filtering
#------------------------------------------------------------------------------

set.seed(2017)
ibcf_scheme <- evaluationScheme(real_r_filtered_rows_cols,
                                method = "split", # random train/test scheme
                                train = 0.75,
                                k = 1,
                                given = 5,  # how many records for a test user will learn the model? 
                                goodRating = 3) # threshold for classification. Just above Median Total_rating_max10.

ibcf_scheme

#Brief descriptions of this algorithm and its default tuning parameters are illustrated below. 
# recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommenderRegistry$get_entries(dataType = "realRatingMatrix")$IBCF_realRatingMatrix 

ibcf_algorithms_list <- list(
  "ibcf_cosine_100k" = list(name = "IBCF",
                            param = list(method = "cosine", 
                                         k = 100,
                                         normalize = "center")),
  "ibcf_pearson_100k" = list(name = "IBCF",
                            param = list(method = "pearson", 
                                         k = 100,
                                         normalize = "center")),
  "popular_recommender" = list(name = "POPULAR", param = NULL),
  "random_recommender" = list(name = "RANDOM", param = NULL)
)

#run IBCF models
set.seed(2017)
ibcf_results <- evaluate(ibcf_scheme, 
                         ibcf_algorithms_list, 
                         type = "topNList", 
                         n = c(5, 10, 15, 20, 25, 50))

s3save(ibcf_results, object = "ibcf_results.Rdata", bucket = "pred498engine")

'/ Begin: old code
set.seed(2017)
model_IBCF <- Recommender(getData(ibcf_scheme, "train"), method = "IBCF", 
                     param=list(normalize = "center", method="Cosine", k=350))

ibcf1_results <- predict(model_IBCF, getData(ibcf_scheme, "known"), type = "topNList")
s3load("ibcf_results.Rdata", bucket = "pred498engine", envir = ibcf_results)

ibcf1_results

rmse_IBCF <- calcPredictionAccuracy(prediction_IBCF, getData(ibcf_scheme, "unknown"), goodRating = 3, given = 5)[1]
rmse_IBCF #0.1410256 #this was all with set.seed(123), switched it to stay consistent with Brandons models
/'
s3load("ibcf_results.Rdata", bucket = "pred498engine")
ibcf_results
### Compare & Evaluate the IBCF Models
plot(ibcf_results, annotate=TRUE) #ROC curve

plot(ibcf_results, "prec/rec", annotate=TRUE)



#------------------------------------------------------------------------------
# Slope One Model https://github.com/tarashnot/SlopeOne
#------------------------------------------------------------------------------
#try slope one model
library(data.table)
library(devtools)
install_github(repo = "tarashnot/SlopeOne")
library("SlopeOne")
SO_ratings <- user_ratings[,c(1, 2, 6)]
SO_ratings <- data.table(SO_ratings)
names(SO_ratings) <- c("user_id", "item_id", "rating")
head(SO_ratings, 5)

SO_ratings[, user_id := as.character(user_id)]
SO_ratings[, item_id := as.character(item_id)]

setkey(SO_ratings, user_id, item_id)

#split train and test
set.seed(2017)

in_train <- rep(TRUE, nrow(SO_ratings))
in_train[sample(1:nrow(SO_ratings), size = round(0.2 * length(unique(SO_ratings$user_id)), 0) * 5)] <- FALSE

SO_ratings_train <- SO_ratings[(in_train)]
SO_ratings_test <- SO_ratings[(!in_train)]

#ratings normalization
SO_ratings_train_norm <- normalize_ratings(SO_ratings_train)

#build slope one model
model_SO <- build_slopeone(SO_ratings_train_norm$ratings)

#make predictions on test set, source code only has "ratings" prediction
SO_predictions <- predict_slopeone(model_SO, 
                                SO_ratings_test[ , c(1, 2), with = FALSE], 
                                SO_ratings_train_norm$ratings)

SO_unnormalized_predictions <- unnormalize_ratings(normalized = SO_ratings_train_norm, 
                                                ratings = SO_predictions)

#calculate RMSE on test set
rmse_slopeone <- sqrt(mean((SO_unnormalized_predictions$predicted_rating - SO_ratings_test$rating)^2))
rmse_slopeone #2.41168 not very good

#------------------------------------------------------------------------------
# Hybrid
#------------------------------------------------------------------------------
## mix popular movies with a random recommendations for diversity and
## rerecommend some movies the user liked.
hybrid_scheme <- evaluationScheme(real_r_filtered_rows_cols,
                                method = "split", # random train/test scheme
                                train = 0.75,
                                k = 1,
                                given = 5,  # how many records for a test user will learn the model? 
                                goodRating = 3) # threshold for classification. Just above Median Total_rating_max10.

ev <- evaluate(hybrid_scheme, "hybrid", type="topNlist")
ptm = proc.time()

hybrid_rec <- HybridRecommender(
  Recommender(getData(hybrid_scheme, "train"), method = "POPULAR"),
  Recommender(getData(hybrid_scheme, "train"), method = "UBCF"),
  Recommender(getData(hybrid_scheme, "train"), method = "IBCF"),
  weights = c(.4, .3, .3)
)
proc.time() - ptm; rm(ptm)

p_hybrid <- predict(hybrid_rec, getData(hybrid_scheme, "known"), type = "topNList")
error_hybrid<-calcPredictionAccuracy(p_hybrid, getData(hybrid_scheme, "unknown"), given = 5, goodRating = 3)
error_hybrid
