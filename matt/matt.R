library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(pryr) # memory usage functions
library(Matrix)
library(recommenderlab)


# specify keys as environment variables so I can read my s3 object(s) from AWS.
# Your unique access key/secret needs to be passed before running the queries below. 
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "")

# load dataset from the bucket as a csv ----------------------------------------

# items data: part number, parent, catalogue, attributes/values.
# parse warning, but get identical results if using read.table and comparing results with all.equal()

colTypes = c("character", "character", "integer", "character",  
  "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer",
  "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer",
  "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer",
  "integer","integer","integer","integer","integer","integer","integer","integer","integer","integer")



user_ratings = read.csv("/Users/haydude/Development/mspa/498 - Capstone/data/webActivityRated.csv", header = TRUE)

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



