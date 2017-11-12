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

# The popular recommender system predictions from test data.
s3load("popular_test_recommendations_final.Rdata", bucket = "pred498finalmodel")
popular <- test_recommendations_final
popular <- popular %>%
  mutate(model_source = 'popular')
remove(test_recommendations_final)

# The best ubcf recommender system predictions from test data. 
s3load("ubcf_test_recommendations_final.Rdata", bucket = "pred498finalmodel")
ubcf <- test_recommendations_final
ubcf <- ubcf %>%
  mutate(model_source = 'ubcf_cosine_50nn')
remove(test_recommendations_final)

# combine/union the recommendations from the popular rec's and the ubcf rec's on test records ---------
full_test_recommendations <- full_join(popular, ubcf)


# work on a weighted ratings scheme between popular and ubcf (DEPRECATED. biased against new rec's only scored by 1 of the 2 systems) -------------------
temp <- full_test_recommendations %>%
  mutate(predicted_rating_weighted = ifelse(model_source == 'ubcf_cosine_50nn', predicted_rating * .5,
                                            predicted_rating * .5)) %>%
  arrange(VisitorId, predicted_parent, predicted_rating)

temp2 <- full_test_recommendations %>%
  # 60% weight on UBCF, 40% weight on Popular
  #mutate(predicted_rating_weighted = ifelse(model_source == 'ubcf_cosine_50nn', predicted_rating * .5,
  #                                          predicted_rating * .5)) %>%
  group_by(VisitorId, predicted_parent) %>%
  mutate(hybrid_predicted_rating = mean(predicted_rating)) %>%
  ungroup() %>%
  arrange(VisitorId, desc(hybrid_predicted_rating))


temp3 <- temp2 %>%
  group_by(VisitorId) %>%
  distinct(VisitorId, predicted_parent, hybrid_predicted_rating, true_positive_rating) %>%
  arrange(VisitorId, desc(hybrid_predicted_rating)) %>%
  slice(1:20) %>%
  ungroup()

#s3load("unknown_test_data_df.Rdata", bucket = "pred498finalmodel")
test_recommendations_final <- temp3 %>%
  left_join(unknown_test_data_df, by = c('VisitorId' = 'VisitorId',
                                         'predicted_parent' = 'parent_rec')) %>%
  select(-i, -j, -known_rating)

test_recommendations_performance <- test_recommendations_final %>%
  group_by(VisitorId) %>%
  summarize(TP = sum(!is.na(true_positive_rating)),
            test_precision = round(TP / n(), 2)) %>%
  ungroup() %>%
  arrange(desc(TP))

summary(test_recommendations_performance$TP)
summary(test_recommendations_performance$test_precision)





## First normalize each model's scores 0-1 -------------------------------------
ubcf_max_rating <- max(ubcf$predicted_rating, na.rm = TRUE)
ubcf_min_rating <- min(ubcf$predicted_rating, na.rm = TRUE)
ubcf_range <- ubcf_max_rating - ubcf_min_rating
popular_max_rating <- max(popular$predicted_rating, na.rm = TRUE)
popular_min_rating <- min(popular$predicted_rating, na.rm = TRUE)
popular_range <- popular_max_rating - popular_min_rating


#http://mathforum.org/library/drmath/view/60433.html
# y = 1 + (x-A)*(10-1)/(B-A). A = desired min, B = desired max

hybrid_norm <- full_test_recommendations %>%
  mutate(predicted_rating_norm = ifelse(model_source == 'ubcf_cosine_50nn',
                                        (predicted_rating - ubcf_min_rating) / ubcf_range,
                                        (predicted_rating - popular_min_rating) / popular_range),
         predicted_rating_10scale = ifelse(model_source == 'ubcf_cosine_50nn',
                                           1 + (predicted_rating - ubcf_min_rating) * (10-1) /(ubcf_range),
                                           1 + (predicted_rating - popular_min_rating) * (10-1) /(popular_range))) %>%
  arrange(VisitorId, predicted_parent, desc(predicted_rating_norm))


hybrid_norm2 <- hybrid_norm %>%
  group_by(VisitorId, predicted_parent) %>%
  mutate(hybrid_predicted_rating = max(predicted_rating_10scale)) %>%
  ungroup() %>%
  arrange(VisitorId, desc(hybrid_predicted_rating))

hybrid_norm3 <- hybrid_norm2 %>%
  group_by(VisitorId) %>%
  distinct(VisitorId, predicted_parent, hybrid_predicted_rating, true_positive_rating) %>%
  arrange(VisitorId, desc(hybrid_predicted_rating)) %>%
  slice(1:20) %>%
  ungroup()

s3load("unknown_test_data_df.Rdata", bucket = "pred498finalmodel")

ubcf_popular_hybrid_test_recommendations_final <- hybrid_norm3 %>%
  left_join(unknown_test_data_df, by = c('VisitorId' = 'VisitorId',
                                         'predicted_parent' = 'parent_rec')) %>%
  select(-i, -j, -known_rating)
#s3save(ubcf_popular_hybrid_test_recommendations_final, bucket = "pred498finalmodel", object = "ubcf_popular_hybrid_test_recommendations_final.Rdata")
#s3load("ubcf_popular_hybrid_test_recommendations_final.Rdata", bucket = "pred498finalmodel")

ubcf_popular_hybrid_test_recommendations_performance <- ubcf_popular_hybrid_test_recommendations_final %>%
  group_by(VisitorId) %>%
  summarize(TP = sum(!is.na(true_positive_rating)),
            test_precision = round(TP / n(), 2)) %>%
  ungroup() %>%
  arrange(desc(TP))
#s3save(ubcf_popular_hybrid_test_recommendations_performance, bucket = "pred498finalmodel", object = "ubcf_popular_hybrid_test_recommendations_performance.Rdata")
#s3load("ubcf_popular_hybrid_test_recommendations_performance.Rdata", bucket = "pred498finalmodel")

summary(ubcf_popular_hybrid_test_recommendations_performance$TP)
summary(ubcf_popular_hybrid_test_recommendations_performance$test_precision)


 