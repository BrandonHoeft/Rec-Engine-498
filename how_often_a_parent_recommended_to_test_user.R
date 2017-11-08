library(aws.s3) 
library(readr) # faster alternatives to base read. methods. 
library(lubridate) # date wrangling. 
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # data viz. 
library(pryr) # mem_used() and object_size() functions to manage/understand memory usage.


#Sys.setenv("AWS_ACCESS_KEY_ID" = "",
#           "AWS_SECRET_ACCESS_KEY" = "")

s3load("ubcf_test_recommendations_final.Rdata", bucket = "pred498finalmodel") # ubcf_test_recommendations_final.csv on google shared drive. 

total_users <- length(unique(test_recommendations_final$VisitorId))

# Provides stats per recommended parent in the test data, how many different users received that recommendation.
ubcf_popular_rec <- test_recommendations_final %>%
  filter(!is.na(predicted_parent)) %>%
  group_by(predicted_parent) %>%
  summarize(users_recommended_to = n(),
            proportion_of_users = round(users_recommended_to / total_users, 2)) %>%
  arrange(desc(users_recommended_to))

#write_csv(test_recommendations_final, "UBCF Most Popular Recommendations.csv")  # also uploaded separately on google drive 11/7/17
