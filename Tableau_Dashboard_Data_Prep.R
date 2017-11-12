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

# Get the test user performance data from A top Candidate Model (UBCF-Popular Hybrid)----
s3load("ubcf_popular_hybrid_test_recommendations_performance.Rdata", bucket = "pred498finalmodel")

# Get the 7-months of web activity for all users. ------------------------------

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

# Get the parent labels associated with PartNumbers ----------------------------
#items: part number, parent, catalogue, attributes/values.
items <- s3read_using(FUN = read_table2, 
                      object = "obfuscatedItems_10_17_17.txt", 
                      col_names = TRUE,
                      col_types = "cciciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii",
                      bucket = "pred498team5")

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


# Use the Test user VisitorId index to filter the web activity data to these 
# relevant test users. 
test_user_activity <- user_items %>%
  inner_join(ubcf_popular_hybrid_test_recommendations_performance,
             by = 'VisitorId') %>%
  mutate(action_rating = if_else(ActionId_label == "select Part", 1, 
                                 if_else(ActionId_label == "select Part detail", 2, 3))) %>%
  select(-TP, -test_precision)

remove(items, users, user_items, users_aprmayjun17, users_febmar17, users_julaug17)

# Summarize Test User Data for Tableau Dashboard Prototype ---------------------

# Information about total clicks, parents, partnumbers, session days overall.
test_user_data1 <- test_user_activity %>%
  group_by(VisitorId) %>%
  summarize(total_clicks = sum(ActionCount),
            parent_count = length(unique(Parent)),
            partnumber_count = length(unique(PartNumber)),
            session_days = length(unique(ActionDate)))

# Information about total clicks within a session day. 
test_user_data2 <- test_user_activity %>%
  group_by(VisitorId, ActionDate) %>%
  summarize(total_clicks_per_session = sum(ActionCount)) %>%
  group_by(VisitorId) %>%
  summarize(avg_clicks_per_session = mean(total_clicks_per_session))

# Information about how many high implict rating events (action3) occur in a session.
test_user_data3 <- test_user_activity %>%
  group_by(VisitorId, ActionDate) %>%
  filter(action_rating == 3) %>%
  summarize(high_rating_actions_per_session = n()) %>%
  group_by(VisitorId) %>%
  summarize(avg_high_rating_actions_per_session = mean(high_rating_actions_per_session),
            sd_high_rating_actions_per_session = sd(high_rating_actions_per_session))

# Get information about days between sessions
test_user_data4 <- test_user_activity %>%
  group_by(VisitorId) %>%
  distinct(ActionDate) %>%
  arrange(VisitorId, ActionDate) %>%
  mutate(days_difference = difftime(ActionDate, lag(ActionDate, n = 1L), units = 'days')) %>%
  summarize(avg_days_between_session = as.numeric(mean(days_difference, na.rm = TRUE)),
            SD_days_between_session = as.numeric(sd(days_difference, na.rm = TRUE)))


test_user_summary <- inner_join(test_user_data1, test_user_data2, by = 'VisitorId') %>%
  inner_join(test_user_data3, by = 'VisitorId') %>%
  inner_join(test_user_data4, by = 'VisitorId') %>%
  inner_join(ubcf_popular_hybrid_test_recommendations_performance, by = 'VisitorId')

