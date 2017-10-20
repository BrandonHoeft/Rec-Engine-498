library(aws.s3) # https://github.com/cloudyr/aws.s3
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(pryr) # memory usage functions

# specify keys as environment variables so I can read my s3 object(s) from AWS.
# Your unique access key/secret needs to be passed before running the queries below. 
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "")

################################################################################
# Read obfuscatedItems_10_17_17.txt & obfuscatedWebActivity_10_16_17.txt from S3 ------

# items data: part number, parent, catalogue, attributes/values.
# parse warning, but get identical results if using read.table and comparing results with all.equal()
items <- s3read_using(FUN = read_table2, 
                      object = "obfuscatedItems_10_17_17.txt", 
                      col_names = TRUE,
                      col_types = "cciciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii",
                      bucket = "pred498team5")

# user click data from the company website for a random day of user's selected,
# their activity for past 3 months, and click summaries of how they interacted with parts. 
users <- s3read_using(FUN = read_table2, 
                      col_names = TRUE,
                      col_types = "ccici",
                      object = "obfuscatedWebActivity_10_16_17.txt", 
                      bucket = "pred498team5")

# left join the parent family to the users PartNumber
user_items <- users %>%
  left_join(items, by = "PartNumber") %>%
  mutate(ActionDate = ymd(ActionDate)) %>% # parse into a date format.
  select(-starts_with("Val"), -starts_with("Attr"), -starts_with("Catalog"))

all.equal(users[1:3], user_items[1:3]) # users matches the first 3 columns after join. 
remove(users)
remove(items)

# save an in-memory R object into S3
s3save(user_items, bucket = "pred498team5", object = "user_items.Rdata")

################################################################################



################################################################################
# load the user_items.Rdata object from S3. ------------------------------------
# This is the data wrangled from lines 14-44. Loads the user_items dataframe. 
s3load("user_items.Rdata", bucket = "pred498team5")


################################################################################



################################################################################
# Pre-process the Ratings data in user_items -----------------------------------
# see explore_users_parts_data.R, lines 206 - 262 for analysis.

# create labels for ActionId
user_items <- user_items %>%
  # convert raw ActionId into labeled values. 
  mutate(ActionId_label = factor(user_items$ActionId, 
                                 labels = c("add to order", "select Part",
                                            "select Part detail",
                                            "print detail",
                                            "save CAD drawing detail",
                                            "print CAD drawing detail")),
         # categorize the 6 actions into 3 ratings, based on similarity.
         # order from lowest to highest implicit rating. implicit to a purchase event.
         # 1 = basic, 2 = moderate, 3 = close to a purchase or high interest to visitor. 
         action_rating = if_else(ActionId_label == "select Part", 1, 
                                 if_else(ActionId_label == "select Part detail", 2, 3)))

user_items %>%
  group_by(ActionId, ActionId_label) %>%
  summarize(frequency = n()) %>%
  arrange(ActionId)

user_items %>%
  group_by(action_rating) %>%
  summarize(frequency = n())

# select a sample of Visitor's and their user activity.
preview <- user_items %>%
  select(VisitorId, Parent, ActionDate, starts_with("Action")) %>%
  arrange(VisitorId, Parent, ActionDate) %>%
  slice(1:1000)
View(preview)

# Analyze each Visitor Session with a Parent Part#. 
preview2 <- preview %>%
  # create window for each visitor's session with a Parent Part#, each day. 
  group_by(VisitorId, Parent, ActionDate) %>%
  # w/in window, keep row of max action_rating for that specific session. 
  filter(action_rating == max(action_rating)) %>%
  arrange(VisitorId, Parent, ActionDate) %>%
  # w/in window, weight the rating by a factor of X times they performed max action_rating.
  # ex. if the highest rating for that session was 1 (selected part),
  # but did it 4 times during that day's session, then weighted rating = 1 x 4.
  mutate(wt_action_rating = action_rating * ActionCount) %>%
  ungroup()

# per Parent per Visitor, sum the wt_action_rating across days. 
# Intuition: If the visitor has had repeat actions with the Parent over many 
# different sessions/days, we can interpret that as they rate that part very highly. 

user_items %>%
  filter(ActionId == 1 & ActionCount > 1)

################################################################################