if(!require(aws.s3)) {
  message("installing the aws.s3 package to connect to S3 file system.")
  install.packages("aws.s3")
}

library(aws.s3) # https://github.com/cloudyr/aws.s3
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)


# specify keys as environment variables so I can read my s3 object(s) from AWS.
# Your unique access key/secret needs to be passed before running the queries below. 
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "")

# load dataset from the bucket as a csv ----------------------------------------

# items data: part number, parent, catalogue, attributes/values.
items <- s3read_using(FUN = read_tsv, 
                      col_names = TRUE,
                      col_types = "cciciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii",
                      object = "obfuscatedItems.txt", 
                      bucket = "pred498team5")

# there appear to be some duplicate PartNumbers (ex. M10027855) b/c appear on 2+ catalog pages.
# To remove duplicates, within each PartNumber/Parent combination, sort so that 
# the duplicate with the most item details is slotted first. Then use the 
# distinct() to keep first duplicate within sorted PartNumber/Parent combo.
items <- items %>%
  mutate(row_element_val_count = rowSums(!is.na(.))) %>%
  group_by(PartNumber, Parent) %>%
  arrange(desc(row_element_val_count)) %>%
  # remove doops in PartNumber, Parent, keeping doop w/most complete data. 
  distinct(PartNumber, Parent, .keep_all = TRUE) %>%
  ungroup()

# user click data from the company website and how they interacted with parts. 
users <- s3read_using(FUN = read_tsv, 
                      col_names = TRUE,
                      col_types = "ccici",
                      object = "obfuscatedWebActivity.txt", 
                      bucket = "pred498team5")

# left join the parent group to the users part number.
user_items <- users %>%
  left_join(items, by = "PartNumber") %>%
  mutate(ActionDate = ymd(ActionDate)) %>% # parse into a date format.
  select(-starts_with("Val"), -starts_with("Attr"), -starts_with("Catalog"), 
         -row_element_val_count) # only keep user fields, and 'Parent' from items.

all.equal(users[1:3], user_items[1:3]) # users matches the first 3 columns after join. 
remove(users)
remove(items)

################################################################################
## Exploratory Analysis about Users, PartNumbers, Parent Families.
################################################################################


# How many Different Users are in the dataset? ---------------------------------
user_items %>%
  distinct(VisitorId) %>% 
  summarize(n())

# How many distinct PartNumber's are in the dataset? ---------------------------
user_items %>%
  distinct(PartNumber) %>% 
  summarize(n())

# How many distinct Parent Family's of PartNumbers are in the dataset? ---------
user_items %>%
  distinct(Parent) %>% 
  summarize(n())


# How do total Action Counts of each different users distribute? ---------------

user_items %>%
  group_by(VisitorId) %>%
  summarize(activity_count = sum(ActionCount)) %>%
  with(summary(activity_count))

user_items %>%
  group_by(VisitorId) %>%
  summarize(activity_count = sum(ActionCount)) %>%
  ggplot(aes(activity_count, y = ..density..)) + 
  geom_histogram(binwidth = 100, colour = "black", fill = "darkgrey") +
  # median vertical line.
  geom_vline(aes(xintercept = median(activity_count)),
             color = "blue", linetype = "dashed", size = 0.5) +
  geom_text(aes(0,.002, label = paste("median = ", median(activity_count))),
            nudge_x = 625, color = "blue", size = 4.5) +
  labs(title = 'Distribution of Total interactions with Parts',
       subtitle = 'For each Visitor')

# How many unique PartNumbers are users interacting with? ----------------------

user_items %>%
  group_by(VisitorId) %>%
  distinct(PartNumber) %>% # only keep distinct part numbers per visitor.
  summarize(unique_part_count = n()) %>% # get count within the group. 
  with(summary(unique_part_count))

user_items %>%
  group_by(VisitorId) %>%
  distinct(PartNumber) %>% # only keep distinct part numbers per visitor.
  summarize(unique_part_count = n()) %>% # get count within the group. 
  ggplot(aes(unique_part_count, y = ..density..)) + 
  geom_histogram(binwidth = 50, colour = "black", fill = "darkgrey") +
  # median vertical line.
  geom_vline(aes(xintercept = median(unique_part_count)),
             color = "blue", linetype = "dashed", size = 0.5) +
  geom_text(aes(0,.004, label = paste("median = ", median(unique_part_count))), 
            nudge_x = 250, color = "blue", size = 4.5) +
  labs(title = 'Distribution of Unique PartNumber Interactions',
       subtitle = 'For each Visitor')

# How many unique Parent Family's are users interacting with? ------------------
# expect this to be less per user typically than the different PartNumbers 
# they may interact with since Parent is a higher level grouping. 

user_items %>%
  group_by(VisitorId) %>%
  distinct(Parent) %>% # only keep distinct parent per visitor.
  summarize(unique_parent_count = n()) %>% # get count within the group. 
  with(summary(unique_parent_count))

user_items %>%
  group_by(VisitorId) %>%
  distinct(Parent) %>% # only keep distinct parent per visitor.
  summarize(unique_parent_count = n()) %>% # get count within the group. 
  ggplot(aes(unique_parent_count, y = ..density..)) + 
  geom_histogram(binwidth = 50, colour = "black", fill = "darkgrey") +
  # median vertical line.
  geom_vline(aes(xintercept = median(unique_parent_count)),
             color = "blue", linetype = "dashed", size = 0.5) +
  geom_text(aes(0,.004, label = paste("median = ", median(unique_parent_count))), 
            nudge_x = 150, color = "blue", size = 4.5) +
  labs(title = 'Distribution of Unique Parent Interactions',
       subtitle = 'For each Visitor')

# How many unique users have interacted with the PartNumber? ----------------
# Very Few generally!
user_items %>%
  group_by(PartNumber) %>%
  distinct(VisitorId) %>% # only keep distinct visitors per Item.
  summarize(unique_users_count = n()) %>%
  with(summary(unique_users_count))

user_items %>%
  group_by(PartNumber) %>%
  distinct(VisitorId) %>% # only keep distinct visitors per Item.
  summarize(unique_users_count = n()) %>%
  ggplot(aes(unique_users_count)) + # modal value is around log(2) or 2 items.
  geom_histogram(binwidth = 10, colour = "black", fill = "darkgrey") +
  # median vertical line.
  labs(title = 'Distribution of Unique User Count who interacted with PartNumber')

user_items %>%
  group_by(PartNumber) %>%
  distinct(VisitorId) %>% # only keep distinct visitors per Item.
  summarize(unique_users_count = n()) %>%
  ggplot(aes(log(unique_users_count))) + # modal value is around log(2) or 2 items.
  geom_density(colour = "black", fill = "darkgrey") +
  # median vertical line.
  labs(title = 'Natural log Distribution of Unique User Count who interacted with PartNumber')


# How many unique users have interacted with the Parent Family? ----------------

# Per Matt Definition of a "Parent": group similar products to a parent family. 
#   it is a hyperlink. the parent groups will vary. It's a way how individual 
#   merchandising managers decide to split stuff up.

# Because Parent Family's are a broader generalization than PartNumbers, we'd 
# expect to have more interactions within Parent Family than at the lower level
# of PartNumber. 

user_items %>%
  group_by(Parent) %>%
  distinct(VisitorId) %>% # only keep distinct visitors per Item.
  summarize(unique_users_count = n()) %>%
  with(summary(unique_users_count))

# the above is how many different users have interacted with a parent family.
# we see we get a little better coverage/less sparsity than per PartNumber.
# The Median is 12 unique users per Parent family. The median was
#   4 activity interactions per PartNumber (line 100-103)

user_items %>%
  group_by(Parent) %>%
  distinct(VisitorId) %>% # only keep distinct visitors per Item.
  summarize(unique_users_count = n()) %>%
  ggplot(aes(unique_users_count, y = ..density..)) + 
  geom_histogram(binwidth = 20, colour = "black", fill = "darkgrey") +
  # median vertical line.
  geom_vline(aes(xintercept = median(unique_users_count)),
             color = "darkgreen", linetype = "dashed", size = 0.5) +
  geom_text(aes(0, .01, label = paste("median = ", median(unique_users_count))),
            nudge_x = 150, color = "darkgreen", size = 4.5) +
  labs(title = 'Distribution of Unique User Count who interacted with Parent Family')

# How many PartNumbers role up to the Parent Family? ---------------------------

user_items %>%
  group_by(Parent) %>%
  distinct(PartNumber) %>% # remove duplicate part numbers within Parent Group. 
  summarize(unique_partnumber_count = n()) %>% # return count of unique items per parent. 
  with(summary(unique_partnumber_count))


################################################################################
## Exploratory Analysis About User Actions
################################################################################

# How do the different action types distribute across all interactions ? -------
user_items %>%
  group_by(ActionId) %>%
  summarize(frequency = n()) %>%
  arrange(ActionId)



user_items %>%
  distinct(VisitorId, PartNumber) %>%
  summarise(n())

user_items %>%
  distinct(VisitorId, Parent, ActionId) %>%
  summarise(n())

# Things to Think About:
# 1) How to rank different actions for a Visitor's interaction with a Parent or PartNumber?
# 2) How to handle duplicity per Visitor of a Parent or a PartNumber?
#     For example, if User #1, has repeat actions many times with the same Parent, 
#     do we just take the highest ActionID value (whatever is closest to a purchase)
#     or do we add a weight factor for repeats?
#     Maybe there can be 2 different model approaches here: 1 model just looks at the 
#     most material action the Visitor has had with a Parent. 1 model first weights
#     how many times they've interacted with that part as part of the implicit weighting.
#       This 2nd model, maybe needs to consider the action date, when figuring out 
#       a single session for a specific Parent group.