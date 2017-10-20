if(!require(aws.s3)) {
  message("installing the aws.s3 package to connect to S3 file system.")
  install.packages("aws.s3")
}


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

# load dataset from the bucket as a csv ----------------------------------------

# items data: part number, parent, catalogue, attributes/values.
# parse warning, but get identical results if using read.table and comparing results with all.equal()
items <- s3read_using(FUN = read_table2, 
                      object = "obfuscatedItems_10_17_17.txt", 
                      col_names = TRUE,
                      col_types = "cciciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii",
                      bucket = "pred498team5")

# all rows are unique PartNumbers.
length(unique(items$PartNumber))  == nrow(items)

# user click data from the company website for a random day of user's selected,
# their activity for past 3 months, and click summaries of how they interacted with parts. 
users <- s3read_using(FUN = read_table2, 
                      col_names = TRUE,
                      col_types = "ccici",
                      object = "obfuscatedWebActivity_10_16_17.txt", 
                      bucket = "pred498team5")

# left join the parent group to the users part number.
user_items <- users %>%
  left_join(items, by = "PartNumber") %>%
  mutate(ActionDate = ymd(ActionDate)) %>% # parse into a date format.
  select(-starts_with("Val"), -starts_with("Attr"), -starts_with("Catalog"))

all.equal(users[1:3], user_items[1:3]) # users matches the first 3 columns after join. 
remove(users)
remove(items)

################################################################################
## Exploratory Analysis about Users, PartNumbers, Parent Families.
################################################################################


# How many Different Users are in the dataset? ---------------------------------
unique(user_items$VisitorId) %>% length()


# How many distinct PartNumber's are in the dataset? ---------------------------
unique(user_items$PartNumber) %>% length()

# How many distinct Parent Family's of PartNumbers are in the dataset? ---------
unique(user_items$Parent) %>% length()


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
## Data Wrangling of the User Actions 
################################################################################

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
         ActionId_3cat = if_else(ActionId_label == "select Part", 1, 
                                 if_else(ActionId_label == "select Part detail", 2, 3)))

################################################################################
## Exploratory Analysis of the User Actions 
################################################################################

# How do the different action types distribute across all interactions ? -------
user_items %>%
  group_by(ActionId, ActionId_label) %>%
  summarize(frequency = n()) %>%
  arrange(ActionId)

# Bar graph of the 6 different Action ID Labels
user_items %>%
  group_by(ActionId, ActionId_label) %>%
  summarize(frequency = n()) %>%
  ggplot(aes(x = reorder(ActionId_label, desc(frequency)), y = frequency)) +
    geom_bar(aes(fill = ActionId_label), stat = "identity") +
    labs(title = "Distribution of 6 different Actions taken by Users",
         x = NULL) +
    guides(fill=FALSE) + # remove legend
    theme(axis.text.x=element_text(angle=25,hjust=1)) + # tilt x labels
    scale_y_continuous(labels = function(n) format(n, scientific = FALSE)) # raw frequency scale


user_items %>%
  group_by(ActionId_3cat) %>%
  summarize(frequency = n())

# Bar graph of the condensed categories from 6 to 3. To be used for ratings matrix.
user_items %>%
  group_by(ActionId_3cat) %>%
  summarize(frequency = n()) %>%
  ggplot(aes(x = ActionId_3cat, y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Implicit Action Rating taken by Users",
       subtitle = "1 = low, 2 = medium, 3 = high rating",
       x = NULL) +
  guides(fill=FALSE) + # remove legend
  scale_y_continuous(labels = function(n) format(n, scientific = FALSE)) # raw frequency scale



# Things to Think About --------------------------------------------------------
# a) How to rank different actions for a Visitor's interaction with a Parent or PartNumber?
#   Answer: actions #1,4,5,6 should be treated as equal weight. then #3 next important, and #2 is basic. 
# b) How to handle duplicity of a Parent or a PartNumber for a Visitor?
#     For example, if User #1, has repeat actions many times with the same Parent, 
#     do we just take the highest ActionID value (whatever is closest to a purchase)?
#     Do we add a weight factor for repeat sessions with that exact Parent number?

#   Answer: Maybe there can be 3 different model approaches here for ratings structure: 

#     1) unary rating: whether or not a visitor interacted with an item within the 3 month span.
#       1 = interaction, missings filled with 0.

#     2) model just looks at the most material action the Visitor has had with a 
#     Parent ever across all separate interactions with that parent and goes with that rating.
#     example: 1 (ActionID = 2), 2 (ActionID = 3), 3 (Action ID includes(1,4,5,6))

#     3) the model takes repeat sessions with a Parent Number into account in weighting the
#     implicit rating value. This maybe needs to consider the action date, when figuring out 
#     a single session for a specific Parent number for that visitor.
#     For each time that the visitor has interacted with the specific Parent Family on a given day:
#     step 1) take the value of the highest Action ID by the visitor for that Parent Family.
#     step 2) multiply it by its Action Count. 
#     step 3) We now have this value on a daily basis per Parent per Visitor. Sum the values up
#       per Parent per Visitor (summing values across days). Intuition: If the visitor 
#       has had repeat actions with the Parent over many different sessions/days, 
#       we can interpret that as they rate that part very highly. 
#     step 4) Some ratings will be very high, so we need a function to cap or limit the 
#       upper bound rating (ex. log scale transform) or maybe just cap it at a cutoff
#       value that makes sense given the distribution of the weighted ratings per Parent Family per User.