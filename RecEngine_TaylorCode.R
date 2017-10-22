#set working directory
setwd("/Users/Taylor")
setwd("/Users/Taylor/Google Drive/PRED498_RecEngGrp") 

libs <- c("dprep","bestglm","graphics","forecast","plyr","data.table","Deducer",
          "dplyr", "desc", "lattice", "pander", "stats", "knitr", "corrplot", "MASS", 
          "ggplot2", "rattle", "caret","rpart","Hmisc","rpart.plot", "sm", "psych", 
          "ModelMetrics", "stats", "randomForest", "gbm", "lubridate", "readr", "reshape2", "reshape")
lapply(libs, require, character.only = TRUE)

# items data: part number, parent, catalogue, attributes/values.
items <- read.table("/Users/Taylor/Google Drive/PRED498_RecEngGrp/data/obfuscated 2/obfuscatedItems.txt",
                           header = TRUE, sep = "", fill = TRUE)

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
web_activity <- read.table("/Users/Taylor/Google Drive/PRED498_RecEngGrp/data/obfuscated 2/obfuscatedWebActivity.txt",
                           header = TRUE, sep = "")


# left join the parent group to the users part number.
user_items <- web_activity %>%
  left_join(items, by = "PartNumber") %>%
  mutate(ActionDate = ymd(ActionDate)) %>% # parse into a date format.
  select(-starts_with("Val"), -starts_with("Attr"), -starts_with("Catalog"), 
         -row_element_val_count) # only keep user fields, and 'Parent' from items.

all.equal(web_activity[1:3], user_items[1:3]) # web_activity matches the first 3 columns after join. 

user <- read.table("/Users/Taylor/Google Drive/PRED498_RecEngGrp/data//obfuscated 2/obfuscateUserData.txt", header=TRUE, 
                   sep="")

# How many zipcodes does each person have items shipped to? ---------------------------

zipcounts <- user %>%
  group_by(VisitorId) %>%
  distinct(Zip) %>% #only keep distinct zipcode per customer
  summarise(unique_zip_count = n()) %>% # return count of unique zipcodes per user 
  as.data.frame

histogram(~zipcounts$unique_zip_count)
bwplot(~zipcounts$unique_zip_count)


'/ Cant get this to work
m1 <- matrix(5:45, ncol=2)
lst <- split(m1, row(m1))
res <- reshape(items, idvar=c("PartNumber", "Parent", "CatalogNumber", "CatalogPage"), varying = lst, direction = "long")
/'


mlt <- melt(items, id.vars=c("PartNumber", "Parent", "CatalogNumber", "CatalogPage"))

items %>%
  gather(key, value, -PartNumber, -Parent, -CatalogNumber, -CatalogPage) %>%
  extract(key, c("attribute", "value"), "(Attr.)(Val.)") %>%
  spread(attribue, value)

Attr_names = colnames(items[, grepl("Attr", names(items))])
Attr_names
new_items <- reshape(items, varying = Attr_names, idvar = c("PartNumber", "Parent", "CatalogNumber", "CatalogPage"), 
        v.names=c("Attribute","Value"), direction = "long")
