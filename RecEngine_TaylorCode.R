#set working directory
setwd("/Users/Taylor")
setwd("/Users/Taylor/Google Drive/PRED498_RecEngGrp") 

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

library(aws.s3) # https://github.com/cloudyr/aws.s3
library(readr)
library(dplyr)
library(tidyr)
library(Matrix)


# specify keys as environment variables so I can read my s3 object(s) from AWS.
# Your unique access key/secret needs to be passed before running the queries below. 
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAIEDTLZQEO3XY3RQQ",
           "AWS_SECRET_ACCESS_KEY" = "ybxfOsHYxx5Cef8mmlRdXOReinfJJg8FqvEb3O1j")

install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

libs <- c("bestglm","graphics","forecast","plyr","data.table","Deducer",
          "dplyr", "desc", "lattice", "pander", "stats", "knitr", "corrplot",  
          "ggplot2", "rattle", "caret","rpart","Hmisc","rpart.plot", "sm", "psych", 
          "ModelMetrics", "stats", "randomForest", "gbm", "lubridate", "readr", "reshape2", "reshape")
install(libs)
#lapply(libs, require, character.only = TRUE)

# items data: part number, parent, catalogue, attributes/values.
items <- s3read_using(FUN = read_table2, 
                      object = "obfuscatedItems.txt", 
                      col_names = TRUE,
                      col_types = "cciciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii",
                      bucket = "pred498engine")


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

# How many unique partnumbers are there per parent? ---------------------------

PartNumbercounts <- items %>%
  group_by(Parent) %>%
  distinct(PartNumber) %>% #only keep distinct zipcode per customer
  summarise(unique_PartNumber_count = n()) %>% # return count of unique zipcodes per user 
  as.data.frame

summary(PartNumbercounts$unique_PartNumber_count)
histogram(PartNumbercounts$unique_PartNumber_count) #majorly skewed right

#create matrix of partnumber, attr_value, and count= 1 ---------------------------------------------------
# tried this with the first 5 Attribute/value columns to start
items_long <- items %>%
  select(PartNumber, Attr1:Val8) %>%
  # tidy the different attributes columns into a single column.
  gather(attribute_position, attribute_number, starts_with("Attr"), -PartNumber, 
         na.rm = TRUE) %>%
  # tidy the different values columns into a single column. 
  gather(value_position, value_number, starts_with("Val"), - PartNumber,
         na.rm = TRUE) %>%
  # strip 'Attr' and 'Val' from the position columns that identified the original column position.
  mutate(attribute_position =  gsub("[^0-9]", "", attribute_position),
         value_position = gsub("[^0-9]", "", value_position)) %>%
  # keep columns where the Attr# == Val# to map the original attr:val relationships.
  filter(attribute_position == value_position) %>%
  select(PartNumber, attribute_number, value_number) %>%
  unite(attr_value, attribute_number, value_number, sep = ":") %>%
  mutate(count = 1)


# using sparseMatrix from Matrix package 
# https://stackoverflow.com/questions/28430674/create-sparse-matrix-from-data-frame?noredirect=1&lq=1
# the VisitorId and Parent need to be 1 based indices when creating a matrix. 
# Per ?factor, the levels of a factor are by default sorted.
m <- sparseMatrix(i = as.integer(as.factor(items_long$PartNumber)),
                  j = as.integer(as.factor(items_long$attr_value)),
                  x = items_long$count)
mm <- as.matrix(m)
attr_valMartix <- as.data.frame(m)
# How many attributes are associated with each PartNumber? ---------------------------

attr_valueNumbercounts <- items_long %>%
  group_by(PartNumber) %>%
  distinct(attr_value) %>% #only keep distinct zipcode per customer
  summarise(unique_attr_value_count = n()) %>% # return count of unique zipcodes per user 
  as.data.frame


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
