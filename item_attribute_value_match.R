# Items with similar attributes 

# Objective: sorting through the items data to find items with matching attributes.
# A way to identy items with matching attributes could be a matrix or data frame
# where the rows represent unique items, and the columns represent each unique
# combination of attribute:value. 

# Toy Example with Simulated Data. --------------------------------------------

library(tidyr)
library(dplyr)
install.packages("slam")
#library(slam) # for sparse matrices


set.seed(2)
temp_df <- data.frame(PartNumber = c(letters[10:19]),
                    Attr1 = sample(1:20, size = 10, replace = TRUE), 
                    Val1 = sample(90:100, size = 10, replace = TRUE), 
                    Attr2 = sample(1:20, size = 10, replace = TRUE), 
                    Val2 = sample(90:100, size = 10, replace = TRUE), 
                    Attr3 = sample(1:20, size = 10, replace = TRUE), 
                    Val3 = sample(90:100, size = 10, replace = TRUE), 
                    Attr4 = sample(1:20, size = 10, replace = TRUE), 
                    Val4 = sample(90:100, size = 10, replace = TRUE), 
                    Attr5 = sample(1:20, size = 10, replace = TRUE), 
                    Val5 = sample(90:100, size = 10, replace = TRUE))


#items_attr_value_matrix <- 

df <- temp_df %>%
  # tidy the different attributes columns into a single column.
  gather(attribute_position, attribute_number, starts_with("attr"), -PartNumber) %>%
  # tidy the different values columns into a single column. 
  gather(value_position, value_number, starts_with("val"), - PartNumber) %>%
  # strip 'Attr' and 'Val' from the position columns that identified the original column position.
  mutate(attribute_position =  gsub("[^0-9]", "", attribute_position),
         value_position = gsub("[^0-9]", "", value_position)) %>%
  # keep columns where the Attr# == Val# to map the original attr:val relationships.
  filter(attribute_position == value_position) %>%
  select(PartNumber, attribute_number, value_number) %>%
  arrange(PartNumber) %>%
  # combine the attribute# and value# into a single measure.
  unite(attr_value, attribute_number, value_number, sep = ":") %>%
  spread(attr_value, attr_value) 


# a couple ways to convert into an indicator matrix:
df %>%
  mutate_at(2:ncol(.), funs(ifelse(!is.na(.), 1, .)))

df %>%
  mutate_at(2:ncol(.), function(x) {ifelse(!is.na(x), 1, x)}) 


# with the items dataset -------------------------------------------------------

library(aws.s3) # https://github.com/cloudyr/aws.s3
library(readr)
library(dplyr)
library(tidyr)


# specify keys as environment variables so I can read my s3 object(s) from AWS.
# Your unique access key/secret needs to be passed before running the queries below. 
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "")

items <- s3read_using(FUN = read_table2, 
                      object = "obfuscatedItems_10_17_17.txt", 
                      col_names = TRUE,
                      col_types = "cciciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii",
                      bucket = "pred498team5")


# tried this with the first 5 Attribute/value columns to start
items_long <- items %>%
  select(PartNumber, Attr1:Val5) %>%
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

# Errors generate due to size of object.
items_long %>%
  spread(attr_value, attr_value) %>%
  mutate_at(2:ncol(.), function(x) {ifelse(!is.na(x), 1, x)}) %>%
  as.matrix()

# using sparseMatrix from Matrix package 
# https://stackoverflow.com/questions/28430674/create-sparse-matrix-from-data-frame?noredirect=1&lq=1
# the VisitorId and Parent need to be 1 based indices when creating a matrix. 
# Per ?factor, the levels of a factor are by default sorted.
m <- sparseMatrix(i = as.integer(as.factor(items_long$PartNumber)),
                         j = as.integer(as.factor(items_long$attr_value)),
                         x = items_long$count)

# can rename the matrix row and column labels with unique VisitorId and Parent names. 
dimnames(m) <- list(sort(unique(items_long$PartNumber)),
                    sort(unique(items_long$attr_value)))

# check that the order of factor levels and dimnames passed to indices are the same. 
all.equal(levels(as.factor(items_long$PartNumber)), # line 104
          sort(unique(items_long$PartNumber))) # line 110

