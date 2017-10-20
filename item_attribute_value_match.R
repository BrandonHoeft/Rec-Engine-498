# Items with similar attributes 

# Objective: sorting through the items data to find items with matching attributes.
# A way to identy items with matching attributes could be a matrix or data frame
# where the rows represent unique items, and the columns represent each unique
# combination of attribute:value. 

# Toy Example with Simulated Data. --------------------------------------------

library(tidyr)
library(dplyr)

set.seed(2)
items <- data.frame(PartNumber = c(letters[10:19]),
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

df <- items %>%
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
  spread(attr_value, attr_value, fill = 0) 

df %>%
  mutate_at(2:ncol(.), funs(ifelse(. != 0, 1, .)))

df %>%
  mutate_at(2:ncol(.), funs(ifelse(is.na(.), 0, 1)))


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
indicator_matrix <- items %>%
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
  unite(attr_value, attribute_number, value_number, sep = ":") 

%>%
  spread(attr_value, attr_value) %>%
  mutate_at(2:ncol(.), funs(ifelse(is.na(.), 0, 1)))
  
