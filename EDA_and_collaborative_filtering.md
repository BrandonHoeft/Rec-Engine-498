# EDA and Collaborative Filtering
Brandon Hoeft  
October 22, 2017  



## Introduction

This Markdown file has most of my code, relevant plots, and explanations of decisions made so far in the data wrangling process.

I will update it with collaborative filtering model information too, as needed. 

## Getting the data into R

I used the following packages to do most of the initial data pre-processing and exploratory analysis. I'm working on an AWS AMI instance that's a linux machine with RStudio and git installed, using one of the instances publicly available and pre-configured from [Louis Aslett's website](http://www.louisaslett.com/RStudio_AMI/). I initially started with the free tier, *t2.micro* instance, but once I started the modeling phase, I scaled up to *m4.large*, which is also a general purpose computing instance but with better RAM, CPU, and better optimized memory management. AWS AMI instance costs can be calculated [here](https://calculator.s3.amazonaws.com/index.html). 

The datasets queried by Matt Hayden, I've put in my own S3 Bucket on Amazon Web Services (AWS). As such, the `aws.s3` [package](https://github.com/cloudyr/aws.s3) was used to access that bucket and bring the datasets into my environment. Access Keys are needed to make read/write calls to the bucket, but I've intentionally left that code out of here. 


```r
library(aws.s3) 
library(readr) # faster alternatives to base read. methods. 
library(lubridate) # date wrangling. 
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # data viz. 
library(pryr) # mem_used() and object_size() functions to manage/understand memory usage.
```

To read the Part items data and the user webactivity data from s3 I passed read_table2() functions for reading tabular data into the `aws.s3` functions for reading objects from AWS. 




```r
# items: part number, parent, catalogue, attributes/values.
items <- s3read_using(FUN = read_table2, 
                      object = "obfuscatedItems_10_17_17.txt", 
                      col_names = TRUE,
                      col_types = "cciciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii",
                      bucket = "pred498team5")

# users: click data from the company website for a random day of user's selected, their activity for past 3 months and click summaries of how they interacted with parts. 
users <- s3read_using(FUN = read_table2, 
                      col_names = TRUE,
                      col_types = "ccici",
                      object = "obfuscatedWebActivity_10_16_17.txt", 
                      bucket = "pred498team5")
```

## Wrangling the Dataset 

The only information I'm particularly interested in from the **items** dataset is the Parent column, and left joining that to the matching PartNumber in the users data. Building a recommender system using the Parent family instead of the specific PartNumber will be discussed later. 


```r
# left join the parent family to the users PartNumber
user_items <- users %>%
  left_join(items, by = "PartNumber") %>%
  mutate(ActionDate = ymd(ActionDate)) %>% # parse into a date format.
  select(-starts_with("Val"), -starts_with("Attr"), -starts_with("Catalog"))

# Check that left join worked and initial columns match identically. 
all.equal(users[1:3], user_items[1:3])
```

```
[1] TRUE
```

```r
remove(users)
remove(items)
```

### Initial EDA

With a baseline dataset, I can start exploring some information about the users. I'll refer to them as visitors, since these are web click data about how visitors/customers of a manufacturing supplier are interacting with the company's website and mobile application. 

We'll start by asking some questions of the data. The web activity covered by these data begins on 2017-03-24 and goes through 2017-07-08, covering a span of 106. Initial analysis shows that there are 5895 unique VisitorID numbers in this dataset. 


```r
# How many Different Users are in the dataset? 
unique(user_items$VisitorId) %>% length()
# How many distinct PartNumber's are in the dataset? 
unique(user_items$PartNumber) %>% length()
# How many distinct Parent Family's of PartNumbers are in the dataset? 
unique(user_items$Parent) %>% length()

max(user_items$ActionDate) 
min(user_items$ActionDate)
max(user_items$ActionDate) - min(user_items$ActionDate)
```
