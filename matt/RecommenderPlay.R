library(recommenderlab)
library(recosystem)
library(devtools)

#install_github(repo = "SlopeOne", username = "tarashnot/SVDApproximation")
#install_github(repo = "SVDApproximation", username = "tarashnot/SVDApproximation")

library(SlopeOne)
library(SVDApproximation)
library(data.table)
library(RColorBrewer)
library(ggplot2)

data(ratings)
head(ratings)

visualize_ratings(ratings_table = ratings)
