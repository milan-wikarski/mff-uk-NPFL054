params.testSize <- 0.3


library(ISLR)

data <- College

# Randomly select indexes
indexes.all <- c(1: nrow(data))
indexes.test <- sample(indexes.all, as.integer(length(indexes.all) * params.testSize))
indexes.train <- indexes.all[is.na(pmatch(indexes.all, indexes.test))]

# Split data into train and test subsets
data.train <- data[indexes.train, ]
data.test <- data[indexes.test, ]

