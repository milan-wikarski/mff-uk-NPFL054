inc <- function(x) {
 eval.parent(substitute(x <- x + 1))
}

binary.evaluation <- function(predictedValues, trueValues) {
  # Convert predicted and true values to vectors
  predictedValues <- as.vector(predictedValues)
  trueValues <- as.vector(trueValues)

  # Check the length
  if (length(predictedValues) != length(trueValues)) {
    stop("[predictedValues] and [trueValues] are of different length")
  }

  res <- list()

  res$prediction <- predictedValues
  res$truth <- trueValues

  res$confusion.matrix <- matrix(data = c(0, 0, 0, 0), nrow = 2, ncol = 2)

  dimnames(res$confusion.matrix) <- list(
    c("Truly positive", "Truly negative"),
    c("Predicted positive", "Predicted negative")
  )

  for (i in 1:length(predictedValues)) {
    inc(res$confusion.matrix[
      ifelse(trueValues[i] == 1, 1, 2),
      ifelse(predictedValues[i] == 1, 1, 2)
    ])
  }

  res$true.positive  <- res$confusion.matrix[1, 1]
  res$false.positive <- res$confusion.matrix[2, 1]
  res$false.negative <- res$confusion.matrix[1, 2]
  res$true.negative  <- res$confusion.matrix[2, 2]

  res$precision <- res$true.positive / (res$true.positive + res$false.positive)
  # TODO: Is this a thing??
  # res$precision.negative <- res$true.negative / (res$true.negative + res$false.negative)
  res$recall <- res$true.positive / (res$true.positive + res$false.negative)
  res$specificity <- res$true.negative / (res$true.negative + res$false.positive)

  res$accuracy <- (res$true.positive + res$true.negative) / sum(res$confusion.matrix)

  return(res)
}

.libPaths("~/R/libs/")
# install.packages("ISLR")
install.packages("class")
library(ISLR)
library(class)

d <- Caravan
attach(Caravan)

# Convert Purchase feature to int
d$Purchase <- ifelse(d$Purchase == "Yes", 1, 0)

TEST_SUBSET_SIZE <- 0.5

# Randomly divide indexes 1:nrow(d) into test and train subsets
indexes.all <- c(1:nrow(d))
indexes.test <- sample(c(1:nrow(d)), nrow(d) * TEST_SUBSET_SIZE, F)
indexes.train <- indexes.all[is.na(pmatch(indexes.all, indexes.test))]

# Use indexes to get test subset and train subset
test <- d[indexes.test, ]
train <- d[indexes.train, ]

# Always predict the most frequent value
trivial.predict <- as.integer(ifelse(table(train$Purchase)[1] > table(train$Purchase)[2], 0, 1))
message(paste("\nTrivial model: always predict", trivial.predict))

# Evaluate trivial classifier and print the results
(trivial.evaluation <- binary.evaluation(rep(trivial.predict, nrow(test)), test$Purchase))

log <- glm(Purchase ~ ., data = train, family = binomial(link = "logit"))

log.probs <- predict(log, test, type = "response")

log.pred <- rep(0, nrow(test))
log.pred[log.probs > 0.25] <- 1

(binary.evaluation(log.pred, test$Purchase))

train.X <- train[, c(1:85)]
train.Y <- train[, 86]

test.X <- test[, c(1:85)]
test.Y <- test[, 86]

for (i in 1:10) {
  k <- binary.evaluation(knn(train.X, test.X, train.Y, k = i), test.Y)
  print(c(i, k$precision))
}