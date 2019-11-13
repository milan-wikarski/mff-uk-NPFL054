## PARAMETERS ##
params.fileOutput <- TRUE
params.workDir <- "~/School/NPFL054/cars/"
params.libPaths <- "~/R/libs"
params.seed <- 4356



#########################################
##          HELPER FUNCTIONS           ##
#########################################
inc <- function(x) {
 eval.parent(substitute(x <- x + 1))
}

entropy <- function(p) {
  if (sum(p) == 0) {
    return(0)
  }

  p <- p / sum(p)
  p <- p[p > 0] # Discard zero entries

  H = -sum(p*log(p,base=2))

  return(H)
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

  # res$prediction <- predictedValues
  # res$truth <- trueValues

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
  res$recall <- res$true.positive / (res$true.positive + res$false.negative)
  res$specificity <- res$true.negative / (res$true.negative + res$false.positive)

  res$F.score <- 2 * res$precision * res$recall / (res$recall + res$precision) 

  res$accuracy <- (res$true.positive + res$true.negative) / sum(res$confusion.matrix)
  res$error <- 1 - res$accuracy

  return(res)
}


#########################################
##              PART 00                ##
#########################################

# Set seed
set.seed(params.seed)

# Setup libs directory
.libPaths(params.libPaths)

if (!("rpart" %in% rownames(installed.packages()))) {
  install.packages("rpart")
}

if (!("rpart.plot" %in% rownames(installed.packages()))) {
  install.packages("rpart.plot")
}

if (!("ISLR" %in% rownames(installed.packages()))) {
  install.packages("ISLR")
}

library(rpart)
library(rpart.plot)
library(ISLR)

# Initiate global variables
global.colors <- c()
while (length(global.colors) < 50) {
  global.colors <- c(global.colors, paste("#", paste(sample(c(0:9, LETTERS[1:6]), 6, T), collapse = ""), sep = ""))
}

# Set working directry and load data
setwd(params.workDir)

# Global chart parameters
par(cex = 1.5)

# Create /out directory
if (params.fileOutput && !dir.exists("out")) {
  dir.create("out")
}

# Load data
data <- Auto
attach(data)



#########################################
##              PART 01                ##
#########################################

#
# (0): Simple linear regression
#

# All numerical features (= all features except `name`)
cor.names <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year", "origin")

# Calculate the correlation across all features in cor.names
cor <- as.data.frame(cor(data[, cor.names]))

if (params.fileOutput) {
  pdf("out/simple-linear-regression.pdf", width = 20, height = 10)
}

par(mfrow = c(2, 4), mar = c(8, 5, 8, 5))

for (name in cor.names) {
  # Plot values of all features in cor.names againts mpg
  plot(
    mpg ~ data[, c(name)],
    ylab = "mpg",
    xlab = name,
    main = paste("Linear regression\nmpg ~", name, "\nr =", round(cor[, c(name)][1], digits = 5))
  )

  # Create a simple linear model for each feature in cor.names
  model <- lm(mpg ~ data[, c(name)])

  # Print the summary of the linear model
  message(paste("\n\n\nSummary of mpg ~", name, "\n------------------------------------------------------------"))
  print(summary(model))
  message("------------------------------------------------------------")

  # Plot the regression line for each feature in cor.names
  abline(a = model$coefficients[1], b = model$coefficients[2])
}

if (params.fileOutput) {
  dev.off()
}

#
# (1): Multiple linear regression
#

# Create the linear model
model <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + 
origin)

if (params.fileOutput) {
  pdf("out/multiple-linear-regression-residuals.pdf", width = 20, height = 10)
}

par(mfrow = c(1, 2), mar=c(8, 8, 8, 8))

# Print historgram of residuals to visualize the distribution
hist(
  model$residuals,
  main = "Multiple linear regression\nResiduals histogram",
  xlab = "error"
)

# Print Q-Q plot of residuals to visually chceck if they are normally distributed
qqnorm(
  model$residuals,
  main = "Multiple linear regression\nNormal Q-Q Plot of residuals"
)

if (params.fileOutput) {
  dev.off()
}

# Get and analyse the summary of the model
summary(model)

#
# (2): Polynomial regression
#

DEGREES_COUNT <- 5

# Sort the records by acceleration
acceleration.sorted <- data[order(data$acceleration), c("acceleration")]

if (params.fileOutput) {
  pdf("out/polynomial-regression.pdf", width = 20, height = 10)
}

par(mfrow = c(1, 2), mar = c(6, 6, 6, 6))

# Plot the values of acceleration againts mpg
plot(mpg ~ acceleration, main = "Polynomial regression\nmpg ~ acceleration")

r.squared <- c()
adj.r.squared <- c()

for (degree in 1:DEGREES_COUNT) {
  # Create polynomial regression models degrees 1 to 5
  model <- lm(mpg ~ poly(acceleration.sorted, degree))

  # Plot the polynomial regression lines degrees 1 to 5
  points(
    acceleration.sorted,
    predict(model),
    type = "l",
    lwd = 4,
    col = global.colors[degree]
  )

  # Retrieve the R-squared value
  r.squared <- c(r.squared, summary(model)$r.squared) 
  adj.r.squared <- c(adj.r.squared, summary(model)$adj.r.squared)
}

legend(
  8.3, 46,
  legend = paste(c(1:DEGREES_COUNT), "Deg."),
  col = global.colors,
  lty = 1,
  lwd = 4,
  cex = 1.2
)

# Plot the values of R-squared
plot(
  r.squared,
  ylim = c(0.3, 0.4),
  xlab = "Degree",
  ylab = "R-squared",
  main = "R-squared of polynomial regression models with various degrees",
  lwd = 4,
  type = "l",
  col = global.colors[11]
)

lines(
  adj.r.squared,
  lwd = 4,
  col = global.colors[12]
)

legend(
  1, 0.4,
  legend = c("R-squared", "Adjusted R-squared"),
  col = global.colors[11:12],
  lty = 1,
  lwd = 4,
  cex = 1.2
)

if (params.fileOutput) {
  dev.off()
}



#########################################
##              PART 02                ##
#########################################

#
# (1): Mpg01
#

# Create binare feature mpg01 defined as:
#   1 <=> mpg >= median(mpg)
#   0 <=> mpg <  median(mpg)
data$mpg01 <- as.integer(data$mpg > median(data$mpg))

# Create data set d
d <- data[, c(2:10)]

# Calculate the entropy of mpg01
# The entropy should be equal to 1
# This is because there are two possible values (1, 0), with 50/50 probability
entropy(table(data$mpg01))

#
# (2): Splitting data
#

TEST_SUBSET_SIZE <- 0.2

# Randomly divide indexes 1:nrow(d) into test and train subsets
indexes.all <- c(1:nrow(d))
indexes.test <- sample(c(1:nrow(d)), nrow(d) * TEST_SUBSET_SIZE, F)
indexes.train <- indexes.all[is.na(pmatch(indexes.all, indexes.test))]

# Use indexes to get test subset and train subset
test <- d[indexes.test, ]
train <- d[indexes.train, ]

#
# (3): Trivial classifier
#

# Print the frequency table of mpg01 of the training set  
table(train$mpg01)

# Always predict the most frequent value
trivial.predict <- as.integer(ifelse(table(train$mpg01)[1] > table(train$mpg01)[2], 0, 1))
message(paste("\nTrivial model: always predict", trivial.predict))

# Evaluate trivial classifier and print the results
(trivial.evaluation <- binary.evaluation(rep(trivial.predict, nrow(test)), test$mpg01))

#
# (4): Logistic reggression
#

# Create the logistic regression model
log <- glm(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = train, family = binomial(link = "logit"))

# Calculate probabilities on both sets
log.probs.train <- predict(log, train, type = "response")
log.probs.test <- predict(log, test, type = "response")

# Predict one if Pr(x, theta) > 0.5
log.05.train.predict <- rep(0, length(log.probs.train))
log.05.train.predict[log.probs.train > 0.5] <- 1

log.05.test.predict <- rep(0, length(log.probs.test))
log.05.test.predict[log.probs.test > 0.5] <- 1

# Evaluate the results on both sets
message("\n\nlog.05 TRAIN data evaluation\n")
(log.05.train.evaluation <- binary.evaluation(log.05.train.predict, train$mpg01))

message("\n\nlog.05 TEST data evaluation\n")
(log.05.test.evaluation <- binary.evaluation(log.05.test.predict, test$mpg01))

#
# (5): More logistic regression
#

# Predict one if Pr(x, theta) > 0.1 and evaluate
log.01.predict <- rep(0, length(log.probs.test))
log.01.predict[log.probs.test > 0.1] <- 1
(log.01.evaluation <- binary.evaluation(log.01.predict, test$mpg01))

# Predict one if Pr(x, theta) > 0.9 and evaluate
log.09.predict <- rep(0, length(log.probs.test))
log.09.predict[log.probs.test > 0.9] <- 1
(log.09.evaluation <- binary.evaluation(log.09.predict, test$mpg01))

#
# (5.1): Even more logistic regression
#

# Number of points on x axis
LOG_PERF_POINTS_COUNT <- 200

log.perf.x <- c(1:LOG_PERF_POINTS_COUNT) / LOG_PERF_POINTS_COUNT

# Performance measures
log.perf.precision <- c()
log.perf.recall <- c()
log.perf.specificity <- c()
log.perf.F.score <- c()
log.perf.accuracy <- c()

# Predict and evaluate performance for all points on the x axis
for (i in log.perf.x) {
  predict <- rep(0, length(log.probs.train))
  predict[log.probs.train > i] <- 1

  evaluation <- binary.evaluation(predict, train$mpg01)

  log.perf.precision <- c(log.perf.precision, evaluation$precision)
  log.perf.recall <- c(log.perf.recall, evaluation$recall)
  log.perf.specificity <- c(log.perf.specificity, evaluation$specificity)
  log.perf.F.score <- c(log.perf.F.score, evaluation$F.score)
  log.perf.accuracy <- c(log.perf.accuracy, evaluation$accuracy)
}

if (params.fileOutput) {
  pdf("out/logistic-regression-performance.pdf", width = 20, height = 10)
}

par(mar = c(6, 6, 6, 12))

# Plot everything
plot(
  x = log.perf.x,
  y = log.perf.precision,
  main = "Performance measures of different logistic regression tresholds using `train` set",
  xlab = "Treshold",
  ylab = "Performance",
  lwd = 4,
  type = "l",
  col = global.colors[1]
)

lines(x = log.perf.x, y = log.perf.recall, lwd = 4, col = global.colors[2])
lines(x = log.perf.x, y = log.perf.specificity, lwd = 4, col = global.colors[3])
lines(x = log.perf.x, y = log.perf.F.score, lwd = 4, col = global.colors[4])
lines(x = log.perf.x, y = log.perf.accuracy, lwd = 4, col = global.colors[5])

par(xpd = TRUE)

legend(
  1.06, 1.0126,
  legend = c("Precision", "Recall", "Specificity", "F-score", "Accuracy"),
  col = global.colors,
  lty = 1,
  lwd = 4,
  cex = 1.2
)

if (params.fileOutput) {
  dev.off()
}

#
# (6): Decision trees
#

CP <- 0.0001

# Generate inital decision tree to get cptable and print it
tree <- rpart(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = train, method = "class", cp = CP); tree$cptable

if (params.fileOutput) {
  pdf("out/decision-trees.pdf", width = 20, height = 20)
}

tree.accuracy.train <- c()
tree.accuracy.test <- c()

par(mar = c(6, 6, 6, 6), mfrow = c(2, 2))

for (cp in tree$cptable[, 1]) {
  # Generate a decision tree model for each of possible CP values
  tree <- rpart(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = train, method = "class", cp = cp);

  # Predict values for train and test sets
  tree.predict.train <- ifelse(predict(tree, train)[, 1] > 0.5, 0, 1) 
  tree.predict.test <- ifelse(predict(tree, test)[, 1] > 0.5, 0, 1) 

  # Evaluate the model
  tree.evaluation.train <- binary.evaluation(tree.predict.train, train$mpg01)
  tree.evaluation.test <- binary.evaluation(tree.predict.test, test$mpg01)

  # Save the accuracy value
  tree.accuracy.train <- c(tree.accuracy.train, tree.evaluation.train$accuracy)
  tree.accuracy.test <- c(tree.accuracy.test, tree.evaluation.test$accuracy)

  # Plot the decision tree model
  rpart.plot(tree, main = paste(
    "Decision tree for mpg01 feature prediction\ncp =",
    round(cp, digits = 4),
    "\ntest error =",
    round(tree.evaluation.test$error, digits = 4)
  ));
}

# Create a analysis data frame and print it
tree.cp.analysis <- data.frame(tree$cptable[, 1:5], tree.accuracy.train, tree.accuracy.test)
tree.cp.analysis$tree.error.train <- 1 - tree.cp.analysis$tree.accuracy.train
tree.cp.analysis$tree.error.test <- 1 - tree.cp.analysis$tree.accuracy.test
as.data.frame(t(tree.cp.analysis))

if (params.fileOutput) {
  dev.off()
}

if (params.fileOutput) {
  pdf("out/decision-tree-best.pdf", width = 10, height = 10)
}

par(mfrow = c(1, 1), mar = c(6, 6, 6, 6))

# Plot the best decision tree (cp = 0.01923077)
rpart.plot(rpart(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = train, method = "class", cp = tree.cp.analysis[2, 1]), main = paste("The best decision tree for mpg01 feature prediction\ncp =", round(tree.cp.analysis[2, 1], digits = 4), "\ntest error =", round(tree.cp.analysis[2, 9], digits = 4)))

if (params.fileOutput) {
  dev.off()
}