# Table of contents
# │
# ├─ 1: Parameters
# │
# ├─ 2: Initialization
# │
# ├─ 3: Helper functions
# │
# ├─ 4: Data Divison
# │
# ├─ 5: Data analysis (Task 1)
# │  ├─ 5.1: Traget feature distribution
# │  ├─ 5.2: MOSHOOFD (1a)
# │  ├─ 5.3: MOSTYPE (1a)
# │  └─ 5.4: MOSTYPE, MOSHOOFD relationship (1b)
# │
# ├─ 6: Model fitting, optimization and selection (Task 2)
# │  ├─ 6.1: Decision Tree (2a)
# │  ├─ 6.2: Random Forest (2b)
# │  ├─ 6.3: Regularized Logistic Regression (2c)
# │  ├─ 6.4: Models Testing and Comparison (2d)
# │  │  ├─ 6.4.1: Decision Tree
# │  │  ├─ 6.4.2: Random Forest
# │  │  └─ 6.4.3: Regularized Logistic Regression
# │  │
# │  └─ 6.5: Best Model (2e)
# │
# ├─ 7: Model interpretation and feature selection (Task 3)
# │  ├─ 7.1: Tree vs Forest
# │  └─ 7.2: Lasso
# │
# └─ 8: Final prediction on the blind set (Task 4)
#
#
#
# used: │, ├, ─, └

#
#
#
#
#
#

########################################################
##                    1: PARAMETERS                   ##
########################################################

# ANCHOR 

params.workDir <- '~/School/NPFL054/hw/hw3'
params.outDir <- 'out/'
params.checkPackages <- FALSE

params.fileOutput <- TRUE
params.readDataFromFiles <- TRUE

params.seed <- 4356

params.testSize <- 1000
params.predictPositive <- 100
params.folds <- 10
params.alpha <- 0.05

#
#
#
#
#
#

########################################################
##                  2: INITIALIZATION                 ##
########################################################

# ANCHOR 

# Set working directry and load data
setwd(params.workDir)

# Set seed
set.seed(params.seed)

# Load packages
if (params.checkPackages) {
  packages <- installed.packages()[, 1]

  if (!("ISLR" %in% packages)) {
    install.packages("ISLR")
  }
  if (!("rpart" %in% packages)) {
    install.packages("rpart")
  }
  if (!("rpart.plot" %in% packages)) {
    install.packages("rpart.plot")
  }
  if (!("RColorBrewer" %in% packages)) {
    install.packages("RColorBrewer")
  }
  if (!("randomForest" %in% packages)) {
    install.packages("randomForest")
  }
  if (!("pROC" %in% packages)) {
    install.packages("pROC")
  }
  if (!("ROCR" %in% packages)) {
    install.packages("ROCR")
  }
  if (!("glmnet" %in% packages)) {
    install.packages("glmnet")
  }
}

library(ISLR)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(pROC)
library(ROCR)
library(glmnet)
 
# Global chart parameters
par(xpd=TRUE)

# Create /out directory
if (params.fileOutput && !dir.exists(params.outDir)) {
  dir.create(params.outDir)
}

# Choose colors
colors <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert=T)], 30)


#
#
#
#
#
#

########################################################
##                 3: HELPER FUNCTIONS                ##
########################################################

# ANCHOR 

cv.split.safe <- function(data, prop, value, folds) {
  data.positive.indexes <- which(data[, prop] == value)
  data.negative.indexes <- which(data[, prop] != value)

  data.positive <- data.frame(index=sample(data.positive.indexes))
  data.positive$fold <- cut(seq(1, nrow(data.positive)), breaks=folds, labels=FALSE)

  data.negative <- data.frame(index=sample(data.negative.indexes))
  data.negative$fold <- cut(seq(1, nrow(data.negative)), breaks=folds, labels=FALSE)

  res <- list()

  for (i in 1:folds) {
    res[[i]] <- c(
      data.positive[which(data.positive$fold == i), 1],
      data.negative[which(data.negative$fold == i), 1]
    )
  }

  return (res)
}

error.bars <- function(x, y0, y1, step) {
  segments(x0=x, y0=y0, y1=y1)
  segments(x0=x - step, x1=x + step, y0=y0)
  segments(x0=x - step, x1=x + step, y0=y1)
}

predict.100 <- function(probs) {
  model.pred <- rep(0, 1000)
  model.pred[order(probs)[901:1000]] <- 1

  return (model.pred)
}

predict.100.eval <- function(probs, target) {
  res = list()
  
  res$confusion.matrix <- table(as.numeric(target) - 1, predict.100(probs))
  res$precision <- res$confusion.matrix[4] / 100

  # model.pred.prob <- predict(model, data.test, type="prob")[, 2]
  # model.pred <- rep(0, 1000)
  # table(data.test[rownames(subset(data.frame(model.pred.prob, model.pred), model.pred == 1)), 86]) # Alternative way  

  return (res)
}

auc.quick <- function(
  truth,
  prediction,
  fpr.stop=0.2,
  levels=c("No", "Yes"),
  direction="<"
) {
  r <- roc(
    truth,
    prediction,
    partial.auc=c(1, 1-fpr.stop),
    levels=levels,
    direction=direction
  )

  return (as.numeric(r$auc))
}

auc.summary <- function(auc) {
  res <- c(mean(auc), sd(auc))

  if (length(unique(auc)) == 1) {
    res <- c(res, mean(auc), mean(auc))
  } else {
    T <- t.test(auc)
    res <- c(res, T$conf.int[1], T$conf.int[2])
  }

  return (res)
}

#
#
#
#
#
#

########################################################
##                   4: DATA DIVISION                 ##
########################################################

# ANCHOR 

# Load data
data <- cbind(as.data.frame(lapply(Caravan[, 1:64], factor)), Caravan[, 65:86])
attach(data)

# Randomly select indexes
indexes.all <- c(1: nrow(data))
indexes.test <- sort(sample(indexes.all, params.testSize))
indexes.train <- indexes.all[is.na(pmatch(indexes.all, indexes.test))]

# Split data into train and test subsets
data.train <- data[indexes.train, ]
data.test <- data[indexes.test, ]

#
#
#
#
#
#

########################################################
##                  5: DATA ANALYSIS                  ##
########################################################

#
# 5.1: Target attribute distribution | ANCHOR 
#

purchase.freq <- table(Purchase) / nrow(data)

# Barplot of target feature distribution
if (params.fileOutput) {
  pdf(paste(params.outDir, "data-analysis/target-attribute-frequency.pdf", sep=""), width=10, height=10)
}

par(mar=c(3, 3, 3, 3), cex=1.8)

purchase.barplot <- barplot(
  table(Purchase),
  ylim=c(0, length(Purchase) + 500),
  main="Target feature distribution"
)

text(
  x=purchase.barplot,
  y=table(Purchase) + 200,
  label=paste(round(purchase.freq, 4) * 100, "%")
)

if (params.fileOutput) {
  dev.off()
}

# Print the mean of precision sampling distribution
message("The mean of precision sampling distribution is:")
as.numeric(purchase.freq[2])

#
# 5.2: MOSHOOFD | ANCHOR 
#

MOSHOOFD.labels <- c(
  "Successfull hedonists (1)",
  "Driven growers (2)",
  "Average family (3)",
  "Career loners (4)",
  "Living well (5)",
  "Cruising seniors (6)",
  "Retired and religious (7)",
  "Family with grown ups (8)",
  "Convervative families (9)",
  "Farmers (10)"
)

# Calculate Purchase frequency per group
MOSHOOFD.purchase <- (table(MOSHOOFD, Purchase) / as.vector(table(MOSHOOFD)))

# Save table to CSV
if (params.fileOutput) {
  write.csv(
    as.data.frame(cbind(MOSHOOFD.labels, table(MOSHOOFD), paste(round(MOSHOOFD.purchase[, 2] * 100, 2), "%", sep=""))),
    file=paste(params.outDir, "data-analysis/customer-main-type.csv", sep="")
  )
}

# Plot MOSHOOFD
if (params.fileOutput) {
  pdf(paste(params.outDir, "data-analysis/customer-main-type.pdf", sep=""), width=20, height=6)
}

par(mar=c(3, 3, 3, 3), mfrow=c(1, 2), cex=1.8)

# Distribution
barplot(
  table(MOSHOOFD),
  main="Customer main type distribution"
)

# Purchase frequency
barplot(
  MOSHOOFD.purchase[, 2],
  main="Customer main type ~ Purchase"
)

if (params.fileOutput) {
  dev.off()
}

# Plot MOSHOOFD purchase frequency boxplot
if (params.fileOutput) {
  pdf(paste(params.outDir, "data-analysis/customer-main-type-boxplot", sep=""), width=20, height=10)
}

par(mar=c(3, 3, 3, 3), mfrow=c(1, 2), cex=1.8)

boxplot(
  as.vector(table(MOSHOOFD)),
  main="Group sizes boxplot"
)

boxplot(
  MOSHOOFD.purchase[, 2],
  main="Purchase frequency per subgroup boxplot"
)

if (params.fileOutput) {
  dev.off()
}

#
# 5.3: MOSTYPE | ANCHOR 
#

MOSTYPE.labels <- c(
  "High Income (1)",
  "Very Important Provincials (2)",
  "High status seniors (3)",
  "Affluent senior apartments (4)",
  "Mixed seniors (5)",
  "Career and childcare (6)",
  "Dinki's (double income no kids) (7)",
  "Middle class families (8)",
  "Modern (9)",
  "Stable family (10)",
  "Family starters (11)",
  "Affluent young families (12)",
  "Young all american family (13)",
  "Junior cosmopolitan (14)",
  "Senior cosmopolitans (15)",
  "Students in apartments (16)",
  "Fresh masters in the city (17)",
  "Single youth (18)",
  "Suburban youth (19)",
  "Etnically diverse (20)",
  "Young urban have-nots (21)",
  "Mixed apartment dwellers (22)",
  "Young and rising (23)",
  "Young (24)",
  "Young seniors in the city (25)",
  "Own home elderly (26)",
  "Seniors in apartments (27)",
  "Residential elderly (28)",
  "Porchless seniors: no front yard (29)",
  "Religious elderly singles (30)",
  "Low income catholics (31)",
  "Mixed seniors (32)",
  "Lower class large families (33)",
  "Large family (34)",
  "Village families (35)",
  "Couples with teens 'Married with children' (36)",
  "Mixed small town dwellers (37)",
  "Traditional families (38)",
  "Large religous families (39)",
  "Large family farms (40)",
  "Mixed rurals (41)"
)

# Calculate Purchase frequency per group
MOSTYPE.purchase <- (table(MOSTYPE, Purchase) / as.vector(table(MOSTYPE)))

# Save table to CSV
if (params.fileOutput) {
  write.csv(
    as.data.frame(cbind(MOSTYPE.labels[sort(unique(MOSTYPE))], table(MOSTYPE), paste(round(MOSTYPE.purchase[, 2] * 100, 2), "%", sep=""))),
    file=paste(params.outDir, "data-analysis/customer-sub-type.csv", sep="")
  )
}

# Plot MOSTYPE distribution
if (params.fileOutput) {
  pdf(paste(params.outDir, "data-analysis/customer-sub-type-distribution.pdf", sep=""), width=20, height=6)
}

par(mar=c(3, 3, 3, 3), cex=1.8)

barplot(
  table(MOSTYPE),
  main="Customer subtype distribution"
)

if (params.fileOutput) {
  dev.off()
}

# Plot MOSTYPE purchase frequency
if (params.fileOutput) {
  pdf(paste(params.outDir, "data-analysis/customer-sub-type-frequency.pdf", sep=""), width=20, height=6)
}

par(mar=c(3, 3, 3, 3), cex=1.8)


barplot(
  MOSTYPE.purchase[, 2],
  main="Customer subtype ~ Purchase"
)

if (params.fileOutput) {
  dev.off()
}

# Plot MOSTYPE purchase frequency boxplot
if (params.fileOutput) {
  pdf(paste(params.outDir, "data-analysis/customer-sub-type-boxplot", sep=""), width=20, height=10)
}

par(mar=c(3, 3, 3, 3), mfrow=c(1, 2), cex=1.8)

boxplot(
  as.vector(table(MOSTYPE)),
  main="Subgroup sizes boxplot"
)

boxplot(
  MOSTYPE.purchase[, 2],
  main="Purchase frequency per group boxplot"
)

if (params.fileOutput) {
  dev.off()
}

#
# 5.4: MOSTYPE, MOSHOOFD relationship | ANCHOR 
#

# Distribution of subgroups
MM <- table(MOSTYPE, MOSHOOFD)

if (params.fileOutput) {
  write.csv(MM, paste(params.outDir, "data-analysis/customer-types.csv", sep=""))
}

# Plot distribution of subgroups in every main type group
if (params.fileOutput) {
  pdf(paste(params.outDir, "data-analysis/customer-subgroups-distribution.pdf", sep=""), width=25, height=10)
}

par(mfrow=c(2, 5), mar=c(3, 3, 3, 3), oma=c(0, 0, 6, 0), cex=1.8)

for (i in 1:length(table(MOSHOOFD))) {
  barplot(
    MM[which(MM[, i] != 0), i],
    main=paste(MOSHOOFD.labels[i])
  )
}

mtext("Distribution of subgroups in each of customer main type groups", side=1, line=-25, cex=2.6, outer=TRUE, font=2)

if (params.fileOutput) {
  dev.off()
}

# Purchase frequency in subgroups
MM.purchase <- table(MOSTYPE, Purchase)[, 2] / table(MOSTYPE, MOSHOOFD)
MM.purchase[which(!is.finite(MM.purchase))] <- 0

# Plot purchase frequency of subgroups in every main type group
if (params.fileOutput) {
  pdf(paste(params.outDir, "data-analysis/customer-subgroups-purchase.pdf", sep=""), width=25, height=10)
}

par(mfrow=c(2, 5), mar=c(3, 3, 3, 3), oma=c(0, 0, 6, 0), cex=1.8)

for (i in 1:length(table(MOSHOOFD))) {
  barplot(
    MM.purchase[which(MM[, i] != 0), i],
    main=paste(MOSHOOFD.labels[i])
  )
}

mtext("Purchase frequency of subgroups in each of customer main type groups", side=1, line=-25, cex=2.6, outer=TRUE, font=2)

if (params.fileOutput) {
  dev.off()
}

# Plot subgroup sizes, Purchase frequency barchart and boxplot combination for every group
for (i in 1:length(table(MOSHOOFD))) {
  if (params.fileOutput) {
    pdf(paste(params.outDir, "data-analysis/group-", i, "-detail.pdf", sep=""), width=10, height=10)
  }

  par(mfrow=c(2, 2), mar=c(1, 3, 3, 3), oma=c(0, 0, 3, 0), cex=1.8)

  # Group size barplot
  barplot(
    MM[which(MM[, i] != 0), i],
    main="Subgroups sizes"
  )

  # Purchase frequency barplot
  barplot(
    MM.purchase[which(MM[, i] != 0), i],
    main="Purchase frequency"
  )

  par(mar=c(3, 3, 3, 3))

  # Group size boxplot
  boxplot(
    as.vector(MM[which(MM[, i] != 0), i])
  )

  # Purchase frequency boxplot
  boxplot(
    as.vector(MM.purchase[which(MM[, i] != 0), i])
  )

  mtext(MOSHOOFD.labels[i], side=1, line=-26, cex=2.6, outer=TRUE, font=2)

  if (params.fileOutput) {
    dev.off()
  }

}

#
#
#
#
#
#

########################################################
##    6: MODEL FITTING, OPTIMIZATION AND SELECTION    ##
########################################################

data.cv <- cv.split.safe(data.train, prop="Purchase", value="Yes", folds=params.folds)

#
# 6.1: Decision Tree | ANCHOR
#

if (params.readDataFromFiles) {

  tree.eval <- as.data.frame(read.table(paste(params.outDir, "decision-tree/decision-tree-eval.csv", sep=""), header=TRUE, sep=",")[, -1])

  tree.eval.auc.total <- as.data.frame(read.table(paste(params.outDir, "decision-tree/decision-tree-eval-auc.csv", sep=""), header=TRUE, sep=",")[, -1])

} else {

  # Setup eval data frame
  tree.eval <- data.frame(
    cp=seq(from=0.0001, to=0.01, by=0.0002),
    AUC.mean=NaN,
    AUC.std=NaN,
    AUC.CI.low=NaN,
    AUC.CI.high=NaN
  )

  tree.eval.auc.total <- c()

  for (i in 1:nrow(tree.eval)) {
    cp <- tree.eval[i, "cp"]

    tree.eval.auc <- c()

    for (k in 1:params.folds) {
      # Take train and test data for this iteration of CV
      data.cv.train <- data.train[-data.cv[[k]], ]
      data.cv.test <- data.train[data.cv[[k]], ]

      # Build a decision tree
      tree <- rpart(
        formula=Purchase ~ .,
        data=data.cv.train,
        cp=cp,
        model=TRUE
      )

      # Compute AUC
      tree.auc <- auc.quick(data.cv.test$Purchase, predict(tree, data.cv.test)[, 2])

      # Save performance measurements
      tree.eval.auc <- c(tree.eval.auc, tree.auc)
      tree.eval.auc.total <- c(tree.eval.auc.total, tree.auc)
    }

    message(paste("CP value set to", cp))
    print(tree.eval.auc)

    # Calculate mean, std and CI
    tree.eval[i, 2:5] <- auc.summary(tree.eval.auc)

    print(tree.eval[i, ])
  }

  # Convert all AUC_0.2 measurements into a data frame
  tree.eval.auc.total <- cbind(as.data.frame(matrix(tree.eval.auc.total, nrow=nrow(tree.eval), byrow=TRUE)), cp=tree.eval$cp)

  # Save parameter tuning data to .csv files
  if (params.fileOutput) {
    write.csv(tree.eval, file=paste(params.outDir, "decision-tree/decision-tree-eval.csv", sep=""))

    write.csv(tree.eval.auc.total, file=paste(params.outDir, "decision-tree/decision-tree-eval-auc.csv", sep=""))
  }
}

# Plot AUC_0.2 mean and confidence intervals
if (params.fileOutput) {
  pdf(paste(params.outDir, "decision-tree/decision-tree-eval.pdf", sep=""), width=10, height=10)
}

par(cex=1.3, mar=c(5, 5, 5, 5))

plot(
  x=tree.eval$cp,
  y=tree.eval$AUC.mean,
  ylim=c(0, 0.1),
  type="o",
  main="Performance of decision tree model for different values of cp",
  xlab="CP",
  ylab="Mean AUC_0.2",
)

error.bars(
  x=tree.eval$cp,
  y0=tree.eval$AUC.CI.low,
  y1=tree.eval$AUC.CI.high,
  step=0.00005
)

if (params.fileOutput) {
  dev.off()
}

#
# 6.2 Random Forest | ANCHOR
#

# This block of code is used to tune multiple parameters using different ranges of values. The code has to be changed manually before tuning each parameter give a particular value range.

# Parameter tuning takes a long time. Comment the whole section out to skip parameter tuning. Best parameters are declared explicitly in the 6.4 section. Please check the report for their explanation

if (params.readDataFromFiles) {

  forest.eval <- as.data.frame(read.table(paste(params.outDir, "random-forest/random-forest-eval.csv", sep=""), header=TRUE, sep=",")[, -1])

  forest.eval.auc.total <- as.data.frame(read.table(paste(params.outDir, "random-forest/random-forest-eval-auc.csv", sep=""), header=TRUE, sep=",")[, -1])

} else {

  # Setup eval data frame
  forest.eval <- data.frame(
    cp=round(seq(1, 30) ** 1.25),
    AUC.mean=NaN,
    AUC.std=NaN,
    AUC.CI.low=NaN,
    AUC.CI.high=NaN
  )

  forest.eval.auc.total <- c()

  for (i in 1:nrow(forest.eval)) {
    ntree <- forest.eval[i, "ntree"]

    forest.eval.auc <- c()

    for (k in 1:params.folds) {
      # Take train and test data for this iteration of CV
      data.cv.train <- data.train[-data.cv[[k]], ]
      data.cv.test <- data.train[data.cv[[k]], ]

      # Build a random forest
      forest <- randomForest(
        formula=Purchase ~ .,
        data=data.cv.train,
        ntree=ntree
      )

      # Compute AUC
      forest.auc <- auc.quick(data.cv.test$Purchase, predict(forest, data.cv.test, type="prob")[, 2])

      # Save performance measurements
      forest.eval.auc <- c(forest.eval.auc, forest.auc)
      forest.eval.auc.total <- c(forest.eval.auc.total, forest.auc)
    }

    message(paste("ntree value set to", ntree))
    print(forest.eval.auc)

    # Calculate mean, std and CI
    forest.eval[i, 2:5] <- auc.summary(forest.eval.auc)

    print(forest.eval[i, ])
  }

  # Convert all AUC_0.2 measurements into a data frame
  forest.eval.auc.total <- cbind(as.data.frame(matrix(forest.eval.auc.total, nrow=nrow(forest.eval), byrow=TRUE)), ntree=forest.eval$ntree)

  # Save parameter tuning data to .csv files
  if (params.fileOutput) {
    write.csv(forest.eval, file=paste(params.outDir, "random-forest/random-forest-eval.csv", sep=""))

    write.csv(forest.eval.auc.total, file=paste(params.outDir, "random-forest/random-forest-eval-auc.csv", sep=""))
  }
}

# Plot AUC_0.2 mean and confidence intervals
if (params.fileOutput) {
  pdf(paste(params.outDir, "random-forest/random-forest-eval.pdf", sep=""), width=10, height=10)
}

par(cex=1.3, mar=c(5, 5, 5, 5))

plot(
  x=forest.eval$ntree,
  y=forest.eval$AUC.mean,
  ylim=c(0, 0.1),
  type="o",
  main="Performance of random forest model for different values of ntree\nx = round(seq(1, 30) ** 1.25)",
  xlab="ntree",
  ylab="Mean AUC_0.2",
)

error.bars(
  x=forest.eval$ntree,
  y0=forest.eval$AUC.CI.low,
  y1=forest.eval$AUC.CI.high,
  step=0.5
)

if (params.fileOutput) {
  dev.off()
}

#
# 6.3 Regularized Logistic Regression | ANCHOR
#

if (params.readDataFromFiles) {

  glmnet.eval <- as.data.frame(read.table(paste(params.outDir, "glmnet/glmnet-eval.csv", sep=""), header=TRUE, sep=",")[, -1])

  glmnet.eval.auc.total <- as.data.frame(read.table(paste(params.outDir, "glmnet/glmnet-eval-auc.csv", sep=""), header=TRUE, sep=",")[, -1])
  
} else {

  # Setup eval data frame
  glmnet.eval <- data.frame(
    alpha=seq(0, 14) * (1 / 14),
    AUC.mean=NaN,
    AUC.std=NaN,
    AUC.CI.low=NaN,
    AUC.CI.high=NaN
  )

  glmnet.eval.auc.total <- c()

  for (i in 1:nrow(glmnet.eval)) {
    alpha <- glmnet.eval[i, "alpha"]

    glmnet.eval.auc <- c()

    for (k in 1:params.folds) {
      # Take train and test data for this iteration of CV
      data.cv.train <- data.train[-data.cv[[k]], ]
      data.cv.test <- data.train[data.cv[[k]], ]

      x <- model.matrix(Purchase~., data.cv.train)[,-1]
      y <- ifelse(data.cv.train$Purchase == "Yes", 1, 0)

      x.test <- model.matrix(Purchase ~., data.cv.test)[,-1]

      # Fit a regularized logistic regression
      model <- cv.glmnet(x, y, alpha=alpha, family="binomial")

      # Compute AUC
      glmnet.auc <- auc.quick(data.cv.test$Purchase, predict(model, newx = x.test, s="lambda.min", type="response"))

      # Save performance measurements
      glmnet.eval.auc <- c(glmnet.eval.auc, glmnet.auc)
      glmnet.eval.auc.total <- c(glmnet.eval.auc.total, glmnet.auc)
    }

    message(paste("alpha value set to", alpha))
    print(glmnet.eval.auc)

    # Calculate mean, std and CI
    glmnet.eval[i, 2:5] <- auc.summary(glmnet.eval.auc)

    print(glmnet.eval[i, ])
  }

  # Save parameter tuning data to .csv files
  if (params.fileOutput) {
    write.csv(glmnet.eval, file=paste(params.outDir, "glmnet/glmnet-eval.csv", sep=""))

    write.csv(glmnet.eval.auc.total, file=paste(params.outDir, "glmnet/glmnet-eval-auc.csv", sep=""))
  }
}

# Plot AUC_0.2 mean and confidence intervals
if (params.fileOutput) {
  pdf(paste(params.outDir, "glmnet/glmnet-eval.pdf", sep=""), width=10, height=10)
}

par(cex=1.3, mar=c(5, 5, 5, 5))

plot(
  x=glmnet.eval$alpha,
  y=glmnet.eval$AUC.mean,
  ylim=c(0.05, 0.1),
  type="o",
  main="Performance of Regularized Logistic Regression model for\ndifferent values of alpha\nx = seq(0, 14) * (1 / 14)",
  xlab="alpha",
  ylab="Mean AUC_0.2",
)

error.bars(
  x=glmnet.eval$alpha,
  y0=glmnet.eval$AUC.CI.low,
  y1=glmnet.eval$AUC.CI.high,
  step=0.01
)

if (params.fileOutput) {
  dev.off()
}

#
# 6.4 Models Testing and Comparison
#

#
# 6.4.1 Decision Tree | ANCHOR
#

cp.best <- 0.0025

tree <- rpart(
  formula=Purchase ~ .,
  data=data.train,
  cp=cp.best
)

tree.probs <- predict(tree, data.test, type="prob")[, 2]

tree.eval <- predict.100.eval(tree.probs, data.test$Purchase)

if (params.fileOutput) {
  pdf(paste(params.outDir, "decision-tree/decision-tree-best-roc.pdf", sep=""), width=10, height=10)
}

par(cex=1.3)

tree.roc <- roc(
  data.test$Purchase,
  as.vector(tree.probs),
  partial.auc=c(1, 0.8),
  legacy.axes=TRUE,
  plot=TRUE,
  levels=c("No", "Yes"),
  direction="<",
  print.auc=TRUE,
  auc.polygon=TRUE,
  mar=c(6, 6, 6, 6),
  main=paste("AUC_0.2 for Decision Tree\ncp = ", cp.best, "\nprecision = ", tree.eval$precision, sep="")
)

if (params.fileOutput) {
  dev.off()
}

message("Decision Tree")
print(tree.eval)
print(tree.roc$auc)

# Plot the tree
if (params.fileOutput) {
  pdf(paste(params.outDir, "decision-tree/decision-tree-best.pdf", sep=""), width=20, height=10)
}

rpart.plot(tree)

if (params.fileOutput) {
  dev.off()
}

# 
# 6.4.2 Random Forest | ANCHOR 
#

ntree.best <- 1000
mtry.best <- 15

forest <- randomForest(
  formula=Purchase ~ .,
  data=data.train,
  ntree=ntree.best,
  mtry=mtry.best
)

forest.probs <- predict(forest, data.test, type="prob")[, 2]

forest.eval <- predict.100.eval(forest.probs, data.test$Purchase)

if (params.fileOutput) {
  pdf(paste(params.outDir, "random-forest/random-forest-best-roc.pdf", sep=""), width=10, height=10)
}

par(cex=1.3)

forest.roc <- roc(
  data.test$Purchase,
  as.vector(forest.probs),
  partial.auc=c(1, 0.8),
  legacy.axes=TRUE,
  plot=TRUE,
  levels=c("No", "Yes"),
  direction="<",
  print.auc=TRUE,
  auc.polygon=TRUE,
  mar=c(6, 6, 6, 6),
  main=paste("AUC_0.2 for Random Forest\nntree = ", ntree.best, "\nmtry = ", mtry.best, "\nprecision = ", forest.eval$precision, sep="")
)

if (params.fileOutput) {
  dev.off()
}

message("Random Forest")
print(forest.eval)
print(forest.roc$auc)

#
# 6.4.3 Regularized Logistic Regression | ANCHOR 
#

alpha.best <- 0.0714

x <- model.matrix(Purchase~., data.train)[,-1]
y <- ifelse(data.train$Purchase == "Yes", 1, 0)

x.test <- model.matrix(Purchase ~., data.test)[,-1]

model <- cv.glmnet(x, y, alpha=alpha.best, family="binomial")

glmnet.probs <- predict(model, newx = x.test, s="lambda.min", type="response") 

glmnet.eval <- predict.100.eval(glmnet.probs, data.test$Purchase)

if (params.fileOutput) {
  pdf(paste(params.outDir, "glmnet/glmnet-best-roc.pdf", sep=""), width=10, height=10)
}

par(cex=1.3)

glmnet.roc <- roc(
  data.test$Purchase,
  as.vector(glmnet.probs),
  partial.auc=c(1, 0.8),
  legacy.axes=TRUE,
  plot=TRUE,
  levels=c("No", "Yes"),
  direction="<",
  print.auc=TRUE,
  auc.polygon=TRUE,
  mar=c(6, 6, 6, 6),
  main=paste("AUC_0.2 for Regularized Logistic Regression\nalpha = ", alpha.best, "\nprecision = ", glmnet.eval$precision, sep="")
)

if (params.fileOutput) {
  dev.off()
}

#
# 6.5 Best model | ANCHOR 
#

cp.best <- 0.0025

model.best <- rpart(
  Purchase ~ .,
  data=data,
  cp=cp.best
)

if (params.fileOutput) {
  pdf(paste(params.outDir, "best-model.pdf", sep=""), width=20, height=10)
}

rpart.plot(model.best)

if (params.fileOutput) {
  dev.off()
}

#
#
#
#
#
#

########################################################
##    7: MODEL INTERPRETATION AND FEATURE SELECTION   ##
########################################################

#
# 7.1 Tree vs Forest | ANCHOR
#

# Calculate forest importance
forest.importance <- data.frame(prop=rownames(forest$importance), importance=as.vector(forest$importance))
forest.importance <- forest.importance[rev(order(forest.importance[, 2])), ]

# Calculate tree importance
tree.importance <- data.frame(prop=rownames(data.frame(tree$variable.importance)), importance=as.vector(tree$variable.importance))
tree.importance <- tree.importance[rev(order(tree.importance[, 2])), ]

# Plot 15 most important feature
if (params.fileOutput) {
  pdf(paste(params.outDir, "features/15-most-important-features.pdf", sep=""), width=20, height=10)
}

par(mfrow=c(1, 2), cex=1.8, mar=c(8, 4, 4, 4), oma=c(0, 2, 5, 2))

barplot(
  forest.importance[1:15, 2],
  names=forest.importance[1:15, 1],
  las=2,
  main="Random Forest model",
  ylab="Importance"
)

barplot(
  tree.importance[1:15, 2],
  names=tree.importance[1:15, 1],
  las=2,
  main="Decision Tree model",
  ylab="Importance"
)

mtext("15 Most important features", side=1, line=-25, cex=2.8, outer=TRUE, font=2)

if (params.fileOutput) {
  dev.off()
}


# Plot the importance of all feature
if (params.fileOutput) {
  pdf(paste(params.outDir, "features/all-features.pdf", sep=""), width=20, height=10)
}

par(mfrow=c(1, 2), cex=1.8, mar=c(4, 4, 4, 4), oma=c(0, 2, 5, 2))

plot(
  rev(sort(forest$importance)),
  type="o",
  ylab="Importance",
  main=paste("Random Forest model\nm = ", nrow(forest.importance), sep="")
)

plot(
  rev(sort(tree$variable.importance)),
  type="o",
  ylab="Importance",
  main=paste("Decision Tree model\nm = ", nrow(tree.importance), sep="")
)

mtext("Feature importance", side=1, line=-25, cex=2.8, outer=TRUE, font=2)

if (params.fileOutput) {
  dev.off()
}

# Plot the importance of all features standardized
if (params.fileOutput) {
  pdf(paste(params.outDir, "features/all-features-standardized.pdf", sep=""), width=20, height=10)
}

par(mfrow=c(1, 2), cex=1.8, mar=c(4, 4, 4, 4), oma=c(0, 2, 5, 2))

plot(
  rev(sort(scale(forest$importance))),
  type="o",
  ylab="Importance",
  main=paste("Random Forest model\nm = ", nrow(forest.importance), sep="")
)

plot(
  rev(sort(scale(tree$variable.importance))),
  type="o",
  ylab="Importance",
  main=paste("Decision Tree model\nm = ", nrow(tree.importance), sep="")
)

mtext("Standardized feature importance", side=1, line=-25, cex=2.8, outer=TRUE, font=2)

if (params.fileOutput) {
  dev.off()
}

# Explore the most important variables
forest.importance.1 <- forest.importance[(which(scale(forest.importance[, 2]) > 1)), ]

tree.importance.1 <- tree.importance[(which(scale(tree.importance[, 2]) > 1)), ]

paste(forest.importance.1[, 1], collapse=", ")
paste(tree.importance.1[, 1], collapse=", ")

paste(intersect(tree.importance.1[, 1], forest.importance.1[, 1]), collapse=", ")

#
# 7.2 Lasso | ANCHOR 
#

data <- Caravan

# Lasso feature selection
add.names <- function(fit) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels = labs, pos = 4, offset = 0.7)
}

count.features <- function(col) {
  return (length(which(col != 0)))
}

x <- model.matrix(Purchase ~ ., data=data)
y <- data.matrix(data$Purchase)

grid <- 10^seq(-0.5, -5.5, length = 100)

fit.lasso <- glmnet(x, y,
	alpha = 1,
	family = "binomial",
	lambda = grid)

# Analyze the number of features
fit.lasso.features <- data.frame(
  lambda=grid,
  features=apply(fit.lasso$beta, 2, count.features)
)

if (params.fileOutput) {
  write.csv(fit.lasso.features, paste(params.outDir, "features/lasso.csv", sep=""))
}

# Features produced by the Lasso
paste(rownames(data.frame(fit.lasso$beta[, 33][which(fit.lasso$beta[, 33] != 0)])), collapse=", ")

# Plot coefficients
if (params.fileOutput) {
  pdf(paste(params.outDir, "features/lasso-coefficients.pdf", sep=""), width=20, height=10)
}

par(cex=1.8, mar=c(5, 5, 5, 5))

plot(
  fit.lasso,
  xvar = "lambda",
  main="Coefficients in Lasso model"
)

if (params.fileOutput) {
  dev.off()
}


# Plot the number of features
if (params.fileOutput) {
  pdf(paste(params.outDir, "features/lasso-features.pdf", sep=""), width=10, height=10)
}

par(cex=1.8, mar=c(4, 4, 4, 4))

plot(
  x=log(fit.lasso.features$lambda),
  y=fit.lasso.features$features,
  type="o",
  xlab="Log Lambda",
  ylab="Number of non-zero features",
  main="Feature count in lasso model"
)

if (params.fileOutput) {
  dev.off()
}

# Print a feature subste of size 16
print(paste(rownames(data.frame(fit.lasso$beta[, 33][which(fit.lasso$beta[, 33] != 0)])), collapse=", "))

#
#
#
#
#
#

########################################################
##    8: FINAL PREDICTION ON THE BLIND TEST SET   ##
########################################################

#
# ANCHOR 
#

data.blind <- read.table("src/caravan.test.1000.csv")
data.blind <- cbind(as.data.frame(lapply(data.blind[, 1:64], factor)), data.blind[, 65:85])
colnames(data.blind) <- colnames(data)[1:85]

# Ignore these examples:

# MGEMOMV
MGEMOMV <- which(data.blind[, "MGEMOMV"] == 6)
data.blind[MGEMOMV, "MGEMOMV"] <- 3

# MINK123M
MINK123M <- which(data.blind[, "MINK123M"] == 6)
data.blind[MINK123M, "MINK123M"] <- 0

# PWALAND
PWALAND <- which(data.blind[, "PWALAND"] == 1)
data.blind[PWALAND, "PWALAND"] <- 0

# PZEILPL
PZEILPL <- which(data.blind[, "PZEILPL"] == 2)
data.blind[PZEILPL, "PZEILPL"] <- 0

ignored <- c(MGEMOMV, MINK123M, PWALAND, PZEILPL)

# Calcualte probs and predict
probs <- predict(model.best, data.blind)[, 2]
pred <- predict.100(probs)

# Check if ignored are classified as 0
all(pred[ignored] == rep(0, 4))

# Check if 100 examples classified as 1
table(pred)

if (params.fileOutput) {
  write.csv(pred, paste(params.outDir, "final.csv", sep=""))
}