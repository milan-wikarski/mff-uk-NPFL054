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
# │  ├─ 6.1: Decision Tree
# │  ├─ 6.2: Random Forest
# │  ├─ 6.3: Regularized Logistic Regression
# │  ├─ 6.4: Models Testing and Comparison
# │  └─ 6.5: Best Model
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
params.workDir <- '~/School/NPFL054/hw/hw3'
params.outDir <- 'out/'
params.checkPackages <- FALSE

params.fileOutput <- TRUE

params.seed <- 4356

params.testSize <- 1000
params.folds <- 10

#
#
#
#
#
#

########################################################
##                  2: INITIALIZATION                 ##
########################################################

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
}

library(ISLR)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(pROC)
library(ROCR)
 
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

cv.split.safe <- function(data, prop, value, folds=10) {
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

#
#
#
#
#
#

########################################################
##                   4: DATA DIVISION                 ##
########################################################

# Load data
# data <- as.data.frame(lapply(Caravan, factor))
data <- Caravan
attach(data)

# Randomly select indexes
indexes.all <- c(1: nrow(data))
indexes.test <- sample(indexes.all, params.testSize)
indexes.train <- indexes.all[is.na(pmatch(indexes.all, indexes.test))]

if (length(intersect(indexes.test, indexes.train)) != 0) {
  stop('Intersect of indexes.test and indexes.train is not empty')
}

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
# 5.1: Target attribute distribution
#

purchase.freq <- table(Purchase) / nrow(data)

# Barplot of target feature distribution
if (params.fileOutput) {
  pdf(paste(params.outDir, "target-attribute-frequency.pdf", sep=""), width=10, height=10)
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
# 5.2: MOSHOOFD
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
    file=paste(params.outDir, "customer-main-type.csv", sep="")
  )
}

# Plot MOSHOOFD
if (params.fileOutput) {
  pdf(paste(params.outDir, "customer-main-type.pdf", sep=""), width=20, height=6)
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
  pdf(paste(params.outDir, "customer-main-type-boxplot", sep=""), width=20, height=10)
}

par(mar=c(3, 3, 3, 3), mfrow=c(1, 2), cex=1.8)

boxplot(
  as.vector(table(MOSHOOFD)),
  main="Group sizes boxplot"
)

boxplot(
  MOSHOOFD.purchase[, 2],
  main="Purchase frequency per group boxplot"
)

if (params.fileOutput) {
  dev.off()
}

#
# 5.3: MOSTYPE
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
    file=paste(params.outDir, "customer-sub-type.csv", sep="")
  )
}

# Plot MOSTYPE distribution
if (params.fileOutput) {
  pdf(paste(params.outDir, "customer-sub-type-distribution.pdf", sep=""), width=20, height=6)
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
  pdf(paste(params.outDir, "customer-sub-type-frequency.pdf", sep=""), width=20, height=6)
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
  pdf(paste(params.outDir, "customer-sub-type-boxplot", sep=""), width=20, height=10)
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
# 5.4: MOSTYPE, MOSHOOFD relationship
#

# Distribution of subgroups
MM <- table(MOSTYPE, MOSHOOFD)

if (params.fileOutput) {
  write.csv(MM, paste(params.outDir, "customer-types.csv", sep=""))
}

# Plot distribution of subgroups in every main type group
if (params.fileOutput) {
  pdf(paste(params.outDir, "customer-subgroups-distribution.pdf", sep=""), width=25, height=10)
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
  pdf(paste(params.outDir, "customer-subgroups-purchase.pdf", sep=""), width=25, height=10)
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
    pdf(paste(params.outDir, "group-", i, "-detail.pdf", sep=""), width=10, height=10)
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
# 6.1: Decision Tree
#



tree.eval <- list()
tree.cp <- 1:50 / 100

for (i in 1:length(tree.cp)) {
  cp <- tree.cp[i]

  if (params.fileOutput) {
    if (!dir.exists(paste(params.outDir, "decision-tree", sep=""))) {
      dir.create(paste(params.outDir, "decision-tree", sep=""))
    }

    pdf(paste(params.outDir, "decision-tree/cp-", cp, ".pdf", sep=""), width=25, height=10)
  }

  par(mfrow=c(2, 5), cex=1.8, mar=c(0, 0, 0, 0))

  tree.eval.auc <- c()

  for (k in 1:params.folds) {
    data.cv.train <- data.train[-data.cv[[k]], ]
    data.cv.test <- data.train[data.cv[[k]], ]

    tree <- rpart(
      formula=Purchase ~ .,
      data=data.cv.train,
      cp=cp,
      model=TRUE,
    )

    tree.roc <- roc(
      data.cv.test$Purchase,
      predict(tree, data.cv.test)[, 2],
      partial.auc=c(1, 0.8),
      levels=c("Yes", "No"),
      direction=">",
      plot=TRUE,
      lagacy.axes=TRUE,
      xlab="FPR",
      ylab="TPR",
      polygon=TRUE
    )

    tree.eval.auc <- c(tree.eval.auc, as.numeric(tree.roc$auc))
  }

  if (params.fileOutput) {
    dev.off()
  }

  message(paste("CP value set to", cp))
  print(tree.eval.auc)

  tree.eval[[i]] <- tree.eval.auc
}


#
# 6.2 Random Forest
#

