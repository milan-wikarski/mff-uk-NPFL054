# Table of contents
# │
# ├─ 1: Parameters
# │
# ├─ 2: Helper functions
# │  ├─ 2.1: inc() 
# │  ├─ 2.2: entropy() 
# │  └─ 2.3: binary.eval()
# │
# ├─ 3: Initialization
# │
# ├─ 4: Data Divison
# │
# ├─ 5: Data analysis
# │  ├─ 5.1: Traget feature distribution
#
# used: │, ├, ─, └

#
#
#
#
#
#

#########################################
##           1: PARAMETERS             ##
#########################################
params.workDir <- '~/School/NPFL054/hw/hw3'
params.outDir <- 'out/'
params.libPaths <- '~/R/libs'
params.checkPackages <- FALSE

params.fileOutput <- TRUE

params.seed <- 4356

params.testSize <- 1000

#
#
#
#
#
#

#########################################
##        2: HELPER FUNCTIONS          ##
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

binary.eval <- function(predictedValues, trueValues) {
  # Convert predicted and true values to vectors
  predictedValues <- as.vector(predictedValues)
  trueValues <- as.vector(trueValues)

  # Check the length
  if (length(predictedValues) != length(trueValues)) {
    stop('[predictedValues] and [trueValues] are of different length')
  }

  res <- list()

  # res$prediction <- predictedValues
  # res$truth <- trueValues

  res$confusion.matrix <- matrix(data = c(0, 0, 0, 0), nrow = 2, ncol = 2)

  dimnames(res$confusion.matrix) <- list(
    c('Truly positive', 'Truly negative'),
    c('Predicted positive', 'Predicted negative')
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

#
#
#
#
#
#

#########################################
##         3: Initialization           ##
#########################################

# Set working directry and load data
setwd(params.workDir)

# Set seed
set.seed(params.seed)

# Setup libs directory
.libPaths(params.libPaths)

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

  library(ISLR)
  library(rpart)
  library(rpart.plot)
  library(RColorBrewer)
  library(randomForest)
}
 
# Global chart parameters
par(cex = 1.5, xpd = TRUE)

# Create /out directory
if (params.fileOutput && !dir.exists(params.outDir)) {
  dir.create(params.outDir)
}

# Choose colors
colors <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], 30)

#
#
#
#
#
#

#########################################
##          4: Data Division           ##
#########################################

# Load data
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

#########################################
##          5: Data analysis           ##
#########################################

#
# 5.1: Target attribute distribution
#

purchase.freq <- table(Purchase) / nrow(data)

# Barplot of target feature distribution
if (params.fileOutput) {
  pdf(paste(params.outDir, "target-attribute-frequency.pdf"), width = 10, height = 10)
}

par(mar = c(5, 5, 5, 5))

purchase.barplot <- barplot(
  table(Purchase),
  ylim = c(0, length(Purchase) + 500),
  main = "Target feature distribution"
)

text(
  x = purchase.barplot,
  y = table(Purchase) + 200,
  label = paste(round(purchase.freq, 4) * 100, "%")
)

if (params.fileOutput) {
  dev.off()
}

# Print the mean of precision sampling distribution
message("The mean of precision sampling distribution is:")
as.numeric(purchase.freq[2])

#
# 5.2
#

# Label factors in MOSHOOFD
MOSHOOFD.labels <- c("Successful hedonists", "Driven growers", "Average family", "Career loners", "Living well", "Cruising seniors", "Retired and religious", "Family with grown ups", "Conservative families", "Farmers")

# Create MOSHOOFD distribution barplot
if (params.fileOutput) {
  pdf(paste(params.outDir, "customer-main-type-distribution.pdf"), width = 10, height = 10)
}

par(mar = c(10, 5, 5, 5))
barplot(table(MOSHOOFD), names = MOSHOOFD.labels, main = "Customer main type distribution", las = 2)

if (params.fileOutput) {
  dev.off()
} 

# Calculate the percentage of people who purchased a caravan policy in every group
MOSHOOFD.purchase <- as.data.frame(MOSHOOFD.labels)
MOSHOOFD.purchase$Count <- table(MOSHOOFD)
MOSHOOFD.purchase$No <- table(MOSHOOFD, Purchase)[, 1]
MOSHOOFD.purchase$No.Freq <- MOSHOOFD.purchase$No / MOSHOOFD.purchase$Count
MOSHOOFD.purchase$Yes <- table(MOSHOOFD, Purchase)[, 2]
MOSHOOFD.purchase$Yes.Freq <- MOSHOOFD.purchase$Yes / MOSHOOFD.purchase$Count

# Plot the distribution
if (params.fileOutput) {
  pdf(paste(params.outDir, "customer-main-type-purchase.pdf"), width = 10, height = 10)
}

par(mar = c(10, 5, 5, 5))
barplot(MOSHOOFD.purchase$Yes.Freq, names = MOSHOOFD.labels, main = "Customer main type ~ Purchase", las = 2)

if (params.fileOutput) {
  dev.off()
}

# Label factors in MOSTYPE
MOSTYPE.labels <- c("High Income","Very Important Provincials","High status seniors","Affluent senior apartments","Mixed seniors","Career and childcare","Dinki's (double income no kids)","Middle class families","Modern","Stable family","Family starters","Affluent young families","Young all american family","Junior cosmopolitan","Senior cosmopolitans","Students in apartments","Fresh masters in the city","Single youth","Suburban youth","Etnically diverse","Young urban have-nots","Mixed apartment dwellers","Young and rising","Young","Young seniors in the city","Own home elderly","Seniors in apartments","Residential elderly","Porchless seniors: no front yard","Religious elderly singles","Low income catholics","Mixed seniors","Lower class large families","Large family","Village families","Couples with teens 'Married with children'","Mixed small town dwellers","Traditional families","Large religous families","Large family farms","Mixed rurals")

# Create MOSTYPE distribution barplot
if (params.fileOutput) {
  pdf(paste(params.outDir, "customer-sub-type-distribution.pdf"), width = 10, height = 10)
}

# Create MOSTYPE table and fill missing values with 0
MOSTYPE.table <- as.vector(table(MOSTYPE))
for (i in 1:41) {
  if (!(i %in% MOSTYPE)) {
    message(i)
    MOSTYPE.table <- c(MOSTYPE.table[1:i-1], 0, MOSTYPE.table[i:length(MOSTYPE.table)])
  }
}

par(mar = c(5, 18, 5, 5), cex = 0.85)
barplot(MOSTYPE.table, names = MOSTYPE.labels, main = "Customer subtype distribution", las =  2, horiz = TRUE)

if (params.fileOutput) {
  dev.off()
} 

# Calculate the percentage of people who purchased a caravan policy in every group
MOSTYPE.purchase <- as.data.frame(MOSTYPE.labels)
MOSTYPE.purchase$Count <- MOSTYPE.table
MOSTYPE.purchase$No <- table(MOSTYPE.table, Purchase)[, 1]
MOSTYPE.purchase$No.Freq <- MOSTYPE.purchase$No / MOSTYPE.purchase$Count
MOSTYPE.purchase$Yes <- table(MOSTYPE.table, Purchase)[, 2]
MOSTYPE.purchase$Yes.Freq <- MOSTYPE.purchase$Yes / MOSTYPE.purchase$Count

# Plot the distribution
if (params.fileOutput) {
  pdf(paste(params.outDir, "customer-main-type-purchase.pdf"), width = 10, height = 10)
}

par(mar = c(10, 5, 5, 5))
barplot(MOSTYPE.purchase$Yes.Freq, names = MOSTYPE.labels, main = "Customer main type ~ Purchase", las = 2)

if (params.fileOutput) {
  dev.off()
}