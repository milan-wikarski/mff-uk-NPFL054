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

# Load packages
pacman::p_load(pacman, ISLR, rpart, rpart.plot, RColorBrewer)

# Set working directry and load data
setwd(params.workDir)

# Global chart parameters
par(cex = 1.5)

# Create /out directory
if (params.fileOutput && !dir.exists("out")) {
  dir.create("out")
}

# Choose colors
colors <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], 30)

# Load data
data <- Caravan
attach(data)