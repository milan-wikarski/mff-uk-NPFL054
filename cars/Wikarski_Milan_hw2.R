## PARAMETERS ##
params.fileOutput <- TRUE
params.workDir <- "~/School/NPFL054/cars/"



#########################################
##              PART 00                ##
#########################################

# Install packages
# .libPaths("~/R/libs")
# pacman::p_load("pacman", "randomcoloR")

# Initiate global variables
# global.colors <- 

global.colors <- c()
for (i in 1:50) {
  global.color <- c(global.colors, paste("#", paste(sample(c(0:9, LETTERS[1:6]), 6, T), collapse = ""), sep = ""))
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
data <- read.csv("Auto.csv", sep = ",")
attach(data)



#########################################
##              PART 01                ##
#########################################

#
# (A): Simple linear regression
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
# (B): Multiple linear regression
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
# (C): Polynomial regression
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

for (degree in 1:DEGREES_COUNT) {
  # Create polynomial regression models degrees 1 to 5
  model <- lm(mpg ~ poly(acceleration.sorted, degree))

  # Plot the polynomial regression lines degrees 1 to 5
  points(
    acceleration.sorted,
    predict(model),
    type = "l",
    lwd = 3,
    col = global.colors[degree]
  )

  # Retrieve the R-squared value
  r.squared <- c(r.squared, summary(model)$r.squared) 
}

legend(
  8.3, 46,
  legend = paste(c(1:DEGREES_COUNT), "Deg."),
  col = global.colors,
  lty = 1,
  lwd = 3,
  cex = 1.2
)

plot(
  r.squared,
  ylim = c(0, 1),
  xlab = "Degree",
  ylab = "R-squared",
  main = "R-squared of polynomial regression models with various degrees"
)

for (i in 1:DEGREES_COUNT) {
  text(i, r.squared[i] + 0.04, round(r.squared[i], digits = 4))
}

if (params.fileOutput) {
  dev.off()
}



#########################################
##              PART 02                ##
#########################################