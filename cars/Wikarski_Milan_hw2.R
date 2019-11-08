## PARAMETERS ##
params.fileOutput <- TRUE
params.workDir <- "~/School/NPFL054/cars/"



#########################################
##              PART 00                ##
#########################################

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
##              PART 00                ##
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

par(mfrow = c(2, 4), mar = c(6, 6, 6, 6))

for (name in cor.names) {
  # Plot values of all features in cor.names againts mpg
  plot(
    mpg ~ data[, c(name)],
    ylab = "mpg",
    xlab = name,
    main = paste("mpg ~ ", name, "\nr = ", round(cor[, c(name)][1], digits = 5))
  )

  # Create a simple linear model for each feature in cor.names
  model <- lm(mpg ~ data[, c(name)])

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
  main = "Residuals histogram",
  xlab = "error"
)

# Print Q-Q plot of residuals to visually chceck if they are normally distributed
qqnorm(
  model$residuals,
  main = "Normal Q-Q Plot of residuals"
)

if (params.fileOutput) {
  dev.off()
}

# Get and analyse the summary of the model
summary(model)