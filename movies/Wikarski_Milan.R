## PARAMETERS ##
params.fileOutput <- TRUE
params.workDir <- "~/School/NPFL054/movies/"


#########################################
##              PART 00                ##
#########################################

# Set working directry and load data
setwd(params.workDir)
source("load-mov-data.R")

# Helper functions

# Wraps string into multiple lines
wrap.it <- function(x, len) { 
  sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
}

# Call this function with a list or vector
wrap.labels <- function(x, len) {
  if (is.list(x)) {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

# Removes last 7 chars of a string
remove.last.7 <- function(cs) {
  return(gsub('.{7}$', '', cs))
}

# Returns probability table
probability <- function(x) {
  return(table(x) / length(x))
}

# Returns the entropy of a given feature
entropy <- function(x) {
  p <- probability(x)
  return (-sum(p * log2(p)))
}

# Global chart parameters
par(cex = 1)

# Create /out directory
if (params.fileOutput && !dir.exists("out")) {
  dir.create("out")
}


#########################################
##              PART 01                ##
#########################################

entropy(examples$occupation)
entropy(examples$rating)



#########################################
##              PART 02                ##
#########################################

# Create a data frame with columns "Var1" (ID), "Freq" (count)
movies.count <- as.data.frame(table(examples$movie))

# Vector with IDs of movies that have exactly 67 reviews
boxplot.movies.ids <- as.vector(movies.count[movies.count$Freq == 67, 1])

# Data frame containing all examples with IDs contained in boxplot.movies.ids 
boxplot.examples <- as.data.frame(examples[examples$movie %in% boxplot.movies.ids, ])

if (params.fileOutput) {
  pdf("out/boxplot.pdf", width = 10, height = 10)
}

par(mar=c(12, 6, 6, 6), mfrow = c(1, 1))

# Draw the actual boxplot 
boxplot(
  boxplot.examples$rating ~ boxplot.examples$movie,
  main = "Movies rated 67 times",
  xlab = "",
  ylab = "",
  names = remove.last.7(wrap.labels(unique(boxplot.examples$title), 20)),
  las = 2
)

for (i in 1:length(boxplot.movies.ids)) {
  points(
    i,
    mean(boxplot.examples$rating[boxplot.examples$movie == boxplot.movies.ids[i]]),
    col = "red",
    pch = 19,
  )
}

if (params.fileOutput) {
  dev.off()
}



#########################################
##              PART 03                ##
#########################################

#
# (A)
#

# Group examples by users and calculate the number of ratings for each user
# This creates a data frame containing:
#   - count of n-star ratings for n in {1, 2, ..., 5}
#   - count of all ratings 
users.ratings <- table(examples[, c(2, 3)])
users.ratings <- as.data.frame(cbind(users.ratings, total = rowSums(users.ratings)))

# Calculate relative frequency of 1-5 stars rating for each user
# This creates a data frame with same structure as users.ratings
#   but uses relative count instead of absolute
users.ratings.rel <- as.data.frame(round(users.ratings / users.ratings$total, 2))

# Extend users by adding column nratings (= number of all ratings)`
users$nratings = users.ratings$total

# Extend users by computing column avg (= average rating)
users$avg = aggregate(examples[, c(3)], list(examples$user), mean)

# Extend users by adding colums `one`, `two`, `three`, `four`, `five` with relative frequency
users$one = users.ratings.rel$`1`
users$two = users.ratings.rel$`2`
users$three = users.ratings.rel$`3`
users$four = users.ratings.rel$`4`
users$five = users.ratings.rel$`5`

# Create normalized version of users using Z-score
users.zscore <- users
users.zscore$age = scale(users$age, mean(users$age), sd(users$age))
users.zscore$one = scale(users$one, mean(users$one), sd(users$one))
users.zscore$two = scale(users$two, mean(users$two), sd(users$two))
users.zscore$three = scale(users$three, mean(users$three), sd(users$three))
users.zscore$four = scale(users$four, mean(users$four), sd(users$four))
users.zscore$five = scale(users$five, mean(users$five), sd(users$five))

# Create normalized version of users using min-max normalization
users.minmax <- users
users.minmax$age = (users$age - min(users$age)) / (max(users$age) - min (users$age)) 
users.minmax$one = (users$one - min(users$one)) / (max(users$one) - min (users$one)) 
users.minmax$two = (users$two - min(users$two)) / (max(users$two) - min (users$two)) 
users.minmax$three = (users$three - min(users$three)) / (max(users$three) - min (users$three)) 
users.minmax$four = (users$four - min(users$four)) / (max(users$four) - min (users$four)) 
users.minmax$five = (users$five - min(users$five)) / (max(users$five) - min (users$five)) 

# Create normalized version of users using mean normalization
users.meannorm <- users
users.meannorm$age = (users$age - mean(users$age)) / (max(users$age) - min (users$age)) 
users.meannorm$one = (users$one - mean(users$one)) / (max(users$one) - min (users$one)) 
users.meannorm$two = (users$two - mean(users$two)) / (max(users$two) - min (users$two)) 
users.meannorm$three = (users$three - mean(users$three)) / (max(users$three) - min (users$three)) 
users.meannorm$four = (users$four - mean(users$four)) / (max(users$four) - min (users$four)) 
users.meannorm$five = (users$five - mean(users$five)) / (max(users$five) - min (users$five)) 


#
# (B)
#

# Perform clustering
users.hc <- hclust(dist(users[, c(2, 8:12)]), method = "average")
users.zscore.hc <- hclust(dist(users.zscore[, c(2, 8:12)]), method = "average")
users.minmax.hc <- hclust(dist(users.minmax[, c(2, 8:12)]), method = "average")
users.meannorm.hc <- hclust(dist(users.meannorm[, c(2, 8:12)]), method = "average")


#
# (C)
#

# Split users into clusters
# Each user will be assigned a cluster ID (number in {1, 2, ..., 20})
users$cluster = cutree(users.hc, k = 20)
users.zscore$cluster = cutree(users.zscore.hc, k = 20)
users.minmax$cluster = cutree(users.minmax.hc, k = 20)
users.meannorm$cluster = cutree(users.meannorm.hc, k = 20)


#
# (D)
#

# Compute the number of users in each cluster
#   Var1: cluster ID
#   Freq: number of users in cluster
users.hc.clusters.count <- as.data.frame(table(users$cluster))
users.zscore.hc.clusters.count <- as.data.frame(table(users.zscore$cluster))
users.minmax.hc.clusters.count <- as.data.frame(table(users.minmax$cluster))
users.meannorm.hc.clusters.count <- as.data.frame(table(users.meannorm$cluster))

# Compute the average age of users in each cluster
#   cluster: cluster ID
#   mean: average age of users in cluster
users.hc.clusters.mean <- aggregate(users[, 2], list(users$cluster), mean)
names(users.hc.clusters.mean) <- c("cluster", "mean")

users.zscore.hc.clusters.mean <- aggregate(users[, 2], list(users.zscore$cluster), mean)
names(users.zscore.hc.clusters.mean) <- c("cluster", "mean")

users.minmax.hc.clusters.mean <- aggregate(users[, 2], list(users.minmax$cluster), mean)
names(users.minmax.hc.clusters.mean) <- c("cluster", "mean")

users.meannorm.hc.clusters.mean <- aggregate(users[, 2], list(users.meannorm$cluster), mean)
names(users.meannorm.hc.clusters.mean) <- c("cluster", "mean")

# Check if some users have same values for attributes age, one, two, three, four, five and cluster
# All FALSE => no duplicates in any group 
table(duplicated(users[, c(2, 8:13)]))


#
# (*)
#

# Plot dendrograms
if (params.fileOutput) {
  pdf("out/dendrograms.pdf", width = 28, height = 7)
}

par(mar=c(4, 4, 4, 4))
par(mfrow = c(1,4))

plot(users.hc, main = "Users [No Normalization]", labels = FALSE, xlab = "", sub = "")
rect.hclust(users.hc, k = 20, border = "blue4")

plot(users.zscore.hc, main = "Users [Z-score Normalized]", labels = FALSE, xlab = "", sub = "")
rect.hclust(users.zscore.hc, k = 20, border = "blue4")

plot(users.minmax.hc, main = "Users [Z-score Normalized]", labels = FALSE, xlab = "", sub = "")
rect.hclust(users.minmax.hc, k = 20, border = "blue4")

plot(users.meannorm.hc, main = "Users [Z-score Normalized]", labels = FALSE, xlab = "", sub = "")
rect.hclust(users.meannorm.hc, k = 20, border = "blue4")

if (params.fileOutput) {
  dev.off()
}

# Plot barplots
if (params.fileOutput) {
  pdf("out/barplots.pdf", width = 20, height = 10)
}

par(mfrow = c(2,4))

# Without normalization - cluster size
barplot(
  t(users.hc.clusters.count$Freq),
  main = "Cluster size [No Normalization]",
  names=c(1:20)
)

# Z-scores normalization - cluster size
barplot(
  t(users.zscore.hc.clusters.count$Freq),
  main = "Cluster size [Z-score Normalized]",
  names=c(1:20)
)

# Min-Max normalization - cluster size
barplot(
  t(users.minmax.hc.clusters.count$Freq),
  main = "Cluster size [Min-Max Normalized]",
  names=c(1:20)
)

# Mean normalization - cluster size
barplot(
  t(users.meannorm.hc.clusters.count$Freq),
  main = "Cluster size [Mean Normalized]",
  names=c(1:20)
)

# Without normalization - average age
barplot(
  t(users.hc.clusters.mean$mean),
  main = "Average age in cluster [No Normalization]",
  names=c(1:20)
)

# z-scores normalization - average age
barplot(
  t(users.zscore.hc.clusters.mean$mean),
  main = "Average age in cluster [Z-score Normalized]",
  names=c(1:20)
)

# Min-Max normalization - average age
barplot(
  t(users.minmax.hc.clusters.mean$mean),
  main = "Average age in cluster [Min-Max Normalized]",
  names=c(1:20)
)

# Mean normalization - average age
barplot(
  t(users.meannorm.hc.clusters.mean$mean),
  main = "Average age in cluster [Mean Normalized]",
  names=c(1:20)
)

if (params.fileOutput) {
  dev.off()
}

# END.