source("~/Projects/school/NPFL054/movies/load-mov-data.R")


#########################################
##              PART 01                ##
#########################################

# Returns probability table
probability <- function(x) {
  return(table(x) / length(x))
}

# Returns the entropy of a given feature
entropy <- function(x) {
  p <- probability(x)
  return (-sum(p * log2(p)))
}

entropy(examples$occupation)
entropy(examples$rating)



#########################################
##              PART 02                ##
#########################################

wrap.it <- function(x, len) { 
  sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
}

# Call this function with a list or vector
wrap.labels <- function(x, len) {
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

remove.last.7 <- function(cs) {
  return(gsub('.{7}$', '', cs))
}

# Create a data frame with columns "Var1" (ID), "Freq" (count)
movies.count <- as.data.frame(table(examples$movie))

# Vector with IDs of movies that have exactly 67 reviews
boxplot.movies.ids <- as.vector(movies.count[movies.count$Freq == 67, 1])

# Data frame containing all examples with IDs contained in boxplot.movies.ids 
boxplot.examples <- as.data.frame(examples[examples$movie %in% boxplot.movies.ids, ])

par(mar=c(10, 8, 8, 4))

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


#########################################
##              PART 03                ##
#########################################

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

head(users)

# Create normalized version of users using Z-score
# users.normalized <- users
# users.normalized$age = scale(users$age, mean(users$age), sd(users$age))
# users.normalized$one = scale(users$one, mean(users$one), sd(users$one))
# users.normalized$two = scale(users$two, mean(users$two), sd(users$two))
# users.normalized$three = scale(users$three, mean(users$three), sd(users$three))
# users.normalized$four = scale(users$four, mean(users$four), sd(users$four))
# users.normalized$five = scale(users$five, mean(users$five), sd(users$five))

# Perform clustering
users.hc <- hclust(dist(users[, c(2, 8:12)]), method = "average")
# users.normalized.hc <- hclust(dist(users.normalized[, c(2, 8:12)]), method = "average")

# Plot dendrogram
plot(users.hc, main = "Users")
rect.hclust(users.hc, k = 20, border = "gray")
rect.hclust(users.hc, k = 3, border = "red")
# plot(users.normalized.hc, main = "Users [Normalized]")
# rect.hclust(users.normalized.hc, k = 20, border = "gray")
# rect.hclust(users.normalized.hc, k = 3, border = "red")

# Split users into clusters
# Each user will be assigned a cluster ID (number in {1, 2, ..., 20})
users$cluster = cutree(users.hc, k = 20)

# Compute the number of users in each cluster
# Var1: cluster ID
# Freq: number of users in cluster
users.hc.clusters.count <- as.data.frame(table(users$cluster))

#barplot(users.hc.clusters.count$Freq, names.arg=users.hc.clusters.count$Var1)

# Compute the average age of users in each cluster
# Group.1: cluster ID
# x: average age of users in cluster
users.hc.clusters.mean <- aggregate(users[, 2], list(users$cluster), mean)

# Check if some users have same values for attributes age, one, two, three, four, five and cluste
# All FALSE => no duplicates in any group 
table(duplicated(users[, c(2, 8:13)]))