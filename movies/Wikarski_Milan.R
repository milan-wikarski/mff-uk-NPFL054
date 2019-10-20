source("C:\\Users\\Wiki\\Projects\\school\\machine-learning\\movies\\load-mov-data.R")



#########################################
##              PART 02                ##
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

# Create a data frame with columns "Var1" (ID), "Freq" (count)
movies.count <- as.data.frame(table(examples$movie))

# Vector with IDs of movies that have exactly 67 reviews
boxplot.movies.ids <- as.vector(movies.count[movies.count$Freq == 67, 1])

# Data frame containing 
boxplot.examples <- as.data.frame(examples[examples$movie %in% boxplot.movies.ids, ])

# Draw the actual boxplot 
boxplot(
  boxplot.examples$rating ~ boxplot.examples$movie,
  main = "Movies rated 67 times",
  xlab = "",
  ylab = "",
  names = unique(boxplot.examples$title),
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
users.ratings <- table(examples[, c(2, 3)])
users.ratings <- as.data.frame(cbind(users.ratings, total = rowSums(users.ratings)))

# Calculate relative frequency of 1-5 stars rating for each user
users.ratings.rel <- as.data.frame(round(users.ratings / users.ratings$total, 2))

# Extend users by adding column `nratings`
users$nratings = users.ratings$total

# Extend users by adding colums `one`, `two`, `three`, `four`, `five` with relative frequency
users$one = users.ratings.rel$`1`
users$two = users.ratings.rel$`2`
users$three = users.ratings.rel$`3`
users$four = users.ratings.rel$`4`
users$five = users.ratings.rel$`5`

colnames(users)

dist(users[, c(2, 7:11)])

head(users[, c(2, 7:11)])

hist(users$nratings)

min(users$age)
max(users$age)

#users.ratings <- as.data.frame(cbind(users.ratings, rel1 = users.ratings[1] / users.ratings$total))

#users.ratings.rel <- table(users.ratings) / users.ratings$total

#users.ratings
