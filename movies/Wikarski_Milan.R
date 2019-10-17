source("C:\\Users\\Wiki\\Projects\\school\\machine-learning\\movies\\load-mov-data.R")

colnames(examples)
colnames(movies)



#########################################
##              PART 01                ##
#########################################

# Create a data frame with columns "Var1" (ID), "Freq" (count)
movies.count <- as.data.frame(table(examples$movie))

# Vector with IDs of movies that have exactly 67 reviews
boxplot.movies.ids <- as.vector(movies.count[movies.count$Freq == 67, 1])

# Data frame containing 
boxplot.examples <- as.data.frame(examples[examples$movie %in% movies.boxplot.ids, ])

# Draw the actual boxplot 
boxplot(boxplot.examples$rating ~ boxplot.examples$movie)



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


?boxplot










examples[examples$movie]

length(examples$rating)

entropy(examples$rating)
entropy(examples$occupation)

plot(examples$occupation, las=2)

colnames(examples)

colnames(movies)

(genres.count <- colSums(movies[, c(4:21)])) 
c(which.max(genres.count), max(genres.count))

barplot(genres.count, las=2)

for (i in 4:21) {
  c(colnames(movies)[i], nrow(movies[movies[i] == 1, ]))
}

entropy(movies$genre_action)
barplot(table(movies$genre_action))

