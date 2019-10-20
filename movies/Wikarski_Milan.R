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
boxplot(
  boxplot.examples$rating ~ boxplot.examples$movie,
  main = "Movies rated 67 times",
  xlab = "",
  ylab = ""
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
