#################################################################################
#################################################################################

### Linear regression -- analytical solution, Gradient Descent Algorithm, lm()

### Logistic regression -- glm()

### Barbora Hladka, Martin Holub

### http://ufal.mff.cuni.cz/course/npfl054

#################################################################################
############################################

###############
## EXERCISE #1: Gradient Descent Algorithm: a simple illustration
## 		Credit: https://www.r-bloggers.com/gradient-descent-in-r/
###############

# define the loss function
f <-  function(x) {
1.2 * (x-2)^2 + 3.2
}

# derivative of f along x df/dx 
grad <- function(x){
	1.2*2*(x-2)
}

# plot the loss function f
# ! assume theta0 = 0, theta1 <> 0
theta1s <- seq(0, 4, len = 1000) 

plot(theta1s , f(theta1s),
	type = "l",
	xlab = expression(theta[1]),
	ylab = "")

title(ylab = expression(1.2(theta[1]-2)^2 + 3.2),
	line = 1.7,
	cex.lab = 1)

###############
## EXERCISE # 1.1: Plot minimum of f -- closed-form solution
###############
lines (c(2,2), c(3.2,8),
	col = "red",
	lty = 2)

text (2, 7, "Closed-form solution",
	col = "red",
	pos = 4)

###############
## EXERCISE # 1.2: Minimum of f -- Gradient Descent Algorithm
###############
theta1 <- 0.1	 # initialize theta1
alpha <- 0.3	 # set up learning step
iter <- 20     # set up number of iterations

# History of theta - start with initial values
theta1s <- theta1
f.theta1s <- f(theta1)

for (step in 1:iter) {
  # gradient descent update
  theta1 <- theta1 - alpha*grad(theta1)
  
  # Append values to the theta history
  theta1s <- c(theta1s, theta1)
  f.theta1s <- c(f.theta1s, f(theta1))
}

lines (theta1s, f.theta1s,
	type = "b",
	col = "blue")

text (0.5, 6, "Gradient Descent",
	col = "blue",
	pos = 4)
 
## Experiment with different values of alpha

###############
## EXERCISE # 2: Read Auto data set
###############

Auto <- read.csv("~/School/NPFL054/cars/Auto.csv", sep = ",");
summary(Auto)

## preprocessing
origins <- c('USA', 'Europe', 'Japan')
Auto$origin <- factor(Auto$origin, labels = origins)

# analysis
attach(Auto)

plot(table(cylinders), ylab="count")
plot(table(year), ylab="count")
plot(table(origin), ylab="count")

message("\n\n#####   Correlation of weight and mpg #####")
cor(Auto[ , c('mpg', 'weight', 'displacement', 'horsepower', 'acceleration')])

message("\n\n###### Display the internal structure of the example data ####")
examples <- cbind(weight, mpg)
num.examples <- nrow(examples)
plot(examples, 
	xlab = "weight",
	ylab = "mpg",
	pch = 21,
	cex = 1.5,
	col = "black",
	bg = "blue")

detach(Auto)

# boxplot mpg and origin
x <- as.list(as.data.frame(cbind(
  subset(Auto$mpg, Auto$origin == "USA"),
  subset(Auto$mpg, Auto$origin == "Europe"),
  subset(Auto$mpg, Auto$origin == "Japan")
)))

names(x)[1] <- "USA"
names(x)[2] <- "Europe"
names(x)[3] <- "Japan"

boxplot(x, 
	main = "MPG comparison by Origin",
	ylab ="mpg",
	las = 1,
	par(cex.axis=0.8),
	horizontal = FALSE)

# boxplot weight and origin
x <- as.list(as.data.frame(cbind(
  subset(Auto$weight, Auto$origin == "USA"),
  subset(Auto$weight, Auto$origin == "Europe"),
  subset(Auto$weight, Auto$origin == "Japan")
)))

names(x)[1] <- "USA"
names(x)[2] <- "Europe"
names(x)[3] <- "Japan"

boxplot(x, 
	main = "Weight comparison by Origin",
	ylab ="weight",
	las = 1,
	par(cex.axis=0.8),
	horizontal = FALSE)

# boxplot mpg and model year
x <- as.list(as.data.frame(cbind(
  subset(Auto$mpg, Auto$year == 70),
  subset(Auto$mpg, Auto$year == 71),
  subset(Auto$mpg, Auto$year == 72),
  subset(Auto$mpg, Auto$year == 73),
  subset(Auto$mpg, Auto$year == 74),
  subset(Auto$mpg, Auto$year == 75),
  subset(Auto$mpg, Auto$year == 76),
  subset(Auto$mpg, Auto$year == 77),
  subset(Auto$mpg, Auto$year == 78),
  subset(Auto$mpg, Auto$year == 79),
  subset(Auto$mpg, Auto$year == 80),
  subset(Auto$mpg, Auto$year == 81),
  subset(Auto$mpg, Auto$year == 82)
)))

names(x)[1] <-  "70"
names(x)[2] <-  "71"
names(x)[3] <-  "72"
names(x)[4] <-  "73"
names(x)[5] <-  "74"
names(x)[6] <-  "75"
names(x)[7] <-  "76"
names(x)[8] <-  "77"
names(x)[9] <-  "78"
names(x)[10] <-  "79"
names(x)[11] <-  "80"
names(x)[12] <-  "81"
names(x)[13] <-  "82"

boxplot(x, 
	main = "MPG comparison by Model Year",
	ylab ="mpg",
	las = 1,
	par(cex.axis=0.8),
	horizontal = FALSE)

###############
## EXERCISE #3: Linear regression
###############

###############
## EXERCISE #3.1: Simple linear regression with the Auto data set
###############

# target attribute: mpg
# feature: weight
# loss function: RSS

attach(Auto)
## solution analytically
y <- as.matrix(mpg)
# add column of 1s into X
weight.1 <- rep(1,num.examples)
X <- as.matrix(cbind(weight.1, weight))
theta <- solve(t(X) %*% X) %*% (t(X) %*% y)	# matrix inversion using 'solve'

plot(examples,
	xlab = "weight",
	ylab = "mpg",
	pch = 21,
	cex = 1.5,
	col = "black",
	bg = "blue")

abline(theta[1], theta[2],
	col="red",
	lwd = 5)

message("\n\n##### Verifying that sum of residuals equals 0  #####")
colSums(X %*% theta - y)

## solution using lm()

# 1. mpg ~ weight 
message("\n\n#####   Building a simple linear regression predictor #####")
m.1 <- lm( mpg ~ weight, as.data.frame(examples) )
#  ~ "is modeled as" or "is a function of"
# y ~ model
message("\n Model built.")

# get hypothesis parameters
m.1
summary(m.1)
coef(m.1)
attributes(m.1)

message("\n\n#####   Draw the fitting line   #####")
plot(examples,
	main = "ISLR: Auto data set",
	xlab = "Weight",
	ylab = "Miles Per Gallon",
	pch = 21,
	cex = 1.5,
	col = "black",
	bg  = "blue")

abline(a = m.1$coefficients[[1]], b = m.1$coefficients[[2]],
	col = 'red',
	lwd = 5)
detach(Auto)

## summary statistics
# statistical hypothesis testing

# H_0 (null hypothesis): the theta parameter associated with the feature is equal to zero.
# H_1 (alternate hypothesis): the theta parameter is not equal to zero. (i.e. there exists a relationship between the target attribute and the feature)

# test using t-statistics

# Pr(>|t|) = p value is the probability that we get a t-value 

# significance stars = significance level alpha; the more stars, the more significant feature

# We reject H0 if p <= alpha 

# 2. mpg ~ origin
examples <- Auto[,c(1,8)]

message("\n\n#####   Building a simple linear regression predictor #####")
m.2 <- lm( mpg ~ origin, examples )

# hypothesis parameters
m.2
summary(m.2)
coef(m.2)
attributes(m.2)

summary(m.2)$r.squared
summary(m.2)$adj.r.squared

################
## EXERCISE #3.2: Build a polynomial regression predictor
################

# the plots require the data to be sorted
sorted <- Auto[order(Auto$weight), ] 
attach(sorted)

fit1 <- lm(mpg ~ poly(weight, 1), data=sorted) 
fit2 <- lm(mpg ~ poly(weight, 2), data=sorted) 
fit5 <- lm(mpg ~ poly(weight, 5), data=sorted) 


plot(weight, mpg, main="ISLR: Auto data set", 
	xlab = "Weight",
	ylab = "Miles Per Gallon",
	pch = 19,
	col = "red")
 
points(weight, predict(fit1), type="l", lwd=5, col="blue") 
points(weight, predict(fit2), type="l", lwd=5, col="orange") 
points(weight, predict(fit5), type="l", lwd=5, col="green") 

legend("topright",
	c("Linear", "Degree 2", "Degree 5"),
	col = c("blue", "orange", "green"),
	lty = c(1,1,1),
	lwd=c(5,5,5))

detach(sorted)

################
## EXERCISE #3.3:  Contour and three-dimensional plots: an illustration
################

# Residual Sum of Squares
RSS <- function(theta0, theta1) {
  th <- c(theta0, theta1)
  rss <- t(X %*% th - y) %*% (X %*% th - y)
  return(rss)
}

##########

trans3d <- function(x,y,z, pmat) { 
  tr <- cbind(x,y,z,1) %*% pmat 
  list(x = tr[,1]/tr[,4], y= tr[,2]/tr[,4]) 
} 

theta0 <- seq(26, 66, 2)
theta1 <- seq(-0.014, 0, 0.001)

VecFun <- Vectorize( RSS )
z    <- outer(theta0, theta1, FUN = "VecFun")

# sample points

th0.min <- 46.21652
th1.min <- -0.007647

th0.1 <- 38
th1.1 <- -0.004

th0.2 <- 60
th1.2 <- -0.009

# drawing

# 3D
res <- persp(theta0, theta1, z,
	theta = 40,
	phi = 35,
	border = "blue",
	ticktype='detailed',
	main = "Loss Function L has a minimum value at the red point", 
	zlab = "L(theta0, theta1)")

points(trans3d(th0.min, th1.min, RSS(th0.min, th1.min), res),
	col = 2,
	cex = 3,
	pch = 19)

points(trans3d(th0.1, th1.1, RSS(th0.1, th1.1), res),
	col = "sandybrown",
	cex = 3,
	pch = 19)

points(trans3d(th0.2, th1.2, RSS(th0.2, th1.2), res),
	col = "orchid",
	cex = 3,
	pch = 19)

 
# contour plot
contour(theta0, theta1, z, nlevels = 40,
	col = "blue",
	main = "Contours of Loss Function",
	xlab = "theta0",
	ylab = "theta1")

points(th0.min, th1.min,
	col = 2,
	cex = 3,
	pch = 19)

points(th0.1, th1.2,
	col = "sandybrown",
	cex = 3,
	pch = 19)
points(th0.2, th1.2,
	col = "orchid",
	cex = 3,
	pch = 19)


###############
## EXERCISE #4: Logistic regression with the data set on students
## Credit: https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
###############

################
## EXERCISE #4.1: Explore the data and split it into training and test sets
################

examples <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/sample.csv", header=T)

## explore the data
str(examples)
num.examples <- nrow(examples)

# encode the target feature as factor
examples$hon = factor(examples$hon, levels = c(0, 1))

## split data into training and test subsets

# set the number of training examples = 90% of all examples
num.train <- round(0.9 * num.examples)
# set the number of test examples = 10% of all examples
num.test <- num.examples - num.train

# randomly split examples into training and test data using sample()
# use set.seed() to be able to reconstruct the experiment 
# with the SAME training and test sets

set.seed(123)
s <- sample(num.examples) 

# get the training set
# first, generate indices of training examples
indices.train <- s[1:num.train]
# second, get the training examples
train <- examples[indices.train,]
# get the test set 
indices.test <- s[(num.train+1):num.examples]
test <- examples[indices.test,]

hon <- nrow(subset(examples, hon == 1))
not.hon <- nrow(subset(examples, hon == 0))

# count p(y=1|x) from the data
p <- hon/num.examples

## odds from the data
# odds of a female being in the honours class 32:77
p1 <- nrow(subset(examples, ((female==1) & (hon == 1)))) / nrow(subset(examples, female==1))
odds.f.hon <- p1/(1-p1)
# odds of a male being in the honours class 17:74
p2 <- nrow(subset(examples, ((female==0) & (hon == 1)))) / nrow(subset(examples, female==0))
odds.m.hon <- p2/(1-p2)
# ratio of the odds for female to the odds for male
odds.f.hon/odds.m.hon
#[1] 1.809015
# the odds for female are about 81% higher than the odds for males

################
## EXERCISE #4.2: Build a logistic regression classifier with no feature
##		  target value: hon	
################

m.0 <- glm(hon ~ -., 
	data = examples, 
	family = binomial(link = 'logit'))

message("\nModel built.")
m.0
# Coefficients:
# (Intercept)
#    -1.125
theta0 <- m.0$coefficients[1]
# (Intercept) 
#   -1.12546 

# i.e. log(h(x))/(1-h(x)) = -1.125
# h(x) = p(y=1/x), i.e.
exp(theta0)/(1+exp(theta0))
# 0.245	--> see p above
# i.e. theta0 is the estimated log odds of being in honors class for the whole population


################
## EXERCISE #4.3: Build a logistic regression classifier with a single categorical feature
##		  target value: hon
################

# hon ~ female
m.1 <- glm(hon ~ female,
	data = examples,
	family = binomial(link = 'logit'))

message("\nModel built.")
m.1

# Coefficients:
# (Intercept)       female  
#    -1.4709       0.5928

# theta_0 = -1.471 is the log odds for males (male is the reference group)
log(odds.m.hon)
# [1] -1.470852

# theta_1 = 0.5928 is the log of odds ratio between the female group and male group
log(odds.f.hon/odds.m.hon)

################
## EXERCISE #4.4: Build a logistic regression classifier with both categorical and continuous features
##		  target value: hon
##		  Train on the train data and test on the test data		
################

# features read, math and target attribute hon
d <- examples[, c(2,4,5)]
train <- train[, c(2,4,5)]
test <- test[, c(2,4,5)]

# fitting logistic regression to the training set
m.2 <- glm(formula = hon ~ .,
                  family = binomial,
                  data = train)

# predicting the test set results
prob_pred <- predict(m.2, type = 'response', newdata = test)

# classfication rule
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

# making the confusion matrix
cm <- table(test[, 3], y_pred > 0.5)

# accuracy
sum(diag(cm))/sum(cm)

## visualising the test set results
library(ElemStatLearn)
set <- test
X1 <- seq(min(set[, 1]) - 2, max(set[, 1]) + 2, by = 0.07)
X2 <-  seq(min(set[, 2]) - 2, max(set[, 2]) + 2, by = 0.07)

grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('read', 'math')

prob_set <- predict(m.2, type = 'response', newdata = grid_set)
threshold <- 0.5
y_grid <- ifelse(prob_set > threshold, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'read', ylab = 'math',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set,
       pch = '.',
       col = ifelse(y_grid == 1, 'lightpink', 'lightblue'))
points(set,
       pch = 21, 
       cex = 1.5,
       bg = ifelse(set[, 3] == 1, 'red', 'blue'))
