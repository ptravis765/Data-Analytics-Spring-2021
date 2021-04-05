# ISLR: Introduction to Statistical Learning with R (textbook that we use in this class)
# Validation set example with Auto dataset
library(ISLR)
library(MASS)
library(boot)
set.seed(1)
# Read the cv.glm documentation
??cv.glm

#read the documentation of the sample function
help("sample")
train = sample(392,196)
# We use the subset option in the lm() function to fit a linear regression using
# only the observations corresponding to the trainig set
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
# Now we use predict() function to estimate the response for all 392 observations
# and we use the mean() function to calculate the MSE of the 196 observations in the 
# validation set. Note that the -train selects only the observations that are not in
# the training set
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# Therefore, the estimated test MSE for the linear regression fit is 23.26601

# We can use the poly() function to estimate test error for the quadratic and cubic regression
# Quadratic regression line
lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset=train) # Quadratic
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# Cubic regression line
lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# The error rates are: 18.71646 for quadratics and 18.79401 for cubic
# If we choose different trainign set instead, then we will obtain somewhat errors,
# on the validation set
set.seed(2)
train=sample(392,196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# the error rate is 25.72651
lm.fit2 <- lm(mpg~poly(horsepower,2),data=Auto,subset=train)# Quadratic
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# the error rate is 20.43036
lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset=train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# The error rate is 20.38533
# Using this split of the observations into a training set and validation set,
# we find that the validation set error rates for the models with linear, quadratic,
# and cubic terms are 23.29, 18.90 and 19.25 respectively.
# The model that predict mpg using a quadratic function of horsepower performs better,
# than a models that only involves only a linear function of horsepower, and there is a,
# little evidence in favor of a model that uses a cubic function of horsepower.



# k-Fold Cross Validation
# The cv.glm() function can also be used to implement k-fold CV.
# We once again, set a random seed and initialize a vector in which,
# we will store the CV errors corresponding to the polynomial fits of orders one to #ten.
# here the K =10
# Read the cv.glm documentation
??cv.glm
set.seed(17)
help("rep") # read the documentation for the rep() function in R.
cv.error.10 = rep(0,10) # read documentation, help("rep")
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10) $delta[1]
}
cv.error.10
# Notice the computation time is much shorter than LOOCV! :),
# This depends on your laptop performance :)
# We still see little evidence that using cubic or higher-order polynomials terms,
# leads to lower test error than simply using a quadratics fit.




# Random Forest example
install.packages("randomForest")
library(randomForest)

#Loading the dataset
data1 <- read.csv(file.choose(), header=TRUE)
head(data1)
# Adding the column names
colnames(data1) <- c("BuyingPrice","Maintenance","NumDoors","NumPersons","BootSpace","Safety","Condition")
head(data1)
str(data1)
# Let's take a look at the Levels of the Condition column
# Conditions has 4 levels: "acc" "good" "unacc" "vgood"
levels(data1$Condition)
summary(data1)



## Creating the "training dataset" and "Validation dataset"
# we will randomly choose 70% (0.7) of the data points for training and 30% (0.3) for validation
# First we need to set the seed
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)

help(randomForest) # Read the RandomForest Documentation
# Random Forest Model with the default parameters
model1 <- randomForest(Condition~ ., data = TrainSet, importance = TRUE)
model1
# By default, number of trees is 500 and number of variables tried at each split is 2 in this case

# Fine tuning the parameters of the RandomForest model
# we have increased the mtry to 6 from 2
# mtry = Number of variables randomly sampled as candidates at each split
# Note that the default values are different for
# classification (sqrt(p) where p is number of variables in x) and regression (p/3)

slide 35/58 of CrossValidation_BootStrap_Bagging_Random_Forest.pdf




