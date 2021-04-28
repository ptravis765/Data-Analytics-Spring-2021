#install.packages("lme4")
#install.packages("lmer")
#install.packages("statmod")
#install.packages("MASS")
library(lme4)
library(statmod)
library(MASS)
# Now, you have the function lmer() available to you, which is the mixed model
# equivalent of the function lm()

# obtain the data
politeness<-read.csv("http://www.bodowinter.com/uploads/1/2/9/3/129362560/politeness_data.csv")
politeness
# you will see that there there is a missing value in row 39.

# check for missing values
which(is.na(politeness$frequency))
# you will see that there there is a missing value in row 39

# a few missing values provide no problems for our mixed model analyses.
# The difference in politeness level is represented in the column called "attitude".
# In that column, "pol" stands for polite and "inf" for informal. Sex is represented as "F" and "M" in the column "gender". The dependent measure is "frequency",
# which is the voice pitch measured in Hertz (Hz). To remind you, higher values
# mean higher pitch.

boxplot(frequency ~ attitude*gender,col=c("white","lightgray"),politeness)

politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)

summary(politeness.model)




library(MASS)
library(nlme)
data(oats)
names(oats) = c('block', 'variety', 'nitrogen', 'yield')
oats$mainplot = oats$variety
oats$subplot = oats$nitrogen
summary(oats)
library(nlme)
m1.nlme = lme(yield ~ variety*nitrogen, random = ~ 1|block/mainplot, data = oats)
summary(m1.nlme)
anova(m1.nlme)

# Fitting a Regression Trees
library(MASS)
library(tree)
set.seed(1)
help("Boston") # Read the documentation for the Boston Dataset.
head(Boston)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston)
# Note that the output summary() indicates that only three of the variables have been
# used to constructing the tree. In the context of a regression tree,
# the deviance is simply the sum of squared errors for the tree.



# Regression Tree
tree(formula = medv ~. , data = Boston, subset = train)
# We now plot the tree
plot(tree.boston)
text(tree.boston, pretty = 0)
# The variable "lstat" measure the percentage of the individuals with lower socioeconimics status.
# The tree indicates that the lower values of lstat corresponds to more expensive houese.
# Now we use the cv.tree() function to see whether pruning the tree will
# improve performance.
help("cv.tree")
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,typ ='b')



#In this case, the most complex tree is selected by cross-validation.
# However, if we wish to prune the tree, we could do so as follows,
#using the prune.tree() function
help("prune.tree") # Read the documentation of the prune.tree() function.
prune.boston=prune.tree(tree.boston ,best=5)
# best= integer requesting the size (i.e. number of terminal nodes) of a specific
# subtree in the cost-complexity sequence to be returned.
# This is an alternative way to select a subtree than by supplying a scalar cost-complexity parameter k.
# If there is no tree in the sequence of the requested size, the next largest is returned.
plot(prune.boston)
text(prune.boston ,pretty=0)




# In keeping with the cross-validation results,
# we use the unpruned tree to make predictions on the test set.
yhat=predict(tree.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat,boston.test)
# adding the abline()
abline(0,1)
mean((yhat-boston.test)^2)
# In other words, the test set MSE associated with the regression tree is 25.05.
# The square root of the MSE is therefore around 5.005,
# indicating that this model leads to test predictions that
# are within around $5, 005 of the true median home value for the suburb.





# Bagging and Random Forest Example
library(randomForest)
set.seed(1)
bag.boston = randomForest(medv ~., data=Boston, subset = train, mtry=13, importance= TRUE)
bag.boston
# The argument mtry=13 indicates that all 13 predictors should be considered
# for each split of the tree-in other words, that bagging should be done.
# How well does this bagged model perform on the test set?
yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
plot(yhat.bag, boston.test)
# adding the abline()
abline(0,1)
mean((yhat.bag-boston.test)^2)



# The test set MSE associated with the bagged regression tree is 13.16,
#almost half that obtained using an optimally-pruned single tree.
#We could change the number of trees grown by randomForest() using the ntree argument: 
bad.boston = bag.boston=randomForest(medv~.,data=Boston,subset=train, mtry=13,ntree=25)
yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
mean((yhat.bag-boston.test)^2)


set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6,importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)
# The test set MSE is 11.31;
# this indicates that random forests yielded an improvement over bagging in this case.
# Using the importance() function, we can view the importance of each variable.



importance (rf.boston)
# Two measures of variable importance are reported.
#The former is based upon the mean decrease of accuracy in predictions on
# the out of bag samples when a given variable is excluded from the model.
#The latter is a measure of the total decrease in node impurity that results
# from splits over that variable, averaged over all trees.
# In the case of regression trees, the node impurity is measured by the training RSS,
# and for classification trees by the deviance.
# Plots of these importance measures can be produced using the varImpPlot() function.
varImpPlot (rf.boston)


install.packages("caret")
install.packages("caret", dependencies=c("Depends", "Suggests"))
library(caret)

# https://machinelearningmastery.com/machine-learning-in-r-step-by-step/
# attach the iris dataset to the environment
data(iris)
# rename the dataset
dataset <- iris
head(dataset)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]
# dimensions of dataset
dim(dataset)


# list types for each attribute
sapply(dataset, class)
# take a peek at the first 6 rows of the data
head(dataset)
# list the levels for the class
levels(dataset$Species)
# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)


# summarize attribute distributions
summary(dataset)
# plots
# split input and output
x <- dataset[,1:4]
y <- dataset[,5]
# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}


# barplot for class breakdown
plot(y)
# Multivariate Plots
# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")
# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")
# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv",number=10)
metric <- "Accuracy"
#Build Models
# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# KNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)


# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
# compare accuracy of models
dotplot(results)
# summarize Best Model
print(fit.lda)
# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
# Read: What is Kappa: https://www.r-bloggers.com/k-is-for-cohens-kappa/












