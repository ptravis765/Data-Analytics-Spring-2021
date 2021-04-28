##############

#      https://bookdown.org/tpinto_home/Regression-and-Classification/k-nearest-neighbours-regression.html

########



help("select")
library(dplyr)
library(tidyr)
# https://www.datacamp.com/community/tutorials/machine-learning-in-r#two


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

absent <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Assignment7/Absenteeism_at_work.csv",sep=",")
sobar <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Assignment7/sobar-72.csv",sep=",")


# The ',' in the work.load.average.day column was turning the data into a character so I had to get rid of it
absent$Work.load.Average.day <-  as.numeric(gsub(",","",absent$Work.load.Average.day))

absent <- select(absent,-c('ID'))

summary(absent)
summary(sobar)

for (col.name in colnames(absent)){
  #name <- colname(lghc[[i]])
  print(col.name)
  print(mean(absent[[col.name]]))
  print("")
  print("")
}

#install.packages("ggvis")
library(ggvis)
absent %>% ggvis(~Seasons, ~Age, fill = ~Reason.for.absence) %>% layer_points()

absent %>% ggvis(~Age, ~Reason.for.absence, fill = ~Social.drinker) %>% layer_points()
absent %>% ggvis(~Age, ~Reason.for.absence, fill = ~Social.smoker) %>% layer_points()
# Comparing the charts to eachother more of the absentees appear to be drinkers than smokers


absent %>% ggvis(~Age, ~absent$Work.load.Average.day, fill = ~Reason.for.absence) %>% layer_points()
# The rate of absentees from the previous graphs seem to match with this one in the case that the older a person is the less likely they are to be absent as well as have less work hours






barplot( table(absent$Absenteeism.time.in.hours,absent$Age),xlab="Age",beside=TRUE,legend=unique(absent$Absenteeism.time.in.hours) )
par(mfrow=c(1,1))
hist(absent$Age, main="Age of absentees", xlab="age", breaks=seq(20, 60, 1))
# The most common absentees seem to be between 28 and 38

hist(absent$Reason.for.absence, main="Cause of absentees", xlab="Cause", breaks=seq(0, 30, 1))
# 23 seems to be the most common cause for absence

hist(absent$Absenteeism.time.in.hours, main="hours of absence", xlab="Hours", breaks=seq(0, 130, 5))
# 0-5 hours is the most prevelant amount of absence time. What exactly does this mean?

hist(absent$Distance.from.Residence.to.Work, main="Distance from residence", xlab="Distance", breaks=seq(0, 56, 5))
# Between 25-30 and 50-55 miles from work have the most cases of people being absent

plot(Absenteeism.time.in.hours ~ Age, data = absent)
plot(Distance.from.Residence.to.Work ~ Age, data = absent)
plot(Seasons ~ Reason.for.absence, data = absent)
# There does not seem to be a most used/common reason for absence. All of them seem to to have an equal amount of use throughout the year

plot(Social.smoker ~ Reason.for.absence, data = absent)


# A KNN prediction that can be done is predicting the age of an absent person
# Or even the reason for being absent

ab <- absent
#ab <- as.data.frame(lapply(ab[1:length(ab)], normalize))
# Let's see if we can try to predict the reason for absence
#ab[2:length(ab)]
reason <- ab[1]
reason
ab_n <- as.data.frame(lapply(ab[2:length(ab)], normalize))
ab_n
summary(ab)
KNNtrain <- ab_n[1:518,]
KNNtest <- ab_n[519:741,]
KNN_train_labels = ab[1:518, 1]
KNN_test_labels = ab[519:741, 1]
library(class)
KNNtest[is.na(KNNtest)] <- 0
sum(is.na(KNNtest))
knn_test_pred <- knn(train = KNNtrain, test = KNNtest,cl= KNN_train_labels, k = 28)
knn_test_pred
KNN_test_labels[is.na(KNN_test_labels)] <- 0
#install.packages("gmodels")
require("gmodels")
library("gmodels")
CrossTable(x = KNN_test_labels, y = knn_test_pred, prop.chisq = FALSE)
print(sum(knn_test_pred==KNN_test_labels)/length(KNN_test_labels))
# Does not seem like I am able to make an accurate KNN model to predict the reason for absence

#install.packages("FNN")
library(FNN)   #knn regression
set.seed(1974)

knn3.ab <- knn.reg(train=ab$Age, y=ab$Reason.for.absence, test= data.frame(Age=seq(20,50)),k=3)
plot(ab$Age, ab$Reason.for.absence) 
lines(seq(20,50), knn3.ab$pred)  #adds the knn k=3 line

length(ab$Reason.for.absence)
length(ab$Age)
knn3.ab.pred <- knn.reg(train=ab[c("Age")], y=ab$Reason.for.absence, test=ab[c("Age")], k=3)
mse.knn3  <- mean((knn3.ab.pred$pred - ab$Reason.for.absence)^2)  
mse.knn3
# With a mean squared error of 97.57973 I don't think it's possible to try and make a model to accurately predict a reason for absence


knn3.ab <- knn.reg(train=ab$Work.load.Average.day, y=ab$Reason.for.absence, test= data.frame(Work.load.Average.day=seq(250000,300000)),k=3)
plot(ab$Work.load.Average.day, ab$Reason.for.absence) 
lines(seq(250000,300000), knn3.ab$pred)  #adds the knn k=3 line

knn3.ab.pred <- knn.reg(train=ab[c("Work.load.Average.day")], y=ab$Reason.for.absence, test=ab[c("Work.load.Average.day")], k=3)
mse.knn3  <- mean((knn3.ab.pred$pred - ab$Reason.for.absence)^2)  
mse.knn3
# Not able to do it here either




# Decision tree
library(rpart)
library(rpart.plot)
ab <- absent
#ab <- as.data.frame(lapply(ab, normalize))

dim(ab)
s_ab <- sample(740,490)
s_ab

ab_train <- ab[s_ab,]
ab_test <- ab[-s_ab,]
dim(ab_train)
dim(ab_test)

prop.table(table(ab_train$Reason.for.absence))
prop.table(table(ab_test$Reason.for.absence))

detectionTreeModel <- rpart(Reason.for.absence~.,ab_train, method="class")
detectionTreeModel
rpart.plot(detectionTreeModel,extra=106)

predict_unseen <-predict(detectionTreeModel, ab_test, type = 'class')


table_mat <- table(ab_test$Reason.for.absence, predict_unseen)
table_mat


accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
# Only able to get an accuracy score of 14%. This might mean that the data is not enough to try and predict a reason for absence


accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, ab_test, type = 'class')
  table_mat <- table(ab_test$Reason.for.absence, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(Reason.for.absence~., data = ab_train, method = 'class', control = control)
accuracy_tune(tune_fit)

# Even after tuning the tree the highest I was able to get is 23%. I think it's safe to say that the data is not able to predict a reason for absence





############################################################################################################
ab <- absent
#ab[is.na(ab)] <- 0
summary(ab)
ind <- sample(2,nrow(ab), replace=TRUE,prob=c(0.7,0.3))
KNNtrain <- ab[ind==1,]
KNNtest <- ab[ind==2,]

cla <- KNNtrain$Age
KNNtrain <- select(KNNtrain, Reason.for.absence, Distance.from.Residence.to.Work, Service.time, Work.load.Average.day, Social.drinker, Social.smoker)
cla_test <- KNNtest$Age
KNNtest <- select(KNNtest, Reason.for.absence, Distance.from.Residence.to.Work, Service.time, Work.load.Average.day, Social.drinker, Social.smoker)

KNNpred <- knn(train = KNNtrain, test = KNNtest, cl = cla, k=3, prob=TRUE)
ab_TestLabels <- data.frame(cla_test)

merge <- data.frame(KNNpred, ab_TestLabels)

names(merge) <- c("Predicted Age", "Observed Age")

merge
#install.packages("gmodels")
library(gmodels)
CrossTable(x = cla_test, y = KNNpred, prop.chisq=FALSE)
print(sum(KNNpred==cla_test)/length(cla_test))
# Regardless of the value I put in for k the percent value is always low. This mus mean the columns I thought were decion points are not enough

ACC <- 100*sum(KNNpred==cla_test)/length(cla_test)
ACC

plot.df = data.frame(KNNtest, predicted = KNNpred)
plot.df1 = data.frame(x=plot.df$Reason.for.absence,y=plot.df$Work.load.Average.day,predicted=plot.df$predicted)

#install.packages("plyr")
library(plyr)
library(ggplot2)
find_hull = function(df) df[chull(df$x,df$y),]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

ggplot(plot.df, aes(Reason.for.absence, Work.load.Average.day, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)

######################################################################################################################


# https://archive.ics.uci.edu/ml/datasets/Cervical+Cancer+Behavior+Risk
for (col.name in colnames(sobar)){
  #name <- colname(lghc[[i]])
  print(col.name)
  print(mean(sobar[[col.name]]))
  print("")
  print("")
}

hist(sobar$ca_cervix, main="1 = has cervical cancer, 0 = does not", xlab="Decider", breaks=seq(0, 1, 0.5))
# Most of the data seems to involve the patients that do not have cervical cancer

hist(sobar$motivation_strength, main="Motivational strength", xlab="strength", breaks=seq(0, 20, 1))
# It seems the highest number 15 involves the most data. Does this mean that people with no cervical cancer have higher motivation








# Models to use = KNN and Random forest/Logisitic regression/linear regression



#sob <- select(sobar,behavior_sexualRisk,behavior_eating,behavior_personalHygine,intention_aggregation,intention_commitment,attitude_consistency,attitude_spontaneity,norm_significantPerson,norm_fulfillment,perception_vulnerability,perception_severity,motivation_willingness,socialSupport_emotionality,socialSupport_appreciation,socialSupport_instrumental,empowerment_knowledge,empowerment_abilities,empowerment_desires,ca_cervix)
#cla <- sobar$motivation_strength
sob <- sobar

ind <- sample(2,nrow(sob), replace=TRUE,prob=c(0.7,0.3))
KNNtrain <- sob[ind==1,]
KNNtest <- sob[ind==2,]

cla <- KNNtrain$motivation_strength
KNNtrain <- select(KNNtrain,behavior_sexualRisk,behavior_eating,behavior_personalHygine,intention_aggregation,intention_commitment,attitude_consistency,attitude_spontaneity,norm_significantPerson,norm_fulfillment,perception_vulnerability,perception_severity,motivation_willingness,socialSupport_emotionality,socialSupport_appreciation,socialSupport_instrumental,empowerment_knowledge,empowerment_abilities,empowerment_desires,ca_cervix)
cla_test <- KNNtest$motivation_strength
KNNtest <- select(KNNtest,behavior_sexualRisk,behavior_eating,behavior_personalHygine,intention_aggregation,intention_commitment,attitude_consistency,attitude_spontaneity,norm_significantPerson,norm_fulfillment,perception_vulnerability,perception_severity,motivation_willingness,socialSupport_emotionality,socialSupport_appreciation,socialSupport_instrumental,empowerment_knowledge,empowerment_abilities,empowerment_desires,ca_cervix)
library(class)
help("knn")
length(KNNtrain)
KNNpred <- knn(train = KNNtrain, test = KNNtest, cl = cla, k=1)
KNNpred
table(KNNpred)

summary(sobar)


library(ggvis)
sobar %>% ggvis(~behavior_sexualRisk, ~behavior_eating, fill = ~ca_cervix) %>% layer_points()

plot(sobar$socialSupport_emotionality,sobar$socialSupport_appreciation)
sobar %>% ggvis(~socialSupport_emotionality, ~socialSupport_appreciation, fill = ~ca_cervix) %>% layer_points()
# It appears that those with emotional support and appreciate it are less likely to have cancer

sobar %>% ggvis(~norm_fulfillment, ~norm_significantPerson, fill = ~ca_cervix) %>% layer_points()


# A prediction we can do for this is with different selections of criteria see how well it can determine of a person has cancer or not

sob <- sobar
cancer <- sob[20]
str(sob)

KNNtrain <- sob[1:48,]
KNNtest <- sob[49:73,]
KNN_train_labels = sob[1:48, 1]
KNN_test_labels = sob[49:73, 1]

library(class)
KNNtest[is.na(KNNtest)] <- 0
sum(is.na(KNNtest))
knn_test_pred <- knn(train = KNNtrain, test = KNNtest,cl= KNN_train_labels, k = 3)
knn_test_pred
KNN_test_labels[is.na(KNN_test_labels)] <- 0

require("gmodels")
library("gmodels")
CrossTable(x = KNN_test_labels, y = knn_test_pred, prop.chisq = FALSE)
print(sum(knn_test_pred==KNN_test_labels)/length(KNN_test_labels))
# With an accuracy of 88% it looks like this model can predict if a patient has cancer or not 


library(FNN)   #knn regression
set.seed(1974)

knn3.sob <- knn.reg(train=sob$attitude_consistency, y=sob$ca_cervix, test= data.frame(sob$attitude_consistency),k=3)
plot(sob$attitude_consistency, sob$ca_cervix) 
lines(knn3.sob$pred)

knn3.sob.pred <- knn.reg(train=sob[c("attitude_consistency")], y=sob$ca_cervix, test=sob[c("attitude_consistency")], k=3)
mse.knn3  <- mean((knn3.sob.pred$pred - sob$ca_cervix)^2)  
mse.knn3
# a mean squared error of 23% is good


knn3.sob <- knn.reg(train=sob$behavior_eating, y=sob$ca_cervix, test= data.frame(sob$behavior_eating),k=3)
plot(sob$behavior_eating, sob$ca_cervix) 
lines(knn3.sob$pred)

knn3.sob.pred <- knn.reg(train=sob[c("behavior_eating")], y=sob$ca_cervix, test=sob[c("behavior_eating")], k=3)
mse.knn3  <- mean((knn3.sob.pred$pred - sob$ca_cervix)^2)  
mse.knn3
# a mean squared error of 20% is even better



knn3.sob <- knn.reg(train=sob$socialSupport_emotionality, y=sob$ca_cervix, test= data.frame(sob$socialSupport_emotionality),k=3)
plot(sob$socialSupport_emotionality, sob$ca_cervix) 
lines(knn3.sob$pred)

knn3.sob.pred <- knn.reg(train=sob[c("socialSupport_emotionality")], y=sob$ca_cervix, test=sob[c("socialSupport_emotionality")], k=3)
mse.knn3  <- mean((knn3.sob.pred$pred - sob$ca_cervix)^2)  
mse.knn3
# a mean squared error of 15% is even better



knn3.sob <- knn.reg(train=sob$socialSupport_appreciation, y=sob$ca_cervix, test= data.frame(sob$socialSupport_appreciation),k=3)
plot(sob$socialSupport_appreciation, sob$ca_cervix) 
lines(knn3.sob$pred)

knn3.sob.pred <- knn.reg(train=sob[c("socialSupport_appreciation")], y=sob$ca_cervix, test=sob[c("socialSupport_appreciation")], k=3)
mse.knn3  <- mean((knn3.sob.pred$pred - sob$ca_cervix)^2)  
mse.knn3
# a mean squared error of 19% is good



knn3.sob <- knn.reg(train=sob$empowerment_abilities, y=sob$ca_cervix, test= data.frame(sob$empowerment_abilities),k=3)
plot(sob$empowerment_abilities, sob$ca_cervix) 
lines(knn3.sob$pred)

knn3.sob.pred <- knn.reg(train=sob[c("empowerment_abilities")], y=sob$ca_cervix, test=sob[c("empowerment_abilities")], k=3)
mse.knn3  <- mean((knn3.sob.pred$pred - sob$ca_cervix)^2)  
mse.knn3
# a mean squared error of 17% is good



knn3.sob <- knn.reg(train=sob$perception_vulnerability, y=sob$ca_cervix, test= data.frame(sob$perception_vulnerability),k=3)
plot(sob$perception_vulnerability, sob$ca_cervix) 
lines(knn3.sob$pred)

knn3.sob.pred <- knn.reg(train=sob[c("perception_vulnerability")], y=sob$ca_cervix, test=sob[c("perception_vulnerability")], k=3)
mse.knn3  <- mean((knn3.sob.pred$pred - sob$ca_cervix)^2)  
mse.knn3
# a mean squared error of 18% is good




knn3.sob <- knn.reg(train=sob$perception_severity, y=sob$ca_cervix, test= data.frame(sob$perception_severity),k=3)
plot(sob$perception_severity, sob$ca_cervix) 
lines(knn3.sob$pred)

knn3.sob.pred <- knn.reg(train=sob[c("perception_severity")], y=sob$ca_cervix, test=sob[c("perception_severity")], k=3)
mse.knn3  <- mean((knn3.sob.pred$pred - sob$ca_cervix)^2)  
mse.knn3
# a mean squared error of 23% is good







# Decision Tree
library(rpart)
library(rpart.plot)
sob <- sobar

dim(sob)
s_sob <- sample(72,48)
s_sob

sob_train <- sob[s_sob,]
sob_test <- sob[-s_sob,]
dim(sob_train)
dim(sob_test)


prop.table(table(sob_train$ca_cervix))
prop.table(table(sob_test$ca_cervix))

detectionTreeModel <- rpart(ca_cervix~.,sob_train, method="class")
detectionTreeModel
rpart.plot(detectionTreeModel,extra=106)

predict_unseen <-predict(detectionTreeModel, sob_test, type = 'class')


table_mat <- table(sob_test$ca_cervix, predict_unseen)
table_mat


accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
# An accuracy score of 83% is a pretty decent score to have 


accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, sob_test, type = 'class')
  table_mat <- table(sob_test$ca_cervix, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(ca_cervix~., data = sob_train, method = 'class', control = control)
accuracy_tune(tune_fit)








