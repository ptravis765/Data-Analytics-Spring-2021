indicators_last_7_days <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Assignment6/data/Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")
dmh <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Assignment6/data/dmhasnsduhmh2018.csv")
lghc <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Assignment6/data/Health_Data_gov/adult-depression-lghc-indicator-24.csv")
ques <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Assignment6/data/students_mental/data.csv")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }


summary(indicators_last_7_days)
summary(dmh)
summary(lghc)
summary(ques)


str(indicators_last_7_days)
str(dmh)
str(lghc)
str(ques)


names(indicators_last_7_days)
names(dmh)
names(lghc)
names(ques)


for(col.name in colnames(indicators_last_7_days)){
  print(col.name)
  print(unique(indicators_last_7_days[[col.name]]))
}

for(col.name in colnames(dmh)){
  print(col.name)
  print(unique(dmh[[col.name]]))
}

for(col.name in colnames(lghc)){
  print(col.name)
  print(unique(lghc[[col.name]]))
}

for(col.name in colnames(ques)){
  print(col.name)
  print(unique(ques[[col.name]]))
}


table(lghc$Strata)
barplot(table(lghc$Strata))


############################################################################################################################

# Based on the results from these bar graphs it looks like there has been no change in the amount of people with 
# mental illness 
library(ggplot2)
val1 <- length(dmh$Mental.Health[dmh$Mental.Health == 1])
val2 <- length(dmh$Mental.Health[dmh$Mental.Health == 2])
val3 <- length(dmh$Mental.Health[dmh$Mental.Health == 3])
val4 <- length(dmh$Mental.Health[dmh$Mental.Health == 4])
values <- c(val1,val2,val3,val4)
length(unique(dmh$Mental.Health))
colors = c("green","orange","brown","red")
MH <- unique(dmh$Mental.Health)
barplot( table(dmh$Year),beside=TRUE, col=colors )
legend("topleft", MH, cex = 1.3,legend= c(1:4), fill = colors)
table(dmh$Year)

ggplot(dmh, aes(fill=Mental.Health, y=Mental.Health, x=Year)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(data=dmh, aes(x=Year,y=Mental.Health))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_text(aes(label=Mental.Health),vjust=-1.6,color="white",size=3.5)+
  theme_minimal()
str(dmh)
dmh$Value <- normalize((dmh$Value))
ggplot(data=dmh,aes(x=Year,y=Value,fill=Mental.Health))+
  geom_bar(stat="identity")
ggplot(data=dmh,aes(x=Age.Range,y=Year,fill=Mental.Health))+
  geom_bar(stat="identity")
ggplot(data=dmh,aes(x=Region,y=Year,fill=Mental.Health))+
  geom_bar(stat="identity")

names(sort(summary(as.factor(dmh$Mental.Health))))
# This line is supposed to give the most common occurence but all these values occur on the same frequency

barplot((table(indicators_last_7_days$Time.Period.Label)))
barplot((table(lghc$Year)))
############################################################################################################################


#install.packages("dplyr")
library(dplyr)

# https://stackoverflow.com/questions/34059017/replace-factors-with-a-numeric-value

# Converting 'Strata', 'Strata.Name', 'X', and 'X.1' to numerical factors
for (col.name in colnames(lghc)){
  if(is.numeric(lghc[[col.name]]) == FALSE){
    #print(col.name)
    #print(unique(lghc[[col.name]]))
    lghc[[col.name]] <- as.numeric(factor(lghc[[col.name]]))
    #print("")
    #print("")
    
  }
}
# Get rid of 'X' and 'X.1' since they have nothing but NA making them useless 
lghc <- select(lghc,-c('X','X.1'))
lghc[is.na(lghc)] <- 0
summary(lghc)


# converting "Indicator", "Group", "State", "Subgroup", "Phase", "Time.Period.Label", "Time.Period.Start.Date", 
# "Time.Period.End.Date", "Confidence.Interval", and "Quartile.Range" to numerical factors
for (col.name in colnames(indicators_last_7_days)){
  if(is.numeric(indicators_last_7_days[[col.name]]) == FALSE){
    #print(col.name)
    indicators_last_7_days[[col.name]] <- as.numeric(factor(indicators_last_7_days[[col.name]]))
  }
}
indicators_last_7_days[is.na(indicators_last_7_days)] <- 0
summary(indicators_last_7_days)


# converting "Region", "Year", "Age.Range", "Mental.Health", "Measure.Type", "Variable", and "X" to numerical factors
dmh_titles <- factor(c(unique(dmh$Mental.Health)))
for (col.name in colnames(dmh)){
  if(is.numeric(dmh[[col.name]]) == FALSE){
    dmh[[col.name]] <- as.numeric(factor(dmh[[col.name]]))
  }
}
# Get rid of 'X' because it has nothing but NA values
dmh <- select(dmh,-c('X'))
dmh[is.na(dmh)] <- 0
summary(dmh)
barplot( table(dmh$Mental.Health,dmh$Age.Range),beside=TRUE,legend=dmh_titles )






# converting "??..inter_dom", "Region", "Gender", "Academic", "Stay_Cate", "Japanese_cate", "English_cate", "Intimate", 
# "Religion", "Suicide", "Dep", "DepType", "DepSev", "Partner_bi", "Friends_bi", "Parents_bi", "Relative_bi", 
# "Professional_bi", "Phone_bi", "Doctor_bi", "religion_bi", "Alone_bi", "Others_bi", and "Internet_bi" to numerical factors
for (col.name in colnames(ques)){
  if(is.numeric(ques[[col.name]]) == FALSE){
    #print(col.name)
    ques[[col.name]] <- as.numeric(factor(ques[[col.name]]))
  }
}
# get rid of internet because it has nothing but NA values
#ques <- select(ques,-c('Internet'))
ques[is.na(ques)] <- 0
summary(ques)
str(ques)

barplot( table(ques$Dep,ques$Region),beside=TRUE,legend=c("No","Yes") )
barplot( table(ques$Dep,ques$Gender),beside=TRUE,legend=c("No","Yes"))
barplot( table(ques$Dep,ques$Academic),beside=TRUE,legend=c("No","Yes"))


plot(Year ~ Frequency, data = lghc)

plot(Year ~ Value, data = dmh)

with(dmh, hist(Mental.Health))
with(dmh, hist(Age.Range))
barplot((table(dmh$Year)))
barplot((table(dmh$Age.Range)))

table(dmh$Age.Range)







# Identify outliers
#####################################################
summ <- function(x){
  for (col.name in colnames(x)){
    print(col.name)
    print(summary(x[[col.name]]))
  }
  
}

summ(dmh)
# From this 'Value', and 'Measure.Type' do not look like they will be any good to the analysis
dmh <- select(dmh,-c('Measure.Type'))
dmh <- select(dmh,-c('Variable'))


summ(lghc)
summ(ques)
summ(indicators_last_7_days)
#####################################################


################################################################################
# EFA for survey data
head(ques)
results <- prcomp(ques, scale = TRUE)
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
library(ggplot2)

qplot(c(1:50), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot")

summary(ques)
# column 3 (Gender) is when the elbow drops but column 6 (Age_cate) is where it starts to steadily decrease

################################################################################



# bar charts for categorical data





# build 2 models
#   -cluster analysis
#   -vector alanysis
#   -regression analysis 




# Kmeans cluster
# https://uc-r.github.io/kmeans_clustering
# https://www.datanovia.com/en/blog/cluster-analysis-in-r-simplified-and-enhanced/

# Determine number of clusters for indicators_last_7_days
tmp <- (nrow(indicators_last_7_days)-1)*sum(apply(indicators_last_7_days,2,var))
for(i in 2:15) tmp[i] <- sum(kmeans(indicators_last_7_days,centers=i)$withinss)
plot(1:15, tmp, type="b",xlab="Number of Clusters",ylab="Within groups sum of squares")
# At cluster 2-3 is when a bend occurs like with scree plot

# Determine number of clusters for dmh
tmp <- (nrow(dmh)-1)*sum(apply(dmh,2,var))
for(i in 2:15) tmp[i] <- sum(kmeans(dmh,centers=i)$withinss)
plot(1:15, tmp, type="b",xlab="Number of Clusters",ylab="Within groups sum of squares")
# At clusters 3-4 is when a weird bend occurs

# Determine number of clusters for lghc
tmp <- (nrow(lghc)-1)*sum(apply(lghc,2,var))
for(i in 2:15) tmp[i] <- sum(kmeans(lghc,centers=i)$withinss)
plot(1:15, tmp, type="b",xlab="Number of Clusters",ylab="Within groups sum of squares")
# At clusters 6-7 is when a bend occurs

# Determine number of clusters for ques
tmp <- (nrow(ques)-1)*sum(apply(ques,2,var))
for(i in 2:15) tmp[i] <- sum(kmeans(ques,centers=i)$withinss)
plot(1:15, tmp, type="b",xlab="Number of Clusters",ylab="Within groups sum of squares")
# At clusters 7-8 is when a bend occurs

#install.packages("tidyverse")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
#install.packages("factoextra")
library(factoextra) # clustering algorithms & visualization


indicator_KM <- kmeans(indicators_last_7_days, centers = 2, nstart = 25)
indicator_KM
dmh_KM <- kmeans(dmh, centers = 3, nstart = 25)
dmh_KM
lghc_KM <- kmeans(lghc, centers = 6, nstart = 25)
lghc_KM
ques_KM <- kmeans(ques, centers = 4, nstart = 25)
ques_KM





# https://rstudio-pubs-static.s3.amazonaws.com/375287_5021917f670c435bb0458af333716136.html#:~:text=Estimate%20number%20of%20Clusters,-We%20have%20shown&text=Above%20is%20the%20silhouette%20plot,efficient%20occurring%20when%20k%3D2.


fviz_cluster(indicator_KM, data = indicators_last_7_days)
# Enhanced hierarchical clustering
#     indicator.hc <- eclust(indicators_last_7_days,k=2,k.max=2, "hclust") # compute hclust
fviz_silhouette(indicator.hc,palette = "jco", ggtheme = theme_classic()) # silhouette plot
indicators_last_7_days %>%
  mutate(Cluster = indicator_KM$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
# Both cluster 1 and cluster 2 seem to represent the data equally


#dmh <- select(dmh,-c('Value'))
fviz_cluster(dmh_KM, data = dmh)
dmh.hc <- eclust(dmh,k=3,k.max=3, "hclust") # compute hclust
fviz_silhouette(dmh.hc,palette = "jco", ggtheme = theme_classic()) # silhouette plot
dmh %>%
  mutate(Cluster = dmh_KM$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
# Cluster 1 and 3 seem to have the best representation of the data


fviz_cluster(lghc_KM, data = lghc)
lghc.hc <- eclust(lghc,k=6,k.max=6, "hclust") # compute hclust
fviz_silhouette(lghc.hc,palette = "jco", ggtheme = theme_classic()) # silhouette plot
lghc %>%
  mutate(Cluster = lghc_KM$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
# Cluster 6 seems to be the best with cluster 2 closely following


fviz_cluster(ques_KM, data = ques)
ques.hc <- eclust(ques,k=4,k.max=4, "hclust") # compute hclust
fviz_silhouette(ques.hc,palette = "jco", ggtheme = theme_classic()) # silhouette plot
ques %>%
  mutate(Cluster = ques_KM$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
# Cluster 4 overall has the best representation




# Linear Regression
help("glm")
library(lme4)
library(statmod)
library(MASS)
library(nlme)
# For ques want to see about predicting gender, age, and region to see about possible chances for indicators
str(ques)
boxplot(Suicide ~ Gender*Age,col=c("blue","green"),ques)
boxplot(Suicide ~ Age*Region,col=c("blue","green"),ques)
boxplot(Suicide ~ Gender*Region,col=c("blue","green"),ques)

#ques.Gender.model = lmer(Suicide ~ (1|Gender) + (1|Age) + (1|Region), data=ques)
#summary(ques.Gender.model)
glm.fit.model1 = glm(Suicide~Gender+Age+Region, data = ques)
summary(glm.fit.model1)
suicide.prob <- predict(glm.fit.model1, type="response")
suicide.prob[1:10]
ques$Suicide
suicide.pred <- rep(1, length(ques$Suicide))
suicide.pred[suicide.prob > 1.23] = 2
length(suicide.pred)
length(ques$Suicide)
table(suicide.pred, ques$Suicide)
mean(suicide.pred == ques$Suicide)
help("predict")
xweight <- seq(1, 268)  
length(xweight)
length(suicide.prob)
plot(ques$Age, ques$Suicide, xlim=c(15,32), pch = 16, xlab = "Age", ylab = "Suicide")
lines(xweight, suicide.prob)
library(effects)
plot(allEffects(glm.fit.model1))





dmh <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Assignment6/data/dmhasnsduhmh2018.csv")
dmh$Value <- normalize((dmh$Value))
# converting "Region", "Year", "Age.Range", "Mental.Health", "Measure.Type", "Variable", and "X" to numerical factors
dmh_titles <- factor(c(unique(dmh$Mental.Health)))
for (col.name in colnames(dmh)){
  if(is.numeric(dmh[[col.name]]) == FALSE){
    dmh[[col.name]] <- as.numeric(factor(dmh[[col.name]]))
  }
}
# Get rid of 'X' because it has nothing but NA values
dmh <- select(dmh,-c('X'))
dmh[is.na(dmh)] <- 0
dmh <- select(dmh,-c('Measure.Type'))
dmh <- select(dmh,-c('Variable'))

# For dmh let's see if we can predict the severity of a person's mental health
str(dmh)
boxplot(Mental.Health ~ Age.Range*Region,col=c("blue","green"),dmh)
boxplot(Mental.Health ~ Age.Range*Year,col=c("blue","green"),dmh)
boxplot(Mental.Health ~ Region*Year,col=c("blue","green"),dmh)
boxplot(Mental.Health ~ Age.Range*Value,col=c("blue","green"),dmh)


mh.model = lm(Mental.Health ~ Age.Range + (Year) + (Region) + (Value), data=dmh)
summary(mh.model)
mental.health.prob <- predict(mh.model, interval="prediction")
mental.health.prob
length(mental.health.prob)
xweight <- seq(1,2592)
plot(dmh$Mental.Health, dmh$Age.Range, pch = 16, xlab = "Severity", ylab = "Age-range")
BO=lm(Mental.Health ~ Age.Range,data=dmh)
abline(BO)
# Does not seem like a good dataset to try and make predictions with
# The predictions always come out as a straight line indicating that anyone in the age range have the possibility of having the any of the severity of mental health 

mh.model = glm(Mental.Health ~ Age.Range + (Region) + (Value), data=dmh)
summary(mh.model)
mental.health.prob <- predict(mh.model, interval="prediction")
mental.health.prob
plot(dmh$Age.Range, dmh$Mental.Health, pch = 16, xlab = "Age-range", ylab = "Severity")

library(effects)
plot(allEffects(mh.model))
# Same thing with here







# For indicator_last_7_days let's see if we can predict the indicator with Value, Group, and confidence interval
str(indicators_last_7_days)
glm.fit.model3 = glm(Indicator~Value+Group+Confidence.Interval, data = indicators_last_7_days)
summary(glm.fit.model3)
ind.prob <- predict(glm.fit.model3, type="response")
range(ind.prob)
range(indicators_last_7_days$Indicator)
ind.pred <- rep(1, length(indicators_last_7_days$Indicator))
ind.pred[ind.prob > 1.5] = 2
ind.pred[ind.prob > 2] = 3
table(ind.pred, indicators_last_7_days$Indicator)
mean(ind.pred == indicators_last_7_days$Indicator)
length(ind.prob)
xweight <- seq(1, 5364)  
length(xweight)
plot(indicators_last_7_days$Group, indicators_last_7_days$Indicator, pch = 16, xlab = "Group", ylab = "Indicator")
lines(xweight, ind.prob)

# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
#install.packages("effects")
library(effects)
plot(allEffects(glm.fit.model3))




str(lghc)
glm.fit.model4 = glm(Strata ~ Year+Frequency+Weighted.Frequency+Percent, data=lghc)
summary(glm.fit.model4)
lghc.prob <- predict(glm.fit.model4, type="response")
lghc.pred <- rep(1,length(lghc$Strata))
lghc.pred
