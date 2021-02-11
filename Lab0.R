install.packages("MASS") # installing the MASS package
library(MASS) # load the library MASS

boston <- Boston

attach(boston)# attaching the dataset
?Boston # help function with "?"
head(boston) # shows the head of the dataset
dim(boston) #dimensions of the dataset

names(boston) #column names

str(boston) # str function shows the structure of the dataset

nrow(boston) # function shows the number of rows

ncol(boston)

summary(boston)

summary(boston$crim) # summary of the "crime" column in the boston dataset

install.packages("ISLR")

library(ISLR)
data(Auto)
head(Auto)
names(Auto)
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg)
boxplot(Auto$mpg)
hist(Auto$mpg)
summary(Auto$horsepower)
summary(Auto$weight)
fivenum(Auto$weight)
boxplot(Auto$weight)
mean(Auto$weight)
median((Auto$weight))

help(read.csv)
data1 <-read.csv(file.choose(),header=T)
data1
