###################################################################################################
#                               part 1 a
###################################################################################################


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
EPI_data <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Lab2/EPI_data.csv")
EPI = EPI_data$EPI
DALY = EPI_data$DALY
tf <- is.na(EPI)
E <- EPI[!tf]
tf <- is.na(DALY)
D <- DALY[!tf]
EPI_mean = mean(E)
EPI_median = median(E)
EPI_mode = getmode(E)
DALY_mean = mean(D)
DALY_median = median(D)
DALY_mode = getmode(D)

EPI_2010 <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Lab2/2010EPI_data.csv")
E_2010 <- EPI_2010$EPI
D_2010 <- EPI_2010$DALY
hist(E_2010)
hist(D_2010)

env_health <- EPI_2010$ENVHEALTH
ecosys <- EPI_2010$ECOSYSTEM
boxplot(env_health,ecosys)

qqplot(env_health,ecosys)

###################################################################################################
#                               part 1 b
###################################################################################################

EPI_data <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Lab2/EPI_data.csv")
attach(EPI_data)
ENVHEALTH <- EPI_data$ENVHEALTH
#tf <- is.na(ENVHEALTH)
#ENVHEALTH <- ENVHEALTH[!tf]
DALY <- EPI_data$DALY
#tf <- is.na(DALY)
#DALY <- DALY[!tf]
AIR_H <- EPI_data$AIR_H
#tf <- is.na(AIR_H)
#AIR_H <- AIR_H[!tf]
WATER_H <- EPI_data$WATER_H
#tf <- is.na(WATER_H)
#WATER_H <- WATER_H[!tf]
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH <- lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)
cENVH
DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW <- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV <- predict(lmENVH,NEW,interval = 'prediction')
cENV <- predict(lmENVH,NEW,interval = 'confidence')
pENV
AIR_E <- EPI_data$AIR_E
CLIMATE <- EPI_data$CLIMATE
lmAIR_E <- lm(AIR_E~DALY+AIR_H+WATER_H)
cAIR_E <- coef(lmAIR_E)
p_AIR_E <- predict(lmAIR_E, NEW, interval = 'prediction')
c_AIR_E <- predict(lmAIR_E, NEW, interval = 'confidence')

lmCLIMATE <- lm(CLIMATE~DALY+AIR_H+WATER_H)
cCLIMATE <- coef(lmCLIMATE)
p_CLIMATE <- predict(lmCLIMATE, NEW, interval = 'prediction')
c_CLIMATE <- predict(lmCLIMATE, NEW, interval = 'confidence')



###################################################################################################
#                               part 2 Exercise 1
###################################################################################################

dataset <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Lab2/dataset_multipleRegression.csv")
attach(dataset)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}


#data_split <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.7, 0.3))
#KNNtrain <- dataset[data_split==1,]
#KNNtest <- dataset[data_split==2,]


#Linear regression function

help(lm)
mm <- lm(formula = ROLL~UNEM+HGRAD,data=dataset)
mm
summary(mm)
attributes(mm)

#co = coef(summary(mm))
#predict(mm)
#estimates = co[,1]
#Roll_estimate <- estimates[1]
#unem_estimate <- estimates[2]
#hgrad_estimate <- estimates[3]
#new_prediction = (Roll_estimate)+(unem_estimate*7)+(hgrad_estimate*90)
#new_prediction

new_roll = data.frame(UNEM=7,HGRAD=90000)
predict(mm,new_roll)

mm_2 <- lm(formula = ROLL~UNEM+HGRAD+INC,data=dataset)

#co_2=coef(summary(mm_2))
#estimates_2 = co_2[,1]
#Roll_estimate_2 <- estimates_2[1]
#unem_estimate_2 <- estimates_2[2]
#hgrad_estimate_2 <- estimates_2[3]
#inc_estimate <- estimates_2[4]

#new_prediction = (Roll_estimate)+(unem_estimate*7)+(hgrad_estimate*90000)+(inc_estimate*25000)
#new_prediction

new_roll_2 <- data.frame(UNEM=7, HGRAD=90000, INC=25000)
predict(mm_2,new_roll_2)



###################################################################################################
#                               part 2 Exercise 2
###################################################################################################

abalone <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Lab2/abalone.csv")
attach(abalone)
colnames(abalone) <-c("sex","length","diameter","height","whole_weight","shucked_weight","viscera_weight","shell_weight","rings")
summary(abalone)
str(abalone)
summary(abalone$rings)
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35),labels=c("young", "adult", "old"))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)
aba <- abalone
aba$sex <- NULL
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
ind <- sample(2,nrow(aba), replace=TRUE,prob=c(0.7,0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
k <- ceiling(sqrt(2918))
library(class)
help("knn")
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k=55)
KNNpred
table(KNNpred)



###################################################################################################
#                               part2 Exercise 3
###################################################################################################

#install.packages("ggplot2")
#install.packages("colorspace")
library(ggplot2)
head(iris)
str(iris)
summary(iris)
help("sapply")
sapply(iris[,-5],var)
summary(iris)

set.seed(300)
k.max <- 12
wss <- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart=20,iter.max=20)$tot.withinss})
wss

plot(1:k.max,wss,type="b",xlab="Number of clusters(k)",ylab="Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart=20)
table(icluster$cluster,iris$Species)
