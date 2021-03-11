install.packages("kknn")
library(kknn)
install.packages("knn")
library(knn)
help("kknn")
nyt1<-read.csv("C:/Users/petert6/Desktop/Data_Analytics/lab4/nyt_data/nyt1.csv")
nyt1<-nyt1[which(nyt1$Impressions>0 & nyt1$Clicks>0 & nyt1$Age>0),]
nnyt1<-dim(nyt1)[1]		# shrink it down!
sampling.rate=0.9
num.test.set.labels=nnyt1*(1.-sampling.rate)
training <-sample(1:nnyt1,sampling.rate*nnyt1, replace=FALSE)
train<-subset(nyt1[training,],select=c(Age,Impressions))
testing<-setdiff(1:nnyt1,training)
test<-subset(nyt1[testing,],select=c(Age,Impressions))
cg<-nyt1$Gender[training]
true.labels<-nyt1$Gender[testing]
classif<-kknn(train = train,test = test,cg,k=5) #
classif
attributes(.Last.value) 
