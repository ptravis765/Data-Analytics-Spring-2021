EPI_data <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Lab1/2010EPI_data.csv")
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI
tf <- is.na(EPI)
tf
E <- EPI[!tf]
E
summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)
help(stem)

plot(ecdf(EPI),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(EPI);qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

DALY <- EPI_data$DALY
tfd<- is.na(DALY)
tfd
D <- DALY[!tf]
D
summary(DALY)
fivenum(DALY,na.rm=TRUE)
stem(DALY)
hist(DALY)
hist(DALY,seq(30.,30.,1.0),prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)
plot(ecdf(DALY),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(DALY);qqline(DALY)
x<-seq(30,30,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

WH <- EPI_data$WATER_H
tfw<- is.na(WH)
tfw
W <- WH[!tfw]
W
summary(WH)
fivenum(WH,na.rm=TRUE)
stem(WH)
hist(WH)
hist(WH,seq(30.,30.,1.0),prob=TRUE)
lines(density(WH,na.rm=TRUE,bw=1.))
rug(WH)
plot(ecdf(WH),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(WH);qqline(WH)
x<-seq(30,30,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

boxplot(EPI,DALY,WH)
qqplot(EPI,DALY)
qqplot(EPI,WH)
qqplot(DALY,WH)

help(distributions)
EPILand <-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland,seq(30.,30.,1.0),prob=TRUE)
lines(density(Eland,na.rm=TRUE,bw=1.))
rug(Eland)
plot(ecdf(Eland),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(WH);qqline(WH)
x<-seq(30,30,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

EPI_no_water <- EPI[!No_surface_water]
Ewat <- EPI_no_water[!is.na(EPI_no_water)]
hist(Ewat)
hist(Ewat,seq(30.,30.,1.0),prob=TRUE)
lines(density(Ewat,na.rm=TRUE,bw=1.))
rug(Ewat)
plot(ecdf(Ewat),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(Ewat);qqline(Ewat)
x<-seq(30,30,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

EPI_des <- EPI[!Desert]
Edes <- EPI_des[!is.na(EPI_des)]
hist(Edes)
hist(Edes,seq(30.,30.,1.0),prob=TRUE)
lines(density(Edes,na.rm=TRUE,bw=1.))
rug(Edes)
plot(ecdf(Edes),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(Edes);qqline(Edes)
x<-seq(30,30,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

EPI_HPD <- EPI[!High_Population_Density]
Ehpd <- EPI_HPD[!is.na(EPI_HPD)]
hist(Ehpd)
hist(Ehpd,seq(30.,30.,1.0),prob=TRUE)
lines(density(Ehpd,na.rm=TRUE,bw=1.))
rug(Ehpd)
plot(ecdf(Ehpd),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(Ehpd);qqline(Ehpd)
x<-seq(30,30,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

EPI_SA <- EPI[EPI_regions]


GPW3 <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Lab1/GPW3_GRUMP_SummaryInformation_2010.csv")
View(GPW3)
attach(GPW3)
fix(GPW3)
PopulationPerUnit
tf <- is.na(PopulationPerUnit)
tf
A <- PopulationPerUnit[!tf]
A
summary(PopulationPerUnit)
fivenum(PopulationPerUnit,na.rm=TRUE)
stem(PopulationPerUnit)
hist(PopulationPerUnit)
hist(PopulationPerUnit,seq(30.,95.,1.0),prob=TRUE)
lines(density(PopulationPerUnit,na.rm=TRUE,bw=1.))
rug(PopulationPerUnit)

plot(ecdf(PopulationPerUnit),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(PopulationPerUnit);qqline(PopulationPerUnit)
x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)



wt <- read.csv(file = "/Users/petert6/Desktop/Data_Analytics/Lab1/water-treatment.csv")
View(wt)
attach(wt)
fix(wt)
PH.E
tf <- is.na(PH.E)
tf
A <- PH.E[!tf]
A
summary(PH.E)
fivenum(PH.E,na.rm=TRUE)
stem(PH.E)
hist(PH.E)
hist(PH.E,seq(30.,30.,1.0),prob=TRUE)
lines(density(PH.E,na.rm=TRUE,bw=1.))
rug(PH.E)

plot(ecdf(PH.E),do.points=FALSE,verticals=TRUE)
par(pty="s")
qqnorm(PH.E);qqline(PH.E)
x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)




#install.packages("readxl")
library("readxl")
#install.packages("xlsx")
library(xlsx)
data2 <- read.xlsx("/Users/petert6/Desktop/Data_Analytics/Lab1/2010EPI_data.xls",sheetName = "EPI2010_onlyEPIcountries")
View(data2)

boxplot(EPI_data$PopulationPerUnit)
boxplot(data2$Population07)
