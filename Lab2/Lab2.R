getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#2a
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


#2b
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


