#install.packages("cvTools")
#install.packages("robustbase")
library(cvTools)
library(robustbase)
data(coleman, package="robustbase")
call <- call("lmrob", formula = Y ~ .)
# set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
# perform cross-validation
cvTool(call, data = coleman, y = coleman$Y, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))
#vary K and R
#look at cvfits, use densityplot, 
tuning <- list(tuning.psi=seq(2., 6., 20))
cvFitsLmrob <- cvTuning(call, data = coleman, y = coleman$Y, tuning = tuning, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))
densityplot(cvFitsLmrob)
# look at output
cvFitsLmrob
# summarize
aggregate(cvFitsLmrob, summary)
