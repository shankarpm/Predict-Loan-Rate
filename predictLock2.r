library(hydroGOF)
library(leaps)
library(dplyr)
install.packages("Boruta")
library(Boruta)

dir.Name <- 'Projects\\UW Loan Rate'
train <- read.csv(paste(dir.Name,"\\Train3.csv",sep=''))
test <- read.csv(paste(dir.Name,"\\test.csv",sep=''))

source(paste(dir.Name,"\\SourceFunctions.r",sep=''))
returnProcessdata <-  ProcessImputeData()
train <-  returnProcessdata$train
test <- returnProcessdata$test
FeatureRanking()
 
train_sample <- returnProcessdata$trainsample
TrainLockMonth <- returnProcessdata$trainlockmonth
TestLockMonth <- returnProcessdata$testlockMonth

#Predict using linear regression
rmse <- PredictUsingLM()

print(paste('LM - Root mean square error' , rmse$rmse.lm))
#Predict using decision tree
rmseDT <- PredictUsingDT()

print(paste('DT - Root mean square error' , rmseDT$rmse.dt))
#Predict using random forest
rmseRF <-PredictUsingRF()
print(paste('Rf - Root mean square error' , rmseRF$rmse.rf))
#Predict using GBM
rmseGBM <- PredictUsingGBM()
print(paste('GBM - Root mean square error' , rmseGBM$rmse.gbm))

#Predict using xgboost
ptm <- proc.time()
rmseXgboost <- PredictUsingXgBoost()
proc.time() - ptm
print(paste('GBM - Root mean square error' , rmseXgboost$rmse.xgBosst))

 


