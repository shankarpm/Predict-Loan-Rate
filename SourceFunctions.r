#install.packages("hydroGOF")
 #library(caret)
#require(xgboost)
library(hydroGOF)
library(leaps)
library(dplyr)
 
#train$LockDay <- gsub("/", "", substr(train$LockDay,1,2)) 
###############################Imputing the missing data#######################################################
ProcessImputeData <- function()
{
train$LockMonth <- as.numeric(format(as.Date(train$LockDay,'%m/%d/%Y'),'%m'))
train$LockYear <- as.numeric(format(as.Date(train$LockDay,'%m/%d/%Y'),'%Y'))

TrainLockMonth <- train$LockMonth
train$LockMonth <- as.factor(train$LockMonth)

train_calls_2016 <- train  %>% filter(LockYear == 2016)  %>% group_by(LockMonth,LockYear) %>% summarise(MeanCalls = mean(Calls)) %>% as.data.frame()
train_longcalls_2016 <- train  %>% filter(LockYear == 2016)  %>% group_by(LockMonth,LockYear) %>% summarise(MeanLongCalls = mean(LongCalls)) %>% as.data.frame()
train_leads_2013_2015 <- train  %>% filter(LockYear > 2012 &  LockYear < 2016 )  %>% group_by(LockMonth,LockYear) %>% summarise(MeanLeads = mean(Leads)) %>% as.data.frame()
train_leads_2013_2015 <- train_leads_2013_2015 %>% group_by(LockMonth) %>% summarise(MeanLeads = mean(MeanLeads)) %>% as.data.frame()

mean.2016calls <- mean(train_calls_2016$MeanCalls)
mean.2016Longcalls <- mean(train_longcalls_2016$MeanLongCalls)

train_calls_2016$LockYear <- NULL
train_longcalls_2016$LockYear <- NULL

train_calls_2016 <- rbind(train_calls_2016,c(10,mean.2016calls),c(11,mean.2016calls),c(12,mean.2016calls))
train_longcalls_2016 <- rbind(train_longcalls_2016,c(10,mean.2016Longcalls),c(11,mean.2016Longcalls),c(12,mean.2016Longcalls))
 
train_2016 <-  merge(x = train_calls_2016,y = train_longcalls_2016, by = "LockMonth" )  %>% arrange(LockMonth) %>% as.data.frame()
train_2016 <-  merge(x = train_2016,y = train_leads_2013_2015, by = "LockMonth" )  %>% arrange(LockMonth) %>% as.data.frame()

train_2016$MeanCalls <- round(train_2016$MeanCalls,0)
train_2016$MeanLongCalls <- round(train_2016$MeanLongCalls,0) 
train_2016$MeanLeads <- round(train_2016$MeanLeads,0) 

imputedData <-  merge(x = train,y = train_2016, by = "LockMonth" , all.x = T)  %>% arrange(as.Date(LockDay,'%m/%d/%Y')) %>% as.data.frame()

imputedData[which(imputedData$LockYear < 2016 & imputedData$Calls == 0),c('Calls')] <-  subset(imputedData[,c('MeanCalls')],imputedData$LockYear < 2016 & imputedData$Calls == 0)
imputedData[which(imputedData$LockYear < 2016 & imputedData$LongCalls == 0),c('LongCalls')] <-  subset(imputedData[,c('MeanLongCalls')],imputedData$LockYear < 2016 & imputedData$LongCalls == 0)
imputedData[which(imputedData$LockYear < 2013 & imputedData$Leads == 0),c('Leads')] <-  subset(imputedData[,c('MeanLeads')],imputedData$LockYear < 2013 & imputedData$Leads == 0)

write.csv(imputedData, paste(dir.Name,"\\processedImputedData.csv",sep=''),row.names = FALSE)
train <- imputedData

#train$LockDay <- gsub("/", "", substr(train$LockDay,1,2)) 
train$MeanCalls <- NULL
train$MeanLongCalls <- NULL
train$MeanLeads <- NULL
train$LockMonth <- as.numeric(format(as.Date(train$LockDay,'%m/%d/%Y'),'%m'))
train$LockYear <- as.numeric(format(as.Date(train$LockDay,'%m/%d/%Y'),'%Y'))
test$LockMonth <- as.numeric(format(as.Date(test$LockDay,'%m/%d/%Y'),'%m'))
test$LockYear <- as.numeric(format(as.Date(test$LockDay,'%m/%d/%Y'),'%Y'))

TestLockMonth <- test$LockMonth
train$LockMonth <- as.factor(train$LockMonth)
train$LockYear <- as.factor(train$LockYear) 
test$LockMonth <- as.factor(test$LockMonth)
test$LockYear <- as.factor(test$LockYear) 

train_sample <- test # copy test to trainsample for calculating rmse #train[sample(2:20),]
#test$Locks <- NULL
summary(train)
summary(test)
summary(train_sample)
 return(list(train=train, test=test , trainsample=train_sample,trainlockmonth = TrainLockMonth,testlockMonth = TestLockMonth))
}
#############################################################################################################
#######################feature selection####################################
FeatureRanking <- function()
{
boruta.train <- Boruta(Locks ~ Leads + LeSent + Calls + LongCalls + LockMonth + NewLoans + IntentToProceed + ApprOrdered, data = train, doTrace = 2)
print(boruta.train)
summary(boruta.train)


plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

# final.boruta <- TentativeRoughFix(boruta.train)
#  print(final.boruta)
}
#######################feature selection####################################

###################Model datapoints Selection using step ###################################################################
#null=lm(Locks~1, data=train)
#full=lm(Locks~., data=train)
#step(null, scope=list(lower=null, upper=full), direction="forward")
#step(full, data=train, direction="backward")
###########################################################################################################################
###################Prediction using Linear Regression#######################################################################
PredictUsingLM <- function()
{
lm.model <- lm(Locks ~ Leads + LeSent + Calls + LongCalls + LockMonth + NewLoans + IntentToProceed + ApprOrdered, data = train ) #A R-squared:   0.2867    p-value:  2.001e-10 rmse- 6.348545
summary(train)
summary(lm.model)
predict.lm.lockrates <- predict(lm.model,test) 
predict_sample.lm.lockrates <- predict(lm.model,train_sample)

test.output.lm <- cbind(test,predict.lm.lockrates)
train.sample.output.lm <- cbind(train_sample,predict_sample.lm.lockrates)

# rmse -  16.32341
rmse.lm <- rmse(train.sample.output.lm$Locks , train.sample.output.lm$predict_sample.lm.lockrates) 

test.output.lm$Locks <- test.output.lm$predict.lm.lockrates
test.output.lm$LockPred <- round(test.output.lm$Locks,0)
test.output.lm$Locks <- NULL
test.output.lm$LockMonth <- NULL
test.output.lm$LockYear <- NULL
test.output.lm$predict.lm.lockrates <- NULL 
write.csv(test.output.lm, paste(dir.Name,"\\PredictOutput.lm.csv",sep=''),row.names = FALSE)
return (list(rmse.lm = rmse.lm))
}
###################################################################################################
########################Prediction using Decision Tree###############################################
PredictUsingDT <- function()
{
library(rpart)
library(rpart.plot) 
rpart.model <- rpart(Locks ~LeSent + Leads + Calls + LongCalls + LockMonth + NewLoans + IntentToProceed + ApprOrdered,data = train) # dot for all predictors considered

rpart.plot(rpart.model,type=3,extra=101,fallen.leaves=T)
#summary(rpart.model)

predict.rpart.lockrates <- predict(rpart.model,test) 
predict_sample.rpart.lockrates <- predict(rpart.model,train_sample) 

test.output.rpart <- cbind(test,predict.rpart.lockrates)
train.sample.output.rpart <- cbind(train_sample,predict_sample.rpart.lockrates)

#root mean square - 16.72508
rmse.dt <-  rmse(train.sample.output.rpart$Locks , train.sample.output.rpart$predict_sample.rpart.lockrates) 

test.output.rpart$Locks <- test.output.rpart$predict.rpart.lockrates
test.output.rpart$LockPred <- round(test.output.rpart$Locks,0)
test.output.rpart$Locks <- NULL
test.output.rpart$LockMonth <- NULL
test.output.rpart$LockYear <- NULL
test.output.rpart$predict.rpart.lockrates <- NULL

write.csv(test.output.rpart, paste(dir.Name,"\\PredictOutput.rpart.csv",sep=''),row.names = FALSE)
return (list(rmse.dt = rmse.dt))
}
####################################################################################################
############################Prediction using Random Forest###########################################
PredictUsingRF <- function()
{
library(randomForest)
#levels(test$LockMonth) <- levels(train$LockMonth) # to match levels with training data and test data
train$LockMonth <- TrainLockMonth
test$LockMonth <- TestLockMonth
train_sample$LockMonth <- TestLockMonth
rf.model<-randomForest(Locks ~  LeSent + Leads + Calls + LongCalls + LockMonth + NewLoans + IntentToProceed + ApprOrdered, data=train,ntree=600,mtry=4) 
#summary(rf.model)
#train_sample <- test

predict.rf.lockrates <- predict(rf.model,test) 
#tmp <- train_sample$Locks
#train_sample$Locks <- NULL
predict_sample.rf.lockrates <- predict(rf.model,train_sample) 
#train_sample$Locks <- tmp
test.output.rf <- cbind(test,predict.rf.lockrates)
train.sample.output.rf <- cbind(train_sample,predict_sample.rf.lockrates)

#root mean square - 13.42314
rmse.rf <- rmse(train.sample.output.rf$Locks , train.sample.output.rf$predict_sample.rf.lockrates) 

test.output.rf$Locks <- test.output.rf$predict.rf.lockrates
test.output.rf$LockPred <- round(test.output.rf$Locks,0)
test.output.rf$Locks <- NULL
test.output.rf$LockMonth <- NULL
test.output.rf$LockYear <- NULL
test.output.rf$predict.rf.lockrates <- NULL

write.csv(test.output.rf, paste(dir.Name,"\\PredictOutput.rf.csv",sep=''),row.names = FALSE)
return (list(rmse.rf = rmse.rf))
}
################################PRediction using GBM##########################################################################
PredictUsingGBM <- function()
{
#Leads + Calls + LongCalls + LockDay + NewLoans + IntentToProceed + ApprOrdered
library(gbm)
#cannot contain variables that not included in the model , so we delete the unused columns
train$LockDay <- NULL
train$DayOWeek <- NULL
train$LockYear <- NULL

gbm.model <- gbm(Locks ~ LeSent + Leads + Calls + LongCalls + LockMonth + NewLoans + IntentToProceed + ApprOrdered,         # formula rmse -6.160873
                 data=train,                   # dataset
                 distribution="gaussian",     # bernoulli, adaboost, gaussian,
                 # poisson, coxph, and quantile available
                 n.trees=3000,                # number of trees
                 shrinkage=0.005,             # shrinkage or learning rate,
                 # 0.001 to 0.1 usually work
                 interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
                 bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
                 train.fraction = 0.5,        # fraction of data for training,
                 # first train.fraction*N used for training
                 n.minobsinnode = 10,         # minimum total weight needed in each node
                 cv.folds = 5,                # do 5-fold cross-validation
                 keep.data=TRUE,              # keep a copy of the dataset with the object
                 verbose=TRUE)                # print out progress

# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm.model,method="OOB")
#print(best.iter)

# check performance using a 50% heldout test set
best.iter <- gbm.perf(gbm.model,method="test")
#print(best.iter)

# check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm.model,method="cv")
#print(best.iter)

# plot the performance
# plot variable influence
#summary(gbm.model,n.trees=1)         # based on the first tree
#summary(gbm.model,n.trees=best.iter) # based on the estimated best number of trees
#pred_out  <- predict.gbm(gbm.model,test,best.iter)

predict.gbm.lockrates <- predict.gbm(gbm.model,test,best.iter) 
predict_sample.gbm.lockrates <- predict.gbm(gbm.model,train_sample,best.iter) 

test.output.gbm <- cbind(test,predict.gbm.lockrates)
train.sample.output.gbm <- cbind(train_sample,predict_sample.gbm.lockrates)

#root mean square -  16.89591
rmse.gbm <-  rmse(train.sample.output.gbm$Locks , train.sample.output.gbm$predict_sample.gbm.lockrates) 

test.output.gbm$Locks <- test.output.gbm$predict.gbm.lockrates
test.output.gbm$LockPred <- round(test.output.gbm$Locks,0)
test.output.gbm$Locks <-  NULL
test.output.gbm$LockMonth <- NULL
test.output.gbm$LockYear <- NULL
test.output.gbm$predict.gbm.lockrates <- NULL

write.csv(test.output.gbm, paste(dir.Name,"\\PredictOutput.gbm.csv",sep=''),row.names = FALSE)
return (list(rmse.gbm = rmse.gbm))
}
#############################################################################################################
################################PRediction using XGBoost##########################################################################
PredictUsingXgBoost <- function()
{
  #Leads + Calls + LongCalls + LockDay + NewLoans + IntentToProceed + ApprOrdered
  library(caret)
  #cannot contain variables that not included in the model , so we delete the unused columns
  train$LockDay <- NULL
  train$DayOWeek <- NULL
  train$LockYear <- NULL
  
  test$LockDay <- NULL
  test$DayOWeek <- NULL
  test$LockYear <- NULL
  test$LockPred <- NULL
  
  train_sample$LockDay <- NULL
  train_sample$DayOWeek <- NULL
  train_sample$LockYear <- NULL
  train_sample$LockPred <- NULL
  
  head(test)
  xgbGrid1 <- expand.grid(
    nrounds = c(250, 500, 1000),
    max_depth = c(1, 2, 4),
    eta = c(0.001, 0.003, 0.01),
    gamma = c(0, 1, 2),
    colsample_bytree = c(1, 0.5, 0.25),
    min_child_weight = c(1, 2)
  )
  
  xgbTrControl1 <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 2,
    verboseIter = FALSE,
    returnData = FALSE,
    allowParallel = TRUE
  )
  train$LockMonth <- as.numeric(train$LockMonth) ## to have a numeric matrix
  test$LockMonth <- as.numeric(test$LockMonth)
  train_sample$LockMonth <- as.numeric(train_sample$LockMonth)
 # hh <-  as.matrix(train[, ! names(train) %in% c("Locks")])
#   head(hh)
  xgbTrain1 <- train(
    x = as.matrix(train[, ! names(train) %in% c("Locks")]), 
    y = train$Locks,
    objective = "reg:linear",
    trControl = xgbTrControl1,
    tuneGrid = xgbGrid1,
    method = "xgbTree"
  )
  
  head(train)
  head(test)
  predict.xgBoost.lockrates <- predict(xgbTrain1, newdata = test) 
  predict_sample.xgBoost.lockrates <- predict(xgbTrain1,newdata = train_sample) 
  
  test.output.xgBoost <- cbind(test,predict.xgBoost.lockrates)
  train.sample.output.xgBoost <- cbind(train_sample,predict_sample.xgBoost.lockrates)
  
  #root mean square -  25.91338
  rmse.xgBosst <-  rmse(train.sample.output.xgBoost$Locks , train.sample.output.xgBoost$predict_sample.xgBoost.lockrates) 
  rmse.xgBosst
  test.output.xgBoost$Locks <- test.output.xgBoost$predict.xgBoost.lockrates
  test.output.xgBoost$LockPred <- round(test.output.xgBoost$Locks,0)
  test.output.xgBoost$Locks <-  NULL
  test.output.xgBoost$LockMonth <- NULL
  test.output.xgBoost$LockYear <- NULL
  test.output.xgBoost$predict.xgBoost.lockrates <- NULL
  
  write.csv(test.output.xgBoost, paste(dir.Name,"\\PredictOutput.gbm.csv",sep=''),row.names = FALSE)
  return (list(rmse.xgBosst = rmse.xgBosst))
}
#############################################################################################################
###################Prediction using Support Vector Machine#################################
#since test sample doesnt have different range factors for lockday , svm not able to process
#if we have more sample data, then we can predict
# library(e1071)
# 
# svm.model <- svm(Locks ~ Leads + Calls + LongCalls + LockMonth + NewLoans + IntentToProceed , data = train )
# 
# summary(svm.model)
# 
# svm_tuneResult <- tune(svm, Locks ~ Leads + Calls + LongCalls + LockMonth + NewLoans + IntentToProceed ,  data = train,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
# print(svm_tuneResult)
# plot(svm_tuneResult)
# 
# svm_tuneResult <- tune(svm, Locks ~ Leads + Calls + LongCalls + LockMonth + NewLoans + IntentToProceed  ,  data = train,ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))) 
# svm.model <- svm_tuneResult$best.model
# 
# #predict.svm.lockrates <- predict(svm.model,test) 
# predict_sample.svm.lockrates <- predict(svm.model,train_sample) 
# 
# #test.output.svm <- cbind(test,predict.svm.lockrates)
# train.sample.output.svm <- cbind(train_sample,predict_sample.svm.lockrates)
# 
# rmse(train.sample.output.svm$Locks , train.sample.output.svm$predict_sample.svm.lockrates) # 7.854472

#test.output.svm$Locks <- test.output.svm$predict.svm.lockrates
#test.output.svm$Locks <- round(test.output.svm$Locks,0)
#test.output.svm$LockMonth <- NULL
#test.output.svm$LockYear <- NULL
#test.output.svm$predict.svm.lockrates <- NULL
#write.csv(test.output.svm, 'UW Loan Rate\\PredictOutput.svm.csv',row.names = FALSE)