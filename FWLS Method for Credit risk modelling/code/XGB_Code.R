# XGBOOST Model on logi_df

XGB.train=read.csv("D:/Vaibhav/Data/creatd_data_logi/Training_Logistic.csv")
XGB.validate=read.csv("D:/Vaibhav/Data/creatd_data_logi/Validation_Logistic.csv")
XGB.test=read.csv("D:/Vaibhav/Data/creatd_data_logi/Testing_Logistic.csv")

XGB.train_smote=XGB.train
XGB.train_smote$Y=as.factor(XGB.train_smote$Y)


# XGB.train$Y=XGB.train$X60DPD
# XGB.validate$Y=XGB.validate$X60DPD
# XGB.test$Y=XGB.test$X60DPD

trainx_Y_XGB_smote=data.frame(XGB.train_smote[,c(2:8)])
trainX_Y_XGB=data.frame(XGB.train[,c(2:8)])
validX_Y_XGB=data.frame(XGB.validate[,c(2:8)])
testX_Y_XGB=data.frame(XGB.test[,c(2:8)])
library(DMwR)
library(ROCR)
set.seed(123)
smoted_data_XGB=SMOTE(Y~.,trainx_Y_XGB_smote,perc.over = 200,perc.under = 150,learner=NULL)
smoted_data_XGB$Y=as.numeric(as.character(smoted_data_XGB$Y))
str(smoted_data_XGB)

library(gmodels)
a=CrossTable(XGB.train$Y)
OF=a$prop.row[1,2]
b=CrossTable(smoted_data_XGB$Y)
OSF=b$prop.row[1,2]

#install.packages("xgboost")
library(xgboost)

train_matrix_smote=xgb.DMatrix(data=as.matrix(smoted_data_XGB[,-7]),label=smoted_data_XGB[,"Y"])
train_matrix=xgb.DMatrix(data=as.matrix(trainX_Y_XGB[,-7]),label=trainX_Y_XGB[,"Y"])
valid_matrix=xgb.DMatrix(data=as.matrix(validX_Y_XGB[-7]),label=validX_Y_XGB[,"Y"])
test_matrix=xgb.DMatrix(data=as.matrix(testX_Y_XGB[-7]),label=testX_Y_XGB[,"Y"])


# train a model using our training data
model_xgb <- xgboost(data = train_matrix_smote, # the data
                     params = list("eta"=0.1,"max_depth"=3),
                     nround = 100, # max number of boosting iterations
                     objective = "binary:logistic")  # the objective function


# generate predictions for our held-out training data
pred <- predict(model_xgb, train_matrix)
mean(pred)
View(pred)
corrected_train_pred_xgb=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred-1))
mean(corrected_train_pred_xgb)
# get & print the classification error
err <- mean(as.numeric(corrected_train_pred_xgb > 0.06) != trainX_Y_XGB[,"Y"])
print(paste("train-error=", err))
summary(corrected_train_pred_xgb)
library(ROSE)
roc.curve(XGB.train$Y,corrected_train_pred_xgb)


library(StatMeasures)
cm_train=accuracy(XGB.train$Y, corrected_train_pred_xgb, cutoff=0.06)
cm_train$accuracyNum
cm_train$overallAcc
acc=(2622+36684)/(2622+36684+21737+1113)
acc
TPR=(2622)/(2622+1113)
TPR
TNR=(36684)/(36684+21737)
TNR

# generate predictions for our held-out validating data
pred_valid <- predict(model_xgb, valid_matrix)
mean(pred_valid)
View(pred_valid)
corrected_valid_pred_xgb=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_valid-1))
mean(corrected_valid_pred_xgb)
# get & print the classification error
err_valid <- mean(as.numeric(corrected_valid_pred_xgb > 0.06) != validX_Y_XGB[,"Y"])
print(paste("valid-error=", err_valid))
summary(corrected_valid_pred_xgb)

roc.curve(XGB.validate$Y,corrected_valid_pred_xgb)

library(StatMeasures)
cm_valid=accuracy(XGB.validate$Y, corrected_valid_pred_xgb, cutoff=0.06)
cm_valid$accuracyNum
cm_valid$overallAcc
acc=(882+12183)/(882+12183+430+7199)
acc
TPR=(882)/(882+430)
TPR
TNR=(12183)/(12183+7199)
TNR


# generate predictions for our held-out testing data
pred_test <- predict(model_xgb, test_matrix)
mean(pred_test)
View(pred_test)
corrected_test_pred_xgb=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_test-1))
mean(corrected_test_pred_xgb)
# get & print the classification error
err_test <- mean(as.numeric(corrected_test_pred_xgb > 0.06) != testX_Y_XGB[,"Y"])
print(paste("test-error=", err_test))
summary(corrected_test_pred_xgb)

roc.curve(XGB.test$Y,corrected_test_pred_xgb)

library(StatMeasures)
cm_test=accuracy(XGB.test$Y, corrected_test_pred_xgb, cutoff=0.06)
cm_test$accuracyNum
cm_test$overallAcc
acc=(869+12158)/(869+12158+427+7167)
acc
TPR=(869)/(869+427)
TPR
TNR=(12158)/(12158+7167)
TNR

Tr_logidf_corr_xgb=data.frame(XGB.train,corrected_train_pred_xgb)
Vl_logidf_corr_xgb=data.frame(XGB.validate,corrected_valid_pred_xgb)
Ts_logidf_corr_xgb=data.frame(XGB.test,corrected_test_pred_xgb)

write.csv(Tr_logidf_corr_xgb,"D:/Vaibhav/Data/creatd_data_logi/Tr_logidf_corr_xgb.csv")
write.csv(Vl_logidf_corr_xgb,"D:/Vaibhav/Data/creatd_data_logi/Vl_logidf_corr_xgb.csv")
write.csv(Ts_logidf_corr_xgb,"D:/Vaibhav/Data/creatd_data_logi/Ts_logidf_corr_xgb.csv")


#parameters
#nc=length(unique(smoted_data_XGB[,"Y"]))

# xgb_params=list("objective" = "multi:softprob",
#                  "booster" = "gbtree",
#                  "num_class"=nc,
#                  "eval_metric"="mlogloss",
#                  "min_child_weight"= 1)

#To know the best nrounds
# xgbcv <- xgb.cv(params = xgb_params,
#                 data = train_matrix_smote,
#                 nrounds = 250,
#                 nfold = 5,
#                 showsd = T,
#                 stratified = T,
#                 print_every_n = 10,
#                 early_stopping_rounds = 20,
#                 maximize = F)

# watchlist=list(train_smote=train_matrix_smote,train=train_matrix,validate=valid_matrix,test=test_matrix)
#
#
# best_model_XGB1=xgb.train(params = xgb_params,
#                           data =train_matrix_smote,
#                           watchlist=watchlist,
#                           nrounds = 222,
#                           eta=0.01,
#                           gamma = 0.3,
#                           max.depth = 6,
#                           seed = 123)

#Train, Validate & Test error plot
# e=data.frame(best_model_XGB1$evaluation_log)
#
# plot(e$iter,e$train_smote_mlogloss,col="blue")
# lines(e$iter,e$train_mlogloss,col="yellow")
# lines(e$iter,e$validate_mlogloss,col="red")
# lines(e$iter,e$test_mlogloss,col="orange")
#
# min(e$validate_mlogloss)
# e[e$validate_mlogloss==  0.582989,]



# Feature importance
#imp <- xgb.importance(colnames(train_matrix), model = best_model_XGB1)
#print(imp)
#xgb.plot.importance(imp)

# Prediction & confusion matrix - train data
# library(magrittr)
# library(dplyr)
# p_train <- predict(best_model_XGB1, newdata = train_matrix)
# p_train=data.frame(p_train)
# corrected_train_pred_xgb=1/(1+(1/OF-1)/(1/OSF-1)*(1/p_train[,1]-1))
# corrected_train_pred2_xgb=data.frame(corrected_train_pred_xgb)
# pred_train_xgb <- matrix(corrected_train_pred_xgb, nrow = nc,
#                      ncol = length(corrected_train_pred_xgb)/nc) %>%
#   t() %>%data.frame() %>% mutate(label = trainX_Y_XGB[,"Y"],
#                                  max_prob = max.col(., "last")-1)
# CrossTable(pred_train_xgb$max_prob)
#pred

# table(Actual = pred_train_xgb$label,Prediction = pred_train_xgb$max_prob)
#
# library(ROSE)
# roc.curve(pred_train_xgb$label,pred_train_xgb$max_prob)



# Prediction & confusion matrix - validate data
#library(magrittr)
#library(dplyr)
# p_valid <- predict(best_model_XGB1, newdata = valid_matrix)
# p_valid=data.frame(p_valid)
# corrected_valid_pred_xgb=1/(1+(1/OF-1)/(1/OSF-1)*(1/p_valid[,1]-1))
#corrected_valid_pred_xgb=data.frame(corrected_valid_pred_xgb)
# pred_valid_xgb <- matrix(corrected_valid_pred_xgb, nrow = nc, ncol = length(corrected_valid_pred_xgb)/nc) %>% t() %>%data.frame() %>% mutate(label = validX_Y_XGB[,"Y"], max_prob = max.col(., "last")-1)
# CrossTable(pred_valid_xgb$max_prob)
#pred
# table(Actual = pred_valid_xgb$label,Prediction = pred_valid_xgb$max_prob)
#
# #library(ROSE)
# roc.curve(pred_valid_xgb$label,pred_valid_xgb$max_prob)


# Prediction & confusion matrix - test data
#library(magrittr)
#library(dplyr)
# p_test <- predict(best_model_XGB1, newdata = test_matrix)
# p_test=data.frame(p_test)
# corrected_test_pred_xgb=1/(1+(1/OF-1)/(1/OSF-1)*(1/p_test[,1]-1))
# #corrected_valid_pred_xgb=data.frame(corrected_valid_pred_xgb)
# pred_test_xgb <- matrix(corrected_test_pred_xgb, nrow = nc, ncol = length(corrected_test_pred_xgb)/nc) %>%
#   t() %>%data.frame() %>% mutate(label = testX_Y_XGB[,"Y"], max_prob = max.col(., "last")-1)
# CrossTable(pred_test_xgb$max_prob)
# #pred
# table(Actual = pred_test_xgb$label,Prediction = pred_test_xgb$max_prob)
#
# #library(ROSE)
# roc.curve(pred_test_xgb$label,pred_test_xgb$max_prob)

