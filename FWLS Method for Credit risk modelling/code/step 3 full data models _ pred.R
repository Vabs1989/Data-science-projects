### Full data train alternate model and predict on Train, Validation, Test #########

## Logistic Regression Model
Logistic.train=read.csv("D:/Vaibhav/FWLS Method/Input/01_DPDTraining_SoftNo_Hit.csv")
Logistic.validate=read.csv("D:/Vaibhav/FWLS Method/Input/01_DPDValidation_SoftNo_Hit.csv")
Logistic.test=read.csv("D:/Vaibhav/FWLS Method/Input/01_DPDTesting_SoftNo_Hit.csv")

Logistic.train$Y=as.factor(Logistic.train$X60DPD)

trainX_Y_logi=data.frame(Logistic.train[,c(3:44,54,57)])
validX_Y_logi=data.frame(Logistic.validate[,c(3:44,54,56)])
testX_Y_logi=data.frame(Logistic.test[,c(3:44,54,56)])
library(DMwR)
library(ROCR)
set.seed(123)
smoted_data_logi=SMOTE(Y~.,trainX_Y_logi,perc.over = 200,perc.under = 150,learner=NULL)

library(gmodels)
a=CrossTable(Logistic.train$Y)
OF=a$prop.row[1,2]
b=CrossTable(smoted_data_logi$Y)
OSF=b$prop.row[1,2]


Model_logi=glm(Y~.,family=binomial(link=logit),data=smoted_data_logi)
summary(Model_logi)
# 
# #Training Data
# trainX_logi=data.frame(Logistic.train[,c(3:44,54)])
# 
# train_pred_logi=data.frame(predict(Model_logi,trainX_logi,se.fit=TRUE))
# train_Pred2_logi=data.frame(train_pred_logi[,1])
# train_Fit_logi=exp(train_Pred2_logi)/(1+exp(train_Pred2_logi))
# colnames(train_Fit_logi)="train_Fit_logi"
# corrected_train_pred_logi=1/(1+(1/OF-1)/(1/OSF-1)*(1/train_Fit_logi-1))
# 
# summary(corrected_train_pred_logi$train_Fit_logi)
# library(ROSE)
# roc.curve(Logistic.train$Y, corrected_train_pred_logi$train_Fit_logi)
# 
# library(StatMeasures)
# cm_train=accuracy(Logistic.train$Y, corrected_train_pred_logi$train_Fit_logi, cutoff=OF)
# cm_train$accuracyNum
# cm_train$overallAcc
# 
# cm_train$accuracyNum[4]
# 
# 
# acc=(cm_train$accuracyNum[4]+cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[2]+cm_train$accuracyNum[3]+cm_train$accuracyNum[4])
# acc
# TPR=(cm_train$accuracyNum[4])/(cm_train$accuracyNum[4]+cm_train$accuracyNum[2])
# TPR
# TNR=(cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[3])
# TNR
# 
# #Validation
# ValX_logi=data.frame(Logistic.validate[,c(3:44,54)])
# 
# Val_pred_logi=data.frame(predict(Model_logi,ValX_logi,se.fit=TRUE))
# Val_pred2_logi=data.frame(Val_pred_logi[,1])
# Val_Fit_logi=exp(Val_pred2_logi)/(1+exp(Val_pred2_logi))
# colnames(Val_Fit_logi)="Val_Fit_logi"
# corrected_Val_pred_logi=1/(1+(1/OF-1)/(1/OSF-1)*(1/Val_Fit_logi-1))
# 
# summary(corrected_Val_pred_logi$Val_Fit_logi)
# roc.curve(Logistic.validate$X60DPD, corrected_Val_pred_logi$Val_Fit_logi)
# 
# cm_valid=accuracy(Logistic.validate$X60DPD, corrected_Val_pred_logi$Val_Fit_logi, cutoff=OF)
# cm_valid$accuracyNum
# cm_valid$overallAcc
# 
# cm_valid$accuracyNum[4]
# 
# 
# acc=(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[2]+cm_valid$accuracyNum[3]+cm_valid$accuracyNum[4])
# acc
# TPR=(cm_valid$accuracyNum[4])/(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[2])
# TPR
# TNR=(cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[3])
# TNR
# 
# 
# #Testing Data
# testX_logi_full=data.frame(Logistic.test[,c(3:44,54)])
# 
# test_pred_logi_full=data.frame(predict(Model_logi,testX_logi_full,se.fit=TRUE))
# test_Pred2_logi_full=data.frame(test_pred_logi_full[,1])
# test_Fit_logi_full=exp(test_Pred2_logi_full)/(1+exp(test_Pred2_logi_full))
# colnames(test_Fit_logi_full)="test_Fit_logi_full"
# corrected_test_pred_logi=1/(1+(1/OF-1)/(1/OSF-1)*(1/test_Fit_logi_full-1))
# 
# summary(corrected_test_pred_logi$test_Fit_logi_full)
# roc.curve(Logistic.test$X60DPD, corrected_test_pred_logi$test_Fit_logi_full)
# 
# cm_test=accuracy(Logistic.test$X60DPD, corrected_test_pred_logi$test_Fit_logi_full, cutoff=OF)
# cm_test$accuracyNum
# cm_test$overallAcc
# 
# cm_test$accuracyNum[4]
# 
# acc=(cm_test$accuracyNum[4]+cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[2]+cm_test$accuracyNum[3]+cm_test$accuracyNum[4])
# acc
# TPR=(cm_test$accuracyNum[4])/(cm_test$accuracyNum[4]+cm_test$accuracyNum[2])
# TPR
# TNR=(cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[3])
# TNR


## SVM Model
# SVM.train=read.csv("D:/Vaibhav/FWLS Method/Input/01_DPDTraining_SoftNo_Hit.csv")
# SVM.validate=read.csv("D:/Vaibhav/FWLS Method/Input/01_DPDValidation_SoftNo_Hit.csv")
# SVM.test=read.csv("D:/Vaibhav/FWLS Method/Input/01_DPDTesting_SoftNo_Hit.csv")
# 
# SVM.train$Y=as.factor(SVM.train$X60DPD)
# SVM.validate$Y=as.factor(SVM.validate$X60DPD)
# SVM.test$Y=as.factor(SVM.test$X60DPD)

# trainX_Y_svm=data.frame(SVM.train[,c(3:44,54,57)])
# library(DMwR)
# library(ROCR)
# set.seed(123)
# smoted_data_svm=SMOTE(Y~.,trainX_Y_svm,perc.over = 200,perc.under = 150,learner=NULL)
# library(gmodels)
# a=CrossTable(SVM.train$Y)
# OF=a$prop.row[1,2]
# b=CrossTable(smoted_data_svm$Y)
# OSF=b$prop.row[1,2]

# 1) dpath = user working directory
# 2) Train_Filename= Training File Name with csv extention
# 3) Validation_Filename= Validation File Name with csv extention
# 4) Test_Filename= Test File Name with csv extention
# 5) Xcol= The column numbers to be used as X variables
# 5) nfolds = number of resampling iterations.
# 6) repeats= For repeated k-fold cross-validation only: the number of complete sets of folds to compute.
# 7) method= the type of svm to be used.Options can be svmLinear, svmRadial.
# 8) binnum= whether the classification is binary or not. Answer should be "Y" or "N"
# 9) deg=degree to be used in case of polynomial. Leave it at 0 for any other kernel type.
#10) sigma = defines how far the influence of a single training example reaches.
#11) Cost=tells the SVM optimization how much of misclassification is to be avoided.
#12) grid= sigma and cost are to be specified here. In case of 'svmLinear', specify only 'C'
#13) Output_Filename= Output File Name with xlsx extension


library(e1071)
set.seed(123)
best_model_svmR<-svm(Y~.,data=smoted_data_logi,kernel='radial',cost=0.75,gamma=0.015,probability=TRUE)


#Training data prediction
trainX=data.frame(trainX_Y_logi[,-44])
train_pred_svm=predict(best_model_svmR,trainX,probability=TRUE)
prob_train=attr(train_pred_svm,"probabilities")
prob_train=data.frame(prob_train)
corrected_train_pred_svm=1/(1+(1/OF-1)/(1/OSF-1)*(1/prob_train[,2]-1))
corrected_train_pred_svmR=data.frame(corrected_train_pred_svm)

summary(corrected_train_pred_svmR$corrected_train_pred_svm)
roc.curve(Logistic.train$Y, corrected_train_pred_svmR$corrected_train_pred_svm)

library(StatMeasures)
cm_train=accuracy(Logistic.train$Y, corrected_train_pred_svmR$corrected_train_pred_svm, cutoff=OF)
cm_train$accuracyNum
cm_train$overallAcc

cm_train$accuracyNum[4]


acc=(cm_train$accuracyNum[4]+cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[2]+cm_train$accuracyNum[3]+cm_train$accuracyNum[4])
acc
TPR=(cm_train$accuracyNum[4])/(cm_train$accuracyNum[4]+cm_train$accuracyNum[2])
TPR
TNR=(cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[3])
TNR

#Validation data prediction
validX_svm=data.frame(validX_Y_logi[,-44])
valid_pred_svm=predict(best_model_svmR,validX_svm,probability=TRUE)
prob_valid=attr(valid_pred_svm,"probabilities")
prob_valid=data.frame(prob_valid)
corrected_valid_pred_svm=1/(1+(1/OF-1)/(1/OSF-1)*(1/prob_valid[,2]-1))
corrected_valid_pred_svmR=data.frame(corrected_valid_pred_svm)

summary(corrected_valid_pred_svmR$corrected_valid_pred_svm)
roc.curve(Logistic.validate$X60DPD, corrected_valid_pred_svmR$corrected_valid_pred_svm)

cm_valid=accuracy(Logistic.validate$X60DPD, corrected_valid_pred_svmR$corrected_valid_pred_svm, cutoff=OF)
cm_valid$accuracyNum
cm_valid$overallAcc

cm_valid$accuracyNum[4]


acc=(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[2]+cm_valid$accuracyNum[3]+cm_valid$accuracyNum[4])
acc
TPR=(cm_valid$accuracyNum[4])/(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[2])
TPR
TNR=(cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[3])
TNR

#Testing data prediction
testX_svm=data.frame(testX_Y_logi[,-44])
test_pred_svm_full=predict(best_model_svmR,testX_svm,probability=TRUE)
prob_test_full=attr(test_pred_svm_full,"probabilities")
prob_test_full=data.frame(prob_test_full)
corrected_test_pred_svm=1/(1+(1/OF-1)/(1/OSF-1)*(1/prob_test_full[,2]-1))
corrected_test_pred_svmR=data.frame(corrected_test_pred_svm)

summary(corrected_test_pred_svmR$corrected_test_pred_svm)
roc.curve(Logistic.test$X60DPD, corrected_test_pred_svmR$corrected_test_pred_svm)

cm_test=accuracy(Logistic.test$X60DPD, corrected_test_pred_svmR$corrected_test_pred_svm, cutoff=OF)
cm_test$accuracyNum
cm_test$overallAcc

cm_test$accuracyNum[4]

acc=(cm_test$accuracyNum[4]+cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[2]+cm_test$accuracyNum[3]+cm_test$accuracyNum[4])
acc
TPR=(cm_test$accuracyNum[4])/(cm_test$accuracyNum[4]+cm_test$accuracyNum[2])
TPR
TNR=(cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[3])
TNR

##### NN model ###################

NN.train=data.frame(Logistic.train)
NN.validate=data.frame(Logistic.validate)
NN.test=data.frame(Logistic.test)


# # Create Vector of Column Max and Min Values
# maxs_train <- apply(NN.train[,c(3:44,54)], 2, max)
# mins_train <- apply(NN.train[,c(3:44,54)], 2, min)
# 
# maxs_validate <- apply(NN.validate[,c(3:44,54)], 2, max)
# mins_validate <- apply(NN.validate[,c(3:44,54)], 2, min)
# 
# maxs_test <- apply(NN.test[,c(3:44,54)], 2, max)
# mins_test <- apply(NN.test[,c(3:44,54)], 2, min)
# 
# # Use scale() and convert the resulting matrix to a data frame
# scaled.data_train <- as.data.frame(scale(NN.train[,c(3:44,54)],center = mins_train, scale = maxs_train - mins_train))
# scaled.data_validate <- as.data.frame(scale(NN.validate[,c(3:44,54)],center = mins_validate, scale = maxs_validate - mins_validate))
# scaled.data_test <- as.data.frame(scale(NN.test[,c(3:44,54)],center = mins_test, scale = maxs_test - mins_test))

scaled.data_train <- as.data.frame(scale(NN.train[,c(3:44,54)],center = TRUE, scale = TRUE))
scaled.data_validate <- as.data.frame(scale(NN.validate[,c(3:44,54)],center = TRUE, scale = TRUE))
scaled.data_test <- as.data.frame(scale(NN.test[,c(3:44,54)],center = TRUE, scale = TRUE))


scaled.data_train = cbind(scaled.data_train,NN.train$Y)
colnames(scaled.data_train)[ncol(scaled.data_train)]<-"Y"

scaled.data_validate = cbind(scaled.data_validate,NN.validate$X60DPD)
colnames(scaled.data_validate)[ncol(scaled.data_validate)]<-"Y"

scaled.data_test = cbind(scaled.data_test,NN.test$X60DPD)
colnames(scaled.data_test)[ncol(scaled.data_test)]<-"Y"

library(gmodels)
a=CrossTable(scaled.data_train$Y)
OF=a$prop.row[1,2]
library(DMwR)
library(ROCR)
set.seed(123)
smoted_data_NN=SMOTE(Y~.,scaled.data_train,perc.over = 200,perc.under = 150,learner=NULL)
smoted_data_NN$Y=as.numeric(as.character(smoted_data_NN$Y))
b=CrossTable(smoted_data_NN$Y)
OSF=b$prop.row[1,2]

feats <- names(smoted_data_NN[,c(2,3,4,6,7,8,9,18,21,23,24,26,27,29,30,34,36,37,38,39,41,43)])
#feats=names(smoted_data_NN[,c(1:43)])
feats
# # Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste("Y ~",f)

# Convert to formula
f <- as.formula(f)
f
#install.packages("neuralnet")
library(neuralnet)

# Number of hidden nodes:
# There is no magic formula for selecting the optimum number of hidden neurons.
# However, some thumb rules are available for calculating the number of hidden neurons.
# A rough approximation can be obtained by the geometric pyramid rule proposed by Masters (1993).
# For a three layer network with n input and m output neurons, the hidden layer would have sqrt(n*m)neurons.

#sqrt(43*1)

set.seed(123)
nn <-neuralnet( formula = f,
                data = smoted_data_NN,
                hidden=2,
                threshold = 0.01,
                stepmax = 1e+06,
                lifesign = "full",
                algorithm = "rprop+",
                err.fct="ce",
                act.fct="logistic",
                linear.output=FALSE )
#plot(nn)

#  Compute Predictions train Set
pred_nn_train <-neuralnet::compute(nn,scaled.data_train[,feats])

pred_nn_train=data.frame(pred_nn_train)
corrected_train_pred_NN=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_nn_train[,ncol(pred_nn_train)]-1))
corrected_train_pred_NN=data.frame(corrected_train_pred_NN)

summary(corrected_train_pred_NN)
library(ROSE)
roc.curve(scaled.data_train$Y,corrected_train_pred_NN$corrected_train_pred_NN)

library(StatMeasures)
cm_train=accuracy(Logistic.train$Y, corrected_train_pred_NN$corrected_train_pred_NN, cutoff=OF)
cm_train$accuracyNum
cm_train$overallAcc

cm_train$accuracyNum[4]


acc=(cm_train$accuracyNum[4]+cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[2]+cm_train$accuracyNum[3]+cm_train$accuracyNum[4])
acc
TPR=(cm_train$accuracyNum[4])/(cm_train$accuracyNum[4]+cm_train$accuracyNum[2])
TPR
TNR=(cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[3])
TNR

#  Compute Predictions validate Set
pred_nn_valid <-neuralnet::compute(nn,scaled.data_validate[,feats])

pred_nn_valid=data.frame(pred_nn_valid)
corrected_valid_pred_NN=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_nn_valid[,ncol(pred_nn_valid)]-1))
corrected_valid_pred_NN=data.frame(corrected_valid_pred_NN)

summary(corrected_valid_pred_NN)
#library(ROSE)
roc.curve(scaled.data_validate$Y,corrected_valid_pred_NN$corrected_valid_pred_NN)

cm_valid=accuracy(Logistic.validate$X60DPD, corrected_valid_pred_NN$corrected_valid_pred_NN, cutoff=OF)
cm_valid$accuracyNum
cm_valid$overallAcc

cm_valid$accuracyNum[4]


acc=(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[2]+cm_valid$accuracyNum[3]+cm_valid$accuracyNum[4])
acc
TPR=(cm_valid$accuracyNum[4])/(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[2])
TPR
TNR=(cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[3])
TNR

#  Compute Predictions test Set
pred_nn_test <-neuralnet::compute(nn,scaled.data_test[,feats])

pred_nn_test=data.frame(pred_nn_test)
corrected_test_pred_NN=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_nn_test[,ncol(pred_nn_test)]-1))
corrected_test_pred_NN=data.frame(corrected_test_pred_NN)
#NN.test=cbind(NN.test,corrected_test_pred_NN)
summary(corrected_test_pred_NN)
#library(ROSE)
roc.curve(scaled.data_test$Y,corrected_test_pred_NN$corrected_test_pred_NN)

cm_test=accuracy(Logistic.test$X60DPD, corrected_test_pred_NN$corrected_test_pred_NN, cutoff=OF)
cm_test$accuracyNum
cm_test$overallAcc

cm_test$accuracyNum[4]

acc=(cm_test$accuracyNum[4]+cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[2]+cm_test$accuracyNum[3]+cm_test$accuracyNum[4])
acc
TPR=(cm_test$accuracyNum[4])/(cm_test$accuracyNum[4]+cm_test$accuracyNum[2])
TPR
TNR=(cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[3])
TNR


##XGBOOST Model ##############

smoted_data_XGB=smoted_data_logi
smoted_data_XGB$Y=as.numeric(as.character(smoted_data_XGB$Y ))


trainX_Y_XGB=trainX_Y_logi
trainX_Y_XGB$Y=as.numeric(as.character(trainX_Y_XGB$Y ))
validX_Y_XGB=validX_Y_logi
testX_Y_XGB=testX_Y_logi


library(xgboost)

train_matrix_smote=xgb.DMatrix(data=as.matrix(smoted_data_XGB[,-44]),label=smoted_data_XGB[,"Y"])
train_matrix=xgb.DMatrix(data=as.matrix(trainX_Y_XGB[,-44]),label=trainX_Y_XGB[,"Y"])
valid_matrix=xgb.DMatrix(data=as.matrix(validX_Y_XGB[,-44]),label=validX_Y_XGB[,"X60DPD"])
test_matrix=xgb.DMatrix(data=as.matrix(testX_Y_XGB[,-44]),label=testX_Y_XGB[,"X60DPD"])


# train a model using our training data
 
#parameters

xgb_params=list("objective" = "binary:logistic",
                "booster" = "gbtree",
                "min_child_weight"=1)

#To know the best nrounds
# set.seed(123)
# xgbcv <- xgb.cv(params = xgb_params,
#                  data = train_matrix_smote,
#                  nrounds = 500,
#                  nfold = 5,
#                  showsd = T,
#                  print_every_n = 10,
#                  metrics = "auc",
#                  early_stopping_rounds = 20)


watchlist=list(train_smote=train_matrix_smote,train=train_matrix,validate=valid_matrix,test=test_matrix)

set.seed(123)
model_xgb=xgb.train(params = xgb_params,
                    data =train_matrix_smote,
                    watchlist=watchlist,
                    nrounds = 309,
                    eta=0.01,
                    gamma = 0.3,
                    max.depth = 3)

# set.seed(123)
# model_xgb <- xgboost(data = train_matrix_smote, # the data
#                      params = list("eta"=0.01,"max_depth"=3),
#                      nround = 200, # max number of boosting iterations
#                      objective = "binary:logistic")  # the objective function


#Train, Validate & Test error plot
e=data.frame(model_xgb$evaluation_log)

#plot(e$iter,e$train_smote_error,col="blue")
plot(e$iter,e$train_error,col="blue")
lines(e$iter,e$validate_error,col="red")
lines(e$iter,e$test_error,col="orange")


# generate predictions for our held-out training data
pred_train <- predict(model_xgb, train_matrix)
#n_tr <- length(pred)
#pred_train=pred[seq(2, n_tr, 2)]

corrected_train_pred_xgb=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_train-1))
mean(corrected_train_pred_xgb)

# get & print the classification error
err <- mean(as.numeric(corrected_train_pred_xgb > 0.06) != trainX_Y_XGB[,"Y"])
print(paste("train-error=", err))
summary(corrected_train_pred_xgb)
library(ROSE)
roc.curve(trainX_Y_XGB$Y,corrected_train_pred_xgb)

cm_train=accuracy(Logistic.train$Y, corrected_train_pred_xgb, cutoff=OF)
cm_train$accuracyNum
cm_train$overallAcc

cm_train$accuracyNum[4]


acc=(cm_train$accuracyNum[4]+cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[2]+cm_train$accuracyNum[3]+cm_train$accuracyNum[4])
acc
TPR=(cm_train$accuracyNum[4])/(cm_train$accuracyNum[4]+cm_train$accuracyNum[2])
TPR
TNR=(cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[3])
TNR

# generate predictions for our held-out validating data
pred_valid <- predict(model_xgb, valid_matrix)
# n_vl <- length(pred_valid)
# pred_valid=pred[seq(2, n_vl, 2)]

corrected_valid_pred_xgb=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_valid-1))
mean(corrected_valid_pred_xgb)

# get & print the classification error
err_valid <- mean(as.numeric(corrected_valid_pred_xgb > 0.06) != validX_Y_XGB[,"X60DPD"])
print(paste("valid-error=", err_valid))
summary(corrected_valid_pred_xgb)

roc.curve(validX_Y_XGB$X60DPD ,corrected_valid_pred_xgb)

cm_valid=accuracy(Logistic.validate$X60DPD, corrected_valid_pred_xgb, cutoff=OF)
cm_valid$accuracyNum
cm_valid$overallAcc

cm_valid$accuracyNum[4]

acc=(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[2]+cm_valid$accuracyNum[3]+cm_valid$accuracyNum[4])
acc
TPR=(cm_valid$accuracyNum[4])/(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[2])
TPR
TNR=(cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[3])
TNR

# generate predictions for our held-out testing data
pred_test <- predict(model_xgb, test_matrix)

corrected_test_pred_xgb=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_test-1))
mean(corrected_test_pred_xgb)

# get & print the classification error
err_test <- mean(as.numeric(corrected_test_pred_xgb > 0.06) != testX_Y_XGB[,"X60DPD"])
print(paste("test-error=", err_test))
summary(corrected_test_pred_xgb)

roc.curve(testX_Y_XGB$X60DPD ,corrected_test_pred_xgb)

cm_test=accuracy(Logistic.test$X60DPD, corrected_test_pred_xgb, cutoff=OF)
cm_test$accuracyNum
cm_test$overallAcc

cm_test$accuracyNum[4]

acc=(cm_test$accuracyNum[4]+cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[2]+cm_test$accuracyNum[3]+cm_test$accuracyNum[4])
acc
TPR=(cm_test$accuracyNum[4])/(cm_test$accuracyNum[4]+cm_test$accuracyNum[2])
TPR
TNR=(cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[3])
TNR

