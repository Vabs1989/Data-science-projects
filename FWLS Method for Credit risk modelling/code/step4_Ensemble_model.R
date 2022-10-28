#Ensemble Model using logistics

# meta_data=data.frame(Logistic.train[,c("all_loans_amt_outstanding_absolute","avgpixelvalue","births_in_pvt_hosp_percent",
#                                        "births_in_pvt_hosp_percent_point_change_yoy","lpgpng_percent","private_banks_savings_deposits_share",
#                                        "savings_deposits_changeyoy","toilet_percent","two_wheeler_percent","underweight_births_percent",
#                                        "A_Score")])



normaliz_tr=read.csv("D:/Vaibhav/FWLS Method/Input/normlize_LT_tr.csv")
normaliz_vl=read.csv("D:/Vaibhav/FWLS Method/Input/normlize_LT_vl.csv")
normaliz_ts=read.csv("D:/Vaibhav/FWLS Method/Input/normlize_LT_ts.csv")

meta_data_tr=data.frame(normaliz_tr[,c("all_loans_amt_outstanding_absolute","avgpixelvalue","births_in_pvt_hosp_percent",
                                          "births_in_pvt_hosp_percent_point_change_yoy","lpgpng_percent","private_banks_savings_deposits_share",
                                          "savings_deposits_changeyoy","toilet_percent","two_wheeler_percent","underweight_births_percent",
                                          "A_Score")])

meta_data_vl=data.frame(normaliz_vl[,c("all_loans_amt_outstanding_absolute","avgpixelvalue","births_in_pvt_hosp_percent",
                                          "births_in_pvt_hosp_percent_point_change_yoy","lpgpng_percent","private_banks_savings_deposits_share",
                                          "savings_deposits_changeyoy","toilet_percent","two_wheeler_percent","underweight_births_percent",
                                          "A_Score")])

meta_data_ts=data.frame(normaliz_ts[,c("all_loans_amt_outstanding_absolute","avgpixelvalue","births_in_pvt_hosp_percent",
                                          "births_in_pvt_hosp_percent_point_change_yoy","lpgpng_percent","private_banks_savings_deposits_share",
                                          "savings_deposits_changeyoy","toilet_percent","two_wheeler_percent","underweight_births_percent",
                                          "A_Score")])

pred_svm_logi1=merge(x=tot_pred_logit,y=tot_pred_svm,by="Original_RecordID")


pred_svm_logi_fw=data.frame(pred_svm_logi1$test_Fit_logi,pred_svm_logi1$test_Fit_svm,pred_svm_logi1$Y.x)
val_logi_svm_fw=data.frame(corrected_Val_pred_logi$Val_Fit_logi,corrected_valid_pred_svmR$corrected_valid_pred_svm,Logistic.validate$X60DPD)
test_logi_svm_fw=data.frame(corrected_test_pred_logi$test_Fit_logi_full,corrected_test_pred_svmR$corrected_test_pred_svm,Logistic.test$X60DPD)

colnames(pred_svm_logi_fw)=c("p_logit","p_svm","Y")
colnames(val_logi_svm_fw)=c("p_logit","p_svm","Y")
colnames(test_logi_svm_fw)=c("p_logit","p_svm","Y")

Train_svm_logi_fw=data.frame(pred_svm_logi_fw,meta_data_tr)
Valid_logi_svm_fw=data.frame(val_logi_svm_fw,meta_data_vl)
Test_logi_svm_fw=data.frame(test_logi_svm_fw,meta_data_ts)
#train_ensx=data.frame(pred_svm_logi_nn[,c(1:3)])
Train_svm_logi_fw$Y=as.factor(Train_svm_logi_fw$Y)
library(gmodels)
library(DMwR)
library(ROCR)
a=CrossTable(Train_svm_logi_fw$Y)
OF=a$prop.row[1,2]
set.seed(123)
smoted_pred_svm_logi_fw=SMOTE(Y ~.,Train_svm_logi_fw,perc.over = 200,perc.under = 150,learner=NULL)
b=CrossTable(smoted_pred_svm_logi_fw$Y)
OSF=b$prop.row[1,2]

#Logistic Model
FWLS_Model_logi=glm(Y ~.,family=binomial(link=logit),data=smoted_pred_svm_logi_fw)

#Ensemble_Model_logi=glm(Y~.,family=binomial(link = probit),data=smoted_pred_svm_logi)
summary(FWLS_Model_logi)


#Training
train_pred_FWLS_logi=data.frame(predict(FWLS_Model_logi,Train_svm_logi_fw[,-3],se.fit=TRUE))
train_pred2_FWLS_logi=data.frame(train_pred_FWLS_logi[,1])
train_Fit_FWLS_logi=exp(train_pred2_FWLS_logi)/(1+exp(train_pred2_FWLS_logi))
colnames(train_Fit_FWLS_logi)="train_Fit_FWLS_logi"
corrected_train_Fit_FWLS_logi=1/(1+(1/OF-1)/(1/OSF-1)*(1/train_Fit_FWLS_logi[,1]-1))
corrected_train_Fit_FWLS_logi=data.frame(corrected_train_Fit_FWLS_logi)
summary(corrected_train_Fit_FWLS_logi$corrected_train_Fit_FWLS_logi)

roc.curve(Train_svm_logi_fw$Y,corrected_train_Fit_FWLS_logi$corrected_train_Fit_FWLS_logi)
library(StatMeasures)
cm_train=accuracy(Train_svm_logi_fw$Y, corrected_train_Fit_FWLS_logi$corrected_train_Fit_FWLS_logi, cutoff=OF)
cm_train$accuracyNum
cm_train$overallAcc

cm_train$accuracyNum[4]

acc=(cm_train$accuracyNum[4]+cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[2]+cm_train$accuracyNum[3]+cm_train$accuracyNum[4])
acc
TPR=(cm_train$accuracyNum[4])/(cm_train$accuracyNum[4]+cm_train$accuracyNum[2])
TPR
TNR=(cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[3])
TNR


#Validation
valid_pred_FWLS_logi=data.frame(predict(FWLS_Model_logi,Valid_logi_svm_fw[,-3],se.fit=TRUE))
valid_pred2_FWLS_logi=data.frame(valid_pred_FWLS_logi[,1])
valid_Fit_FWLS_logi=exp(valid_pred2_FWLS_logi)/(1+exp(valid_pred2_FWLS_logi))
colnames(valid_Fit_FWLS_logi)="valid_Fit_FWLS_logi"
corrected_valid_Fit_FWLS_logi=1/(1+(1/OF-1)/(1/OSF-1)*(1/valid_Fit_FWLS_logi[,1]-1))
corrected_valid_Fit_FWLS_logi=data.frame(corrected_valid_Fit_FWLS_logi)
summary(corrected_valid_Fit_FWLS_logi$corrected_valid_Fit_FWLS_logi)

roc.curve(Valid_logi_svm_fw$Y,corrected_valid_Fit_FWLS_logi$corrected_valid_Fit_FWLS_logi)
library(StatMeasures)
cm_valid=accuracy(Valid_logi_svm_fw$Y, corrected_valid_Fit_FWLS_logi$corrected_valid_Fit_FWLS_logi, cutoff=OF)
cm_valid$accuracyNum
cm_valid$overallAcc

cm_valid$accuracyNum[4]

acc=(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[2]+cm_valid$accuracyNum[3]+cm_valid$accuracyNum[4])
acc
TPR=(cm_valid$accuracyNum[4])/(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[2])
TPR
TNR=(cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[3])
TNR

#Testing
test_pred_FWLS_logi=data.frame(predict(FWLS_Model_logi,Test_logi_svm_fw[,-3],se.fit=TRUE))
test_pred2_FWLS_logi=data.frame(test_pred_FWLS_logi[,1])
test_Fit_FWLS_logi=exp(test_pred2_FWLS_logi)/(1+exp(test_pred2_FWLS_logi))
colnames(test_Fit_FWLS_logi)="test_Fit_FWLS_logi"
corrected_test_Fit_FWLS_logi=1/(1+(1/OF-1)/(1/OSF-1)*(1/test_Fit_FWLS_logi[,1]-1))
corrected_test_Fit_FWLS_logi=data.frame(corrected_test_Fit_FWLS_logi)
summary(corrected_test_Fit_FWLS_logi$corrected_test_Fit_FWLS_logi)

roc.curve(Test_logi_svm_fw$Y,corrected_test_Fit_FWLS_logi$corrected_test_Fit_FWLS_logi)
library(StatMeasures)
cm_test=accuracy(Test_logi_svm_fw$Y, corrected_test_Fit_FWLS_logi$corrected_test_Fit_FWLS_logi, cutoff=OF)
cm_test$accuracyNum
cm_test$overallAcc

cm_test$accuracyNum[4]

acc=(cm_test$accuracyNum[4]+cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[2]+cm_test$accuracyNum[3]+cm_test$accuracyNum[4])
acc
TPR=(cm_test$accuracyNum[4])/(cm_test$accuracyNum[4]+cm_test$accuracyNum[2])
TPR
TNR=(cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[3])
TNR
