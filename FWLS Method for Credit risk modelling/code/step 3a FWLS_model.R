########## creating Meta Features ###########

## Training dataset ############
library(data.table)
library(ggplot2)
library(gridExtra)
library(ROSE)
# logi_AUC=data.table()
# svm_AUC=data.table()
# NN_AUC=data.table()
# XGB_AUC=data.table()
# 
# curr_AUC_logi=data.table()
# curr_AUC_svm=data.table()
# curr_AUC_NN=data.table()
# curr_AUC_XGB=data.table()
# 
# meta_train=data.frame(Model.train[,c(3:44,54,56)],
#                       corrected_train_pred_logi$train_Fit_logi,
#                       corrected_train_pred_svmR$corrected_train_pred_svm,
#                       corrected_train_pred_NN$corrected_train_pred_NN,
#                       corrected_train_pred_xgb)
# colnames(meta_train)[45]="pred_logi"
# colnames(meta_train)[46]="pred_svm"
# colnames(meta_train)[47]="pred_NN"
# colnames(meta_train)[48]="pred_XGB"
# 
# m_tr_df=data.frame(meta_train[,c(1:43)])
# 
# n=10
# for(i in 1:length(m_tr_df)){
#   meta_train=meta_train[order( meta_train[,i],decreasing = FALSE ),]
#   v=split(meta_train, rep(1:n, each=ceiling(nrow(meta_train)/n), length.out=nrow(meta_train)))
#   for(j in 1:n){
#     v[[j]]$sp=j
#     M_trainFold<-v[[j]]
#     logi_AUC=roc.curve(M_trainFold$Y, M_trainFold$pred_logi)
#     svm_AUC=roc.curve(M_trainFold$Y, M_trainFold$pred_svm)
#     NN_AUC=roc.curve(M_trainFold$Y, M_trainFold$pred_NN)
#     XGB_AUC=roc.curve(M_trainFold$Y, M_trainFold$pred_XGB)
#     curr_AUC_logi=rbind(curr_AUC_logi,logi_AUC$auc)
#     curr_AUC_svm=rbind(curr_AUC_svm,svm_AUC$auc)
#     curr_AUC_NN=rbind(curr_AUC_NN,NN_AUC$auc)
#     curr_AUC_XGB=rbind(curr_AUC_XGB,XGB_AUC$auc)
#     }
# }
# 
# logi_AUCmx=matrix(curr_AUC_logi$x,nrow = n,ncol=ncol(m_tr_df))
# colnames(logi_AUCmx)=colnames(m_tr_df)
# svm_AUCmx=matrix(curr_AUC_svm$x,nrow = n,ncol=ncol(m_tr_df))
# colnames(svm_AUCmx)=colnames(m_tr_df)
# NN_AUCmx=matrix(curr_AUC_NN$x,nrow = n,ncol=ncol(m_tr_df))
# colnames(NN_AUCmx)=colnames(m_tr_df)
# XGB_AUCmx=matrix(curr_AUC_XGB$x,nrow = n,ncol=ncol(m_tr_df))
# colnames(XGB_AUCmx)=colnames(m_tr_df)
# 
# 
# grph_list = list()
# for (i in 1:length(m_tr_df)) {
#   V2=data.frame(logi_AUCmx[,i],
#                 svm_AUCmx[,i],
#                 NN_AUCmx[,i],
#                 XGB_AUCmx[,i],
#                 I=1:n)
#   colnames(V2)=c("LOGI",
#                  "SVM",
#                  "NN",
#                  "XGB",
#                  "FOLD")
#   #a = logi_AUCmx[, i]
#   #b = svm_AUCmx[, i]
#   # c = NN_AUCmx[, i]
#   # d = XGB_AUCmx[, i]
#   # #a1 = aes(x = 1:n,y = a)
#   # #a1$y = a
#   # #b1 = aes(x = 1:n,y = b)
#   # #b1$y = b
#   # c1 = aes(x = 1:n,y = c)
#   # c1$y = c
#   # d1 = aes(x = 1:n,y = d)
#   # d1$y = d
#   # 
#   grph = ggplot(data=V2,aes(FOLD))  + ylab('AUC') + xlab(colnames(NN_AUCmx)[i]) +
#     geom_line(aes(y=LOGI,colour="LOGI"))+
#     geom_line(aes(y=SVM,colour="SVM"))+
#     geom_line(aes(y=NN,colour="NN"))+
#     geom_line(aes(y=XGB,colour="XGB"))
#     
#   grph_list[[i]] = grph
# }
# 
# grid.arrange(grobs = grph_list, nrow = 7, newpage = FALSE)
# 


### validation dataset #########

#logi_AUCval=data.table()
svm_AUCval=data.table()
NN_AUCval=data.table()
#XGB_AUCval=data.table()

#curr_AUCval_logi=data.table()
curr_AUCval_svm=data.table()
curr_AUCval_NN=data.table()
#curr_AUCval_XGB=data.table()

meta_valid=data.frame( Model.validate[,c(3:44,54,56)],
                      #corrected_Val_pred_logi$Val_Fit_logi,
                      corrected_valid_pred_svmR$corrected_valid_pred_svm,
                      corrected_valid_pred_NN$corrected_valid_pred_NN
                      #corrected_valid_pred_xgb
                      )
#colnames(meta_valid)[45]="pred_logi"
colnames(meta_valid)[45]="pred_svm"
colnames(meta_valid)[46]="pred_NN"
#colnames(meta_valid)[46]="pred_XGB"

m_val_df=data.frame(meta_valid[,c(1:43)])

n=10
for(i in 1:length(m_val_df)){
  meta_valid=meta_valid[order( meta_valid[,i],decreasing = FALSE ),]
  v_val=split(meta_valid, rep(1:n, each=ceiling(nrow(meta_valid)/n), length.out=nrow(meta_valid)))
  for(j in 1:n){
    v_val[[j]]$sp=j
    M_validFold<-v_val[[j]]
    #logi_AUCval=roc.curve(M_validFold$X60DPD, M_validFold$pred_logi)
    svm_AUCval=roc.curve(M_validFold$X60DPD, M_validFold$pred_svm)
    NN_AUCval=roc.curve(M_validFold$X60DPD, M_validFold$pred_NN)
    #XGB_AUCval=roc.curve(M_validFold$X60DPD, M_validFold$pred_XGB)
    
    #curr_AUCval_logi=rbind(curr_AUCval_logi,logi_AUCval$auc)
    curr_AUCval_svm=rbind(curr_AUCval_svm,svm_AUCval$auc)
    curr_AUCval_NN=rbind(curr_AUCval_NN,NN_AUCval$auc)
    #curr_AUCval_XGB=rbind(curr_AUCval_XGB,XGB_AUCval$auc)
  }
}

# logi_AUCvalmx=matrix(curr_AUCval_logi$x,nrow = n,ncol=ncol(m_val_df))
# colnames(logi_AUCvalmx)=colnames(m_val_df)
svm_AUCvalmx=matrix(curr_AUCval_svm$x,nrow = n,ncol=ncol(m_val_df))
colnames(svm_AUCvalmx)=colnames(m_val_df)
NN_AUCvalmx=matrix(curr_AUCval_NN$x,nrow = n,ncol=ncol(m_val_df))
colnames(NN_AUCvalmx)=colnames(m_val_df)
# XGB_AUCvalmx=matrix(curr_AUCval_XGB$x,nrow = n,ncol=ncol(m_val_df))
# colnames(XGB_AUCvalmx)=colnames(m_val_df)

grph_list = list()
for (i in 1:length(m_val_df)) {
  V2_val=data.frame(#logi_AUCvalmx[,i],
                    svm_AUCvalmx[,i],
                    NN_AUCvalmx[,i],
                    #XGB_AUCvalmx[,i],
                    I=1:n)
  colnames(V2_val)=c(#"LOGI",
                     "SVM",
                     "NN",
                     #"XGB",
                     "FOLD")
  # a = logi_AUCvalmx[, i]
  # b = svm_AUCvalmx[, i]
  # c = NN_AUCvalmx[, i]
  # d = XGB_AUCvalmx[, i]
  #a1 = aes(x = 1:n,y = a)
  #a1$y = a
  #b1 = aes(x = 1:n,y = b)
  #b1$y = b
  # c1 = aes(x = 1:n,y = c)
  # c1$y = c
  # d1 = aes(x = 1:n,y = d)
  # d1$y = d
  
  grph_valid = ggplot(data=V2_val,aes(FOLD))  + ylab('AUC') + xlab(colnames(svm_AUCvalmx)[i]) +
    #geom_line(aes(y=LOGI,colour="LOGI"))+
    geom_line(aes(y=SVM,colour="SVM"))+
    geom_line(aes(y=NN,colour="NN"))
    #geom_line(aes(y=XGB,colour="XGB"))
  
  grph_list[[i]] = grph_valid
}

grid.arrange(grobs = grph_list, nrow = 7, newpage = FALSE)


# selecting best variables from above graphs and use those variables as meta features

M_train=read.csv("D:/Vaibhav/FWLS Method/Input/01_DPDTraining_SoftNo_Hit.csv")
M_validate=read.csv("D:/Vaibhav/FWLS Method/Input/01_DPDValidation_SoftNo_Hit.csv")
M_test=read.csv("D:/Vaibhav/FWLS Method/Input/01_DPDTesting_SoftNo_Hit.csv")

all_df=read.csv("D:/Vaibhav/LTFS/data/ALL_LTFS_df.csv")
M_train_cnt=data.frame(all_df[1:62156,])
M_validate_cnt=data.frame(all_df[62157:82850,])
M_test_cnt=data.frame(all_df[82851:103471,])


## Create M1 meta feature M_train data use "all_loans_amt_outstanding_absolute" variable (log(sep_varib+1))
M_train$M1=log(M_train$private_sector_bank_mitra + 1)
M_train$M2=log(M_train$term_deposits_value + 1)
M_train$M3=log(M_train$toilet_percent + 1)
M_train$M4=log(M_train$tv_comp_vehicle_phone_percent + 1)
M_train$M5=M_train_cnt$count_pincode

M_validate$M1=log(M_validate$private_sector_bank_mitra + 1)
M_validate$M2=log(M_validate$term_deposits_value + 1)
M_validate$M3=log(M_validate$toilet_percent + 1)
M_validate$M4=log(M_validate$tv_comp_vehicle_phone_percent + 1)
M_validate$M5=M_validate_cnt$count_pincode

M_test$M1=log(M_test$private_sector_bank_mitra + 1)
M_test$M2=log(M_test$term_deposits_value + 1)
M_test$M3=log(M_test$toilet_percent + 1)
M_test$M4=log(M_test$tv_comp_vehicle_phone_percent + 1)
M_test$M5=M_test_cnt$count_pincode


#f0*g0+f1*g0+f0*g1+f1*g1+f0*g2+f1*g2
#f0=1, g0=1 then 1+f1+g1+f1*g1+g2+f1*g2
#drop 1 then final FWLS Features are f1+g1+f1*g1+g2+f1*g2
#f0*g0+f1*g0+f2*g0+f0*g1+f1*g1+f2*g1+f0*g2+f1*g2+f2*g2 => 1+f1+f2+g1+f1*g1+f2*g1+g2+f1*g2+f2*g2

# seven variable as meta feature
#f0*g0+f1*g0+f2*g0+f3*g0+f4*g0+f5*g0+f6*g0+f7*g0+
#f0*g1+f1*g1+f2*g1+f3*g1+f4*g1+f5*g1+f6*g1+f7*g1+
#f0*g2+f1*g2+f2*g2+f3*g2+f4*g2+f5*g2+f6*g2+f7*g2

#f1+f2+f3+f4+f5+f6+f7+g1+f1*g1+f2*g1+f3*g1+f4*g1+f5*g1+f6*g1+f7*g1+g2+f1*g2+f2*g2+f3*g2+f4*g2+f5*g2+f6*g2+f7*g2

# five variables as meta feature
# f0*g0+f1*g0+f2*g0+f3*g0+f4*g0+f5*g0+f0*g1+f1*g1+f2*g1+f3*g1+f4*g1+f5*g1+f0*g2+f1*g2+f2*g2+f3*g2+f4*g2+f5*g2
# 
# f1+f2+f3+f4+f5+g1+f1*g1+f2*g1+f3*g1+f4*g1+f5*g1+g2+f1*g2+f2*g2+f3*g2+f4*g2+f5*g2


fwls_tr=data.frame(f1=M_train$M1,f2=M_train$M2,f3=M_train$M3,f4=M_train$M4,f5=M_train$M5,
                   g1=tot_pred_svm$test_Fit_svm,
                   p_sf1= M_train$M1 * tot_pred_svm$test_Fit_svm,
                   p_sf2= M_train$M2 * tot_pred_svm$test_Fit_svm,
                   p_sf3= M_train$M3 * tot_pred_svm$test_Fit_svm,
                   p_sf4= M_train$M4 * tot_pred_svm$test_Fit_svm,
                   p_sf5= M_train$M5 * tot_pred_svm$test_Fit_svm,
                   g2=tot_pred_xgb$corrected_trfl_pred_xgb,
                   p_xf1= M_train$M1 * tot_pred_xgb$corrected_trfl_pred_xgb,
                   p_xf2= M_train$M2 * tot_pred_xgb$corrected_trfl_pred_xgb,
                   p_xf3= M_train$M3 * tot_pred_xgb$corrected_trfl_pred_xgb,
                   p_xf4= M_train$M4 * tot_pred_xgb$corrected_trfl_pred_xgb,
                   p_xf5= M_train$M5 * tot_pred_xgb$corrected_trfl_pred_xgb,
                   Y=tot_pred_logit$Y)

fwls_vl=data.frame(f1=M_validate$M1,f2=M_validate$M2,f3=M_validate$M3,f4=M_validate$M4,f5=M_validate$M5,
                   g1=corrected_valid_pred_svmR$corrected_valid_pred_svm,
                   p_sf1= M_validate$M1 * corrected_valid_pred_svmR$corrected_valid_pred_svm,
                   p_sf2= M_validate$M2 * corrected_valid_pred_svmR$corrected_valid_pred_svm,
                   p_sf3= M_validate$M3 * corrected_valid_pred_svmR$corrected_valid_pred_svm,
                   p_sf4= M_validate$M4 * corrected_valid_pred_svmR$corrected_valid_pred_svm,
                   p_sf5= M_validate$M5 * corrected_valid_pred_svmR$corrected_valid_pred_svm,
                   g2=corrected_valid_pred_xgb,
                   p_xf1= M_validate$M1 * corrected_valid_pred_xgb,
                   p_xf2= M_validate$M2 * corrected_valid_pred_xgb,
                   p_xf3= M_validate$M3 * corrected_valid_pred_xgb,
                   p_xf4= M_validate$M4 * corrected_valid_pred_xgb,
                   p_xf5= M_validate$M5 * corrected_valid_pred_xgb,
                   Y=Model.validate$X60DPD)

fwls_ts=data.frame(f1=M_test$M1,f2=M_test$M2,f3=M_test$M3,f4=M_test$M4,f5=M_test$M5,
                   g1=corrected_test_pred_svmR$corrected_test_pred_svm,
                   p_sf1= M_test$M1 * corrected_test_pred_svmR$corrected_test_pred_svm,
                   p_sf2= M_test$M2 * corrected_test_pred_svmR$corrected_test_pred_svm,
                   p_sf3= M_test$M3 * corrected_test_pred_svmR$corrected_test_pred_svm,
                   p_sf4= M_test$M4 * corrected_test_pred_svmR$corrected_test_pred_svm,
                   p_sf5= M_test$M5 * corrected_test_pred_svmR$corrected_test_pred_svm,
                   g2=corrected_test_pred_xgb,
                   p_xf1= M_test$M1 * corrected_test_pred_xgb,
                   p_xf2= M_test$M2 * corrected_test_pred_xgb,
                   p_xf3= M_test$M3 * corrected_test_pred_xgb,
                   p_xf4= M_test$M4 * corrected_test_pred_xgb,
                   p_xf5= M_test$M5 * corrected_test_pred_xgb,
                   Y=Model.test$X60DPD)

### logistic model on FWLS DATA
fwls_tr$Y=as.factor(fwls_tr$Y)
library(gmodels)
library(DMwR)
library(ROCR)
a=CrossTable(fwls_tr$Y)
OF=a$prop.row[1,2]
set.seed(123)
smoted_Sx_fw=SMOTE(Y ~.,fwls_tr,perc.over = 200,perc.under = 150,learner=NULL)
b=CrossTable(smoted_Sx_fw$Y)
OSF=b$prop.row[1,2]
sum(is.na(smoted_Sx_fw))
#Logistic Model
FWLS_Model_logi=glm(Y ~.,family=binomial(link=logit),data=smoted_Sx_fw)
summary(FWLS_Model_logi)

#Training
train_pred_FWLS_logi=data.frame(predict(FWLS_Model_logi,fwls_tr[,-18],se.fit=TRUE))
train_pred2_FWLS_logi=data.frame(train_pred_FWLS_logi[,1])
train_Fit_FWLS_logi=exp(train_pred2_FWLS_logi)/(1+exp(train_pred2_FWLS_logi))
colnames(train_Fit_FWLS_logi)="train_Fit_FWLS_logi"
corrected_train_Fit_FWLS_logi=1/(1+(1/OF-1)/(1/OSF-1)*(1/train_Fit_FWLS_logi[,1]-1))
corrected_train_Fit_FWLS_logi=data.frame(corrected_train_Fit_FWLS_logi)
summary(corrected_train_Fit_FWLS_logi$corrected_train_Fit_FWLS_logi)
library(ROSE)
roc.curve(fwls_tr$Y,corrected_train_Fit_FWLS_logi$corrected_train_Fit_FWLS_logi)
library(StatMeasures)
cm_train=accuracy(fwls_tr$Y,corrected_train_Fit_FWLS_logi$corrected_train_Fit_FWLS_logi, cutoff=OF)
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
valid_pred_FWLS_logi=data.frame(predict(FWLS_Model_logi,fwls_vl[,-18],se.fit=TRUE))
valid_pred2_FWLS_logi=data.frame(valid_pred_FWLS_logi[,1])
valid_Fit_FWLS_logi=exp(valid_pred2_FWLS_logi)/(1+exp(valid_pred2_FWLS_logi))
colnames(valid_Fit_FWLS_logi)="valid_Fit_FWLS_logi"
corrected_valid_Fit_FWLS_logi=1/(1+(1/OF-1)/(1/OSF-1)*(1/valid_Fit_FWLS_logi[,1]-1))
corrected_valid_Fit_FWLS_logi=data.frame(corrected_valid_Fit_FWLS_logi)
summary(corrected_valid_Fit_FWLS_logi$corrected_valid_Fit_FWLS_logi)

roc.curve(fwls_vl$Y,corrected_valid_Fit_FWLS_logi$corrected_valid_Fit_FWLS_logi)
library(StatMeasures)
cm_valid=accuracy(fwls_vl$Y,corrected_valid_Fit_FWLS_logi$corrected_valid_Fit_FWLS_logi, cutoff=OF)
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
test_pred_FWLS_logi=data.frame(predict(FWLS_Model_logi,fwls_ts[,-18],se.fit=TRUE))
test_pred2_FWLS_logi=data.frame(test_pred_FWLS_logi[,1])
test_Fit_FWLS_logi=exp(test_pred2_FWLS_logi)/(1+exp(test_pred2_FWLS_logi))
colnames(test_Fit_FWLS_logi)="test_Fit_FWLS_logi"
corrected_test_Fit_FWLS_logi=1/(1+(1/OF-1)/(1/OSF-1)*(1/test_Fit_FWLS_logi[,1]-1))
corrected_test_Fit_FWLS_logi=data.frame(corrected_test_Fit_FWLS_logi)
summary(corrected_test_Fit_FWLS_logi$corrected_test_Fit_FWLS_logi)

roc.curve(fwls_ts$Y,corrected_test_Fit_FWLS_logi$corrected_test_Fit_FWLS_logi)
library(StatMeasures)
cm_test=accuracy(fwls_ts$Y,corrected_test_Fit_FWLS_logi$corrected_test_Fit_FWLS_logi, cutoff=OF)
cm_test$accuracyNum
cm_test$overallAcc

cm_test$accuracyNum[4]

acc=(cm_test$accuracyNum[4]+cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[2]+cm_test$accuracyNum[3]+cm_test$accuracyNum[4])
acc
TPR=(cm_test$accuracyNum[4])/(cm_test$accuracyNum[4]+cm_test$accuracyNum[2])
TPR
TNR=(cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[3])
TNR

# SVM Model on fwls data
set.seed(123)
fwls_model_svm<-svm(Y~.,data=smoted_Sx_fw,kernel='radial',cost=0.75,gamma=0.015,probability=TRUE)

#Training data prediction

train_pred_fwls_svm=predict(fwls_model_svm,fwls_tr[,-15],probability=TRUE)
prob_fwls_train=attr(train_pred_fwls_svm,"probabilities")
prob_fwls_train=data.frame(prob_fwls_train)
corrected_trfwls_pred_svm=1/(1+(1/OF-1)/(1/OSF-1)*(1/prob_fwls_train[,2]-1))
corrected_trfwls_pred_svm=data.frame(corrected_trfwls_pred_svm)

summary(corrected_trfwls_pred_svm$corrected_trfwls_pred_svm)
roc.curve(fwls_tr$Y, corrected_trfwls_pred_svm$corrected_trfwls_pred_svm)

library(StatMeasures)
cm_train=accuracy(fwls_tr$Y, corrected_trfwls_pred_svm$corrected_trfwls_pred_svm, cutoff=OF)
cm_train$accuracyNum
cm_train$overallAcc

cm_train$accuracyNum[4]


acc=(cm_train$accuracyNum[4]+cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[2]+cm_train$accuracyNum[3]+cm_train$accuracyNum[4])
acc
TPR=(cm_train$accuracyNum[4])/(cm_train$accuracyNum[4]+cm_train$accuracyNum[2])
TPR
TNR=(cm_train$accuracyNum[1])/(cm_train$accuracyNum[1]+cm_train$accuracyNum[3])
TNR

#validate data prediction

valid_pred_fwls_svm=predict(fwls_model_svm,fwls_vl[,-15],probability=TRUE)
prob_fwls_valid=attr(valid_pred_fwls_svm,"probabilities")
prob_fwls_valid=data.frame(prob_fwls_valid)
corrected_valfwls_pred_svm=1/(1+(1/OF-1)/(1/OSF-1)*(1/prob_fwls_valid[,2]-1))
corrected_valfwls_pred_svm=data.frame(corrected_valfwls_pred_svm)

summary(corrected_valfwls_pred_svm$corrected_valfwls_pred_svm)
roc.curve(fwls_vl$Y, corrected_valfwls_pred_svm$corrected_valfwls_pred_svm)

library(StatMeasures)
cm_valid=accuracy(fwls_vl$Y, corrected_valfwls_pred_svm$corrected_valfwls_pred_svm, cutoff=OF)
cm_valid$accuracyNum
cm_valid$overallAcc

cm_valid$accuracyNum[4]


acc=(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[2]+cm_valid$accuracyNum[3]+cm_valid$accuracyNum[4])
acc
TPR=(cm_valid$accuracyNum[4])/(cm_valid$accuracyNum[4]+cm_valid$accuracyNum[2])
TPR
TNR=(cm_valid$accuracyNum[1])/(cm_valid$accuracyNum[1]+cm_valid$accuracyNum[3])
TNR

#test data prediction

test_pred_fwls_svm=predict(fwls_model_svm,fwls_ts[,-15],probability=TRUE)
prob_fwls_test=attr(test_pred_fwls_svm,"probabilities")
prob_fwls_test=data.frame(prob_fwls_test)
corrected_tsfwls_pred_svm=1/(1+(1/OF-1)/(1/OSF-1)*(1/prob_fwls_test[,2]-1))
corrected_tsfwls_pred_svm=data.frame(corrected_tsfwls_pred_svm)

summary(corrected_tsfwls_pred_svm$corrected_tsfwls_pred_svm)
roc.curve(fwls_ts$Y, corrected_tsfwls_pred_svm$corrected_tsfwls_pred_svm)

library(StatMeasures)
cm_test=accuracy(fwls_ts$Y, corrected_tsfwls_pred_svm$corrected_tsfwls_pred_svm, cutoff=OF)
cm_test$accuracyNum
cm_test$overallAcc

cm_test$accuracyNum[4]


acc=(cm_test$accuracyNum[4]+cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[2]+cm_test$accuracyNum[3]+cm_test$accuracyNum[4])
acc
TPR=(cm_test$accuracyNum[4])/(cm_test$accuracyNum[4]+cm_test$accuracyNum[2])
TPR
TNR=(cm_test$accuracyNum[1])/(cm_test$accuracyNum[1]+cm_test$accuracyNum[3])
TNR
