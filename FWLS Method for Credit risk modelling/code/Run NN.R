#FilePath & Filenames
dpath = "D:/Vaibhav/Data/d2_all_variables"
Train_Filename="01_DPDTraining_SoftNo_Hit.csv"
Validation_Filename="01_DPDValidation_SoftNo_Hit.csv"
Test_Filename= "01_DPDTesting_SoftNo_Hit.csv"

#Control Parameters for Training
# Xcol=NN.train[,c(3:44,54)]
# ncol=1
# colname="A_Score"


source(file.path(dpath,"code/NN/Read in File.R"))
ReadFile(dpath)

# Create Vector of Column Max and Min Values
maxs_train <- apply(NN.train[,c(3:44,54)], 2, max)
mins_train <- apply(NN.train[,c(3:44,54)], 2, min)

maxs_validate <- apply(NN.validate[,c(3:44,54)], 2, max)
mins_validate <- apply(NN.validate[,c(3:44,54)], 2, min)

maxs_test <- apply(NN.test[,c(3:44,54)], 2, max)
mins_test <- apply(NN.test[,c(3:44,54)], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data_train <- as.data.frame(scale(NN.train[,c(3:44,54)],center = mins_train, scale = maxs_train - mins_train))
scaled.data_validate <- as.data.frame(scale(NN.validate[,c(3:44,54)],center = mins_validate, scale = maxs_validate - mins_validate))
scaled.data_test <- as.data.frame(scale(NN.test[,c(3:44,54)],center = mins_test, scale = maxs_test - mins_test))

scaled.data_train = cbind(scaled.data_train,NN.train$X60DPD)
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
scaled.data_train$Y=as.factor(scaled.data_train$Y)
smoted_data_NN=SMOTE(Y~.,scaled.data_train,perc.over = 200,perc.under = 150,learner=NULL)
smoted_data_NN$Y=as.numeric(as.character(smoted_data_NN$Y))
b=CrossTable(smoted_data_NN$Y)
OSF=b$prop.row[1,2]

source(file.path(dpath,"code/NN/Train NN.R"))
#NN_Model(Xcol,ncol,colname)
