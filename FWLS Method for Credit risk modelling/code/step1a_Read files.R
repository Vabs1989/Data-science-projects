#rm(list = ls())
# gc(verbose = TRUE,reset = TRUE,full = TRUE)

dpath = "D:/Vaibhav/FWLS Method"
Train_Filename="01_DPDTraining_SoftNo_Hit.csv"
Validation_Filename="01_DPDValidation_SoftNo_Hit.csv"
Test_Filename= "01_DPDTesting_SoftNo_Hit.csv"
Kfolds=5
source(file.path(dpath,"code/step1_Read in File.R"))
library(data.table)
ReadFile(dpath)
binnum ="Y"


