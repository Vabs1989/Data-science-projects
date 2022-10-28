feats <- names(smoted_data_NN[,c(2,3,4,6,7,8,9,18,21,23,24,26,27,29,30,34,36,37,38,39,41,43)])
feats
# # Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste("Y ~",f)

# Convert to formula
f <- as.formula(f)
f
#install.packages("neuralnet")
library(neuralnet)

nn <-neuralnet( formula = f, 
                data = smoted_data_NN, 
                hidden=2,
                threshold = 0.01,
                stepmax = 1e+06,
                learningrate=0.0001,
                lifesign = "full",
                algorithm = "rprop+",
                err.fct="ce",
                act.fct="logistic",
                linear.output=FALSE )
plot(nn)

#  Compute Predictions train Set
pred_nn_train <-neuralnet::compute(nn,scaled.data_train[,feats])

pred_nn_train=data.frame(pred_nn_train)
corrected_train_pred_NN=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_nn_train[,ncol(pred_nn_train)]-1))
corrected_train_pred_NN=data.frame(corrected_train_pred_NN)
NN.train=cbind(NN.train,corrected_train_pred_NN)

library(ROSE)
roc.curve(scaled.data_train$Y,corrected_train_pred_NN$corrected_train_pred_NN)

#  Compute Predictions validate Set
pred_nn_valid <-neuralnet::compute(nn,scaled.data_validate[,feats])

pred_nn_valid=data.frame(pred_nn_valid)
corrected_valid_pred_NN=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_nn_valid[,ncol(pred_nn_valid)]-1))
corrected_valid_pred_NN=data.frame(corrected_valid_pred_NN)
NN.validate=cbind(NN.validate,corrected_valid_pred_NN)

library(ROSE)
roc.curve(scaled.data_validate$Y,corrected_valid_pred_NN$corrected_valid_pred_NN)

#  Compute Predictions test Set
pred_nn_test <-neuralnet::compute(nn,scaled.data_test[,feats])

pred_nn_test=data.frame(pred_nn_test)
corrected_test_pred_NN=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_nn_test[,ncol(pred_nn_test)]-1))
corrected_test_pred_NN=data.frame(corrected_test_pred_NN)
NN.test=cbind(NN.test,corrected_test_pred_NN)

library(ROSE)
roc.curve(scaled.data_test$Y,corrected_test_pred_NN$corrected_test_pred_NN)
