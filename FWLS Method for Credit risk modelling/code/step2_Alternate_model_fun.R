library(mltools)
library(DMwR)
library(data.table)
library(gmodels)
#library(randomForest)

train_fold=Model.train[, FoldID := folds(Y, nfolds=5, stratified=TRUE, seed=2016)]
tot_pred_logit = data.frame()
tot_pred_svm = data.frame()
tot_pred_xgb = data.frame()

#Loop through each test fold, fit model to training folds and make predictions on test fold
  for(foldID in 1:Kfolds){
    #foldID=5
    # Build the train/test folds
    print(paste("fold id =",foldID))
    Model.testFold <<- train_fold[J(FoldID=foldID), on="FoldID"]
    Model.trainFolds <- data.frame(Model.train[!J(FoldID=foldID), on="FoldID"])
    Model.trainFolds$Y=as.factor(Model.trainFolds$Y)
    a=CrossTable(Model.trainFolds$Y)
    OF=a$prop.row[1,2]
    set.seed(123)
    smoted_train=SMOTE(Y ~.,Model.trainFolds[,c(3:44,54,56)],perc.over = 200,perc.under = 150)
    b=CrossTable(smoted_train$Y)
    OSF=b$prop.row[1,2]
    
    
    # Train the model & make predictions
    #browser()
    
    #LOGIT MODEL
    
    logi_smote_train=glm(Y ~.,family=binomial(link=logit),smoted_train)
    Model.testFoldX=data.frame(Model.testFold[,c(3:44,54)])
    Model.trainFoldX=data.frame(Model.trainFolds[,c(3:44,54)])
    test_pred_logi=data.frame(predict(logi_smote_train,Model.testFoldX,se.fit=TRUE))
    test_Pred2_logi=data.frame(test_pred_logi[,1])
    test_Fit_logi=exp(test_Pred2_logi)/(1+exp(test_Pred2_logi))
    colnames(test_Fit_logi)="test_Fit_logi"
    corrected_pred_logi=1/(1+(1/OF-1)/(1/OSF-1)*(1/test_Fit_logi-1))
    curr_pred_logit=cbind(Model.testFold,corrected_pred_logi)
    tot_pred_logit = rbind(tot_pred_logit,curr_pred_logit)
    
    #SVM MODEL
    
    library(e1071)
    #tune.results <- tune(e1071::svm,Y~., data=smoted_trainX,kernel='radial',ranges=list(cost=0.75, gamma=0.015))
    model_svm<-svm(Y~.,data=smoted_train,kernel='radial',cost=0.75,gamma=0.015,probability=TRUE)
    
    test_pred_svm=predict(model_svm,Model.testFoldX,probability=TRUE)
    prob=attr(test_pred_svm,"probabilities")                        
    test_Pred2_svm=data.frame(prob[,2])
    colnames(test_Pred2_svm)="test_Fit_svm"
    corrected_pred_svm=1/(1+(1/OF-1)/(1/OSF-1)*(1/test_Pred2_svm-1))
    curr_pred_svm=cbind(Model.testFold,corrected_pred_svm)
    tot_pred_svm = rbind(tot_pred_svm,curr_pred_svm)
    
    #xgb Model
    library(xgboost)
    Model.trainFolds$Y=as.numeric(as.character(Model.trainFolds$Y))
    smoted_train$Y=as.numeric(as.character(smoted_train$Y))
    
    trfl_mx_sm=xgb.DMatrix(data=as.matrix(smoted_train[,-44]),label=smoted_train$Y)
    trfl_mx=xgb.DMatrix(data=as.matrix(Model.trainFoldX),label=Model.trainFolds$Y)
    tsfl_mx=xgb.DMatrix(data=as.matrix(Model.testFoldX),label=Model.testFold$Y)
    
    xgbfl_par=list("objective" = "binary:logistic",
                    "booster" = "gbtree",
                    "min_child_weight"=1)
    watchlist_fl=list(train_smote=trfl_mx_sm,train=trfl_mx,test=tsfl_mx)
    
    set.seed(123)
    model_xgb_fl=xgb.train(params = xgbfl_par,
                           data =trfl_mx_sm,
                           watchlist=watchlist_fl,
                           nrounds = 309,
                           eta=0.01,
                           gamma = 0.3,
                           max.depth = 3)
    pred_tr_fl <- predict(model_xgb_fl, tsfl_mx)
    corrected_trfl_pred_xgb=1/(1+(1/OF-1)/(1/OSF-1)*(1/pred_tr_fl-1))
    curr_pred_xgb=cbind(Model.testFold,corrected_trfl_pred_xgb)
    tot_pred_xgb = rbind(tot_pred_xgb,curr_pred_xgb)
    }
