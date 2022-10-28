
ReadFile=function(dpath)
  {
  a1=  file.path(paste(dpath,"Input",Train_Filename,sep="/"))
  a2=  file.path(paste(dpath,"Input",Validation_Filename,sep="/"))
  a3=  file.path(paste(dpath,"Input",Test_Filename,sep="/"))
  
  Model.train<<-as.data.table(read.csv(a1))
  Model.validate<<-as.data.table(read.csv(a2))
  Model.test<<-as.data.table(read.csv(a3))
  
  colnames(Model.train)[ncol(Model.train)]<<-"Y"
  Model.train$Y<-as.factor(Model.train$Y)
  }

