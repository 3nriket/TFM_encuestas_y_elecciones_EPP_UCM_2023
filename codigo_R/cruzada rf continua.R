

# library(dplyr)
# library(dummies)
# library(MASS)
# library(reshape)
# library(caret)


cruzadarf<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",
  grupos=4,sinicio=1234,repe=5,
  nodesize=20,replace=TRUE,ntree=100,mtry=2,sampsize=1)
  
 { 
 # library(caret)
#  library(dplyr)
 # library(dummies)
  
    # Preparación del archivo
  
  # b)pasar las categóricas a dummies
  
  if  (listclass!=c(""))
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
   databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",
   number=grupos,repeats=repe,
   savePredictions = "all") 
  
  # Aplico caret y construyo modelo

   rfgrid <-expand.grid(mtry=mtry)
  
     if  (sampsize==1)
  {
    rf<- train(formu,data=databis,
   method="rf",trControl=control,
   tuneGrid=rfgrid,nodesize=nodesize,replace=replace,ntree=ntree)
  }
  
else  if  (sampsize!=1)
  {
    rf<- train(formu,data=databis,
   method="rf",trControl=control,
   tuneGrid=rfgrid,nodesize=nodesize,replace=replace,sampsize=sampsize,
   ntree=ntree)
 }

  
  print(rf$results)
  
  preditest<-rf$pred


    preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL

  # preditest$pred <- preditest %>% as.numeric(pred)
  # preditest$obs <- preditest %>% as.numeric(obs)
  preditest$error<-abs(preditest$pred-preditest$obs)
  
  
  medias<-preditest %>%
    group_by(Rep) %>%
    summarize(error=mean(error))    
  
return(medias)
  
}
