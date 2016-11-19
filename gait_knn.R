initial <- Sys.time()

library(ROCR)
library(class)
library(moments)
library(e1071)



f=10
cv=10


sensor=18
m_res=matrix(ncol=5,nrow = sensor)


acc=matrix(ncol = sensor,nrow = f)
sensitivity=matrix(ncol = sensor,nrow = f)
specificity=matrix(ncol = sensor,nrow = f)
auc=matrix(ncol = sensor,nrow = f)
mcc=matrix(ncol = sensor,nrow = f)

for(ii in 1:sensor)
{
  print(ii)  
  
  E=feat_ext(ii)
  
  #print(dim(E))
  
  S<-E[sample(nrow(E),replace = FALSE),]
  folds <- cut(seq(1,nrow(S)),breaks=f,labels=FALSE)
  
  for(z in 1:f)
  {
    # print(z)
    
    
    testindexes <- which(folds==z,arr.ind=TRUE)
    testdata <- S[testindexes, ]
    traindata <- S[-testindexes, ]
    
    train=traindata[,-ncol(traindata)]
    train_cl=traindata[,ncol(traindata)]
    train_cl=as.factor(train_cl)
    
    test=testdata[,-ncol(testdata)]
    test_cl=testdata[,ncol(testdata)]
    
    pred_cl=knn(as.matrix(train),as.matrix(test),train_cl)
    
    pred=prediction(as.numeric(levels(pred_cl))[pred_cl],test_cl)
    
    acc[z,ii]=attr(performance(pred,"acc"),"y.values")[[1]][2]
    sensitivity[z,ii]=attr(performance(pred,"sens"),"y.values")[[1]][2]
    specificity[z,ii]=attr(performance(pred,"spec"),"y.values")[[1]][2]
    auc[z,ii]=attr(performance(pred,"auc"),"y.values")[[1]][1]
    
    t=table(test_cl,pred_cl)
    
    mcc[z,ii]=((t[2,2]*t[1,1])-(t[1,2]*t[2,1]))/sqrt((t[2,2]+t[1,2])*(t[2,2]+t[2,1])*(t[1,1]+t[2,1])*(t[1,1]+t[1,2]))
    #  print(t)
  }
  print("sensor end")
  
  m_res[ii,1]=mean(acc[,ii],na.rm = TRUE)
  m_res[ii,2]=mean(sensitivity[,ii],na.rm = TRUE)
  m_res[ii,3]=mean(specificity[,ii],na.rm = TRUE)
  m_res[ii,4]=mean(auc[,ii],na.rm = TRUE)
  m_res[ii,5]=mean(mcc[,ii],na.rm = TRUE)
}

M=data.frame(m_res)
colnames(M)=c("acc","sensitivity","specificity","auc","mcc")
MR=M[c(1:8,17),]
ML=M[c(9:16,18),]

#setwd("C:/Users/Neel Jambhekar/Desktop/sem 4-1/dop/gaitpdb/pre-processed/res")
#write.csv(MR,"0.3,7feat_right.csv",row.names = T)
#write.csv(ML,"0.3,7feat_left.csv",row.names = T)

ft= Sys.time()
