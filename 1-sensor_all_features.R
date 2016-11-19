#code for taking features for 1 sensor
#let the data-matrix list be data_list

#data is normalized 

#features mean,sd,range,skewness


feat_ext<-function(senso)
{
print("extracting features")
  
L=list.files()

#L=L[1:10]
  
sensor=senso

feat_signal=matrix(ncol = 7,nrow = length(L))
cl=vector("numeric",length = length(L))


for(k in 1:length(L))
{
 # print(k)
  M=read.csv(L[k])
  s=M[,sensor]

  #class
  if(substr(L[k],start = 4,stop=5)==c("Pt"))
  {
    cl[k]=1
  }
 # print(cl[k])
  
  #extract different features
  
  #mean
  feat_signal[k,1]=mean(s)
  
  #standard deviation
  feat_signal[k,2]=sd(s)
  
  #range
  feat_signal[k,3]=max(s)-min(s)
  
  #skewness
  feat_signal[k,4]=skewness(s)
  
  #kurtosis
  feat_signal[k,5]=kurtosis(s)

  #median
  feat_signal[k,6]=median(s)
  
  #interquartile range
  feat_signal[k,7]=IQR(s)
}


#colnames(feat_signal)[1]=c("mean")
#colnames(feat_signal)[2]=c("standard deviation")
#colnames(feat_signal)[3]=c("range")
#colnames(feat_signal)[4]=c("skewness")
#colnames(feat_signal)[5]=c("kurtosis")
#colnames(feat_signal)[6]=c("median")
#colnames(feat_signal)[7]=c("Interquartile range")

feat_signal=cbind(feat_signal,cl)

#nul=which(is.na(feat_signal[,1]))
#nul=as.vector(nul)

print(dim(feat_signal))


#feat_signal=feat_signal[-(nul),]
#print(dim(feat_signal))

return(feat_signal)
}
