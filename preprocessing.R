#preprocessing
L=list.files()
library(signal)
myfilter = butter(2, 0.6, type = 'low', plane='z')  


for(i in 1:length(L))
{
  print(i)
  E=read.table(L[i])
  E=data.matrix(E)
  E=E[,-1]
  
  for(j in 1:ncol(E))
  {
    s=E[,j]
    yfiltered = signal:::filter(myfilter, s)
    n=norm(yfiltered,type = "2")
    E[,j]=yfiltered/n
  }
  
  lab=paste("P",substr(L[i],start = 1,stop = 9),"(0.6)",".csv",sep = "")
  write.csv(E,lab,row.names = FALSE)
}