complete<-function(directory,id=1:332){
  
  file_list<-paste(directory,"/",formatC(id,width=3,flag="0"),".csv",sep="")
  
  pollutant_data<-lapply(file_list,read.csv)
  
  pollutant_data1<-do.call(rbind,pollutant_data)
  
  pollutant_data2<-lapply(split(pollutant_data1,pollutant_data1$ID),complete.cases)
  
  pollutant_data3<-lapply(pollutant_data2,sum)
  
  pollutant_data4<-do.call(rbind,pollutant_data3)
  
  colnames(pollutant_data4)<-c("nobs")
  
  data.frame(pollutant_data4)
}