pollutantmean<-function(directory,pollutant_var,id=1:332){
        
  file_list<-paste(directory,"/",formatC(id,width=3,flag="0"),".csv",sep="")
  
  pollutant_data<-lapply(file_list,read.csv)
  
  pollutant_data1<-do.call(rbind,pollutant_data)
  
  mean(pollutant_data1[pollutant_var][!is.na(pollutant_data1[pollutant_var])])
  
}