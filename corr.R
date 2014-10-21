corr<-function(directory,threshold = 0){
        
        file_list<-list.files(path=directory,full.names=TRUE)
        
        
        pollutant_data<-lapply(file_list,read.csv)
        
        pollutant_data1<-do.call(rbind,pollutant_data)
        
        pollutant_data2<-lapply(split(pollutant_data1,pollutant_data1$ID),complete.cases)
        
        pollutant_data3<-lapply(pollutant_data2,sum)
        
        
        pollutant_data4<-do.call(rbind,pollutant_data3)
        
        
        pollutant_data5<-cbind(1:332,pollutant_data4)
        
        colnames(pollutant_data5)<-c("id","nobs")
        
        pollutant_data6 = data.frame(pollutant_data5)
        
        
        file_list<-paste(directory,"/",
                         formatC(pollutant_data6$id[pollutant_data6["nobs"]>threshold]
                                 ,width=3,flag="0"),".csv",sep="")
        
        
        if( length(pollutant_data6$id[pollutant_data6["nobs"]>threshold]) > 0 ){
                
                pollutant_data7<-lapply(file_list,read.csv)
                
                pollutant_data8<-do.call(rbind,pollutant_data7)
                
                #head(pollutant_data8)
                
                pollutant_data9<-split(pollutant_data8,pollutant_data8$ID)
                
                pollutant_data10<-sapply(pollutant_data9, function(x) cor(x[, c("nitrate", "sulfate")],use="complete.obs"))
                
                #class(pollutant_data10)
                a<-pollutant_data10[2,]
                round(a,digits=5)
        } else {
                length(pollutant_data6$id[pollutant_data6["nobs"]>threshold])
        }
        
        
}