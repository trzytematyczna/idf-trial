library(dplyr)
library(data.table)
library(stringr)
library(DescTools)

# data_name<-"twitter-2M"
info_dir<-"./data/twitter/test-data-500K/"
data_dir<-"./results/twitter-trained/assign/"


data.files <- list.files(data_dir)
nfiles<-length(data.files)
# learning.data.file<-"./data/twitter/split-2M/twitter-2M-sampled.csv"
res_dir<-"./results/twitter-trained/assign-joined/"
# data.files<-data.files[1:2]
 mdf<-data.frame()
 c<-1
 # count<-1
for (i in 1:54){
  assign.file<-paste0("assignment-file_",i,"-testsample.csv")
  test.file<-paste0("file_",i,"-testsample.csv")
  print(i)
  assign <- read.csv(paste0(data_dir,assign.file), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8", na.strings = NA)
  test <- read.csv(paste0(info_dir,test.file), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8", na.strings = NA)
  test$date<-as.Date(test$date)
  
  colnames(assign)<-c("id","1","2","3","4","5","6","7","8","9")
  
  merged<-merge(test,assign, by="id")
  mdf<-rbind(mdf,merged)
  
  if(i%%9==0){
    print(i)
    # count<-1
    filename<-paste0("assign-",c,".csv")
    c<-c+1
    mdf%>%write.csv(paste0(res_dir,filename), quote = TRUE, row.names = FALSE)
    mdf<-data.frame()
  }
}
 
 # df<-df%>%arrange(date)
