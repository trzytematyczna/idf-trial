library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(readr)


#####before joining probs with tweetinfo add in each of 54 files the header using add-ids-to-predict-results-x54-before-twitter-join-predictions.sh

# data_name<-"twitter-2M"
info_dir<-"./data/twitter/test-data-500K/"
data_dir<-"./results/twitter-trained/predict-results/"


data.files <- list.files(data_dir)
nfiles<-length(data.files)
# learning.data.file<-"./data/twitter/split-2M/twitter-2M-sampled.csv"
res_dir<-"./results/twitter-trained/assign-joined/"
 mdf<-data.frame()
 c<-1
 # count<-1
for (i in 1:54){
  assign.file<-paste0("assignment-file_",i,"-testsample.csv")
  test.file<-paste0("file_",i,"-testsample.csv")
  print(i)
  assign <- read_csv(paste0(data_dir,assign.file), col_types = cols (id = col_character()))
  test <- read_csv(paste0(info_dir,test.file), col_types = cols (id = col_character()))
  
  test$date<-as.Date(test$date)
  
  # colnames(assign)<-c("id","t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
  
  merged<-merge(test,assign, by="id")
  mdf<-rbind(mdf,merged)
  
  if(i%%9==0){
    print(i)
    # count<-1
    filename<-paste0("assign-",c,".csv")
    c<-c+1
    # mdf%>%write.csv(paste0(res_dir,filename), quote = TRUE, row.names = FALSE)
    mdf%>%write_csv(paste0(res_dir,filename))
    mdf<-data.frame()
  }
}
 
 # df<-df%>%arrange(date)
