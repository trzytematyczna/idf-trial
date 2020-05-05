library(dplyr)
library(data.table)
library(stringr)


data_name<-"twitter-500K"
data_dir<-"./data/twitter/split-500K/"

data.files <- list.files(data_dir)
nfiles<-length(data.files)
learning.data.file<-"./data/twitter/split-2M/twitter-2M-sampled.csv"
res_dir<-"./data/twitter/test-data-500K/"

# res<-data.frame()
for (i in data.files){
  print(i)
  df <- read.csv(paste0(data_dir,i), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8", na.strings = NA)
  df<-df[!df$text=="",] #empty tweets
  
  #learning data used for LDA
  learning.data <- read.csv(paste0(learning.data.file), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8")
  df<-df[!df$id %in% learning.data$id,]

  #weatherbots
  df<-df[!df$text %like% "Climate Report: High: ",] #61770
  df<-df[!df$text %like% "Climate: Hi: ",] #96986
  df<-df[!df$text %like% "climate report: high: ",] 
  df<-df[!df$text %like% "climate: hi: ",] 
  df<-df[!df$from_user_name %like% "ClapRobot",]
  df<-df[!df$from_user_name %like% "OKCStormWatcher",]
  #dailybots
  df<-df[!df$text %like% "daily is out!",] #660
  df<-df[!df$text %in% grep("^the.*daily!$", df$text, value = TRUE) ,] #4283
  df<-df[!df$text %in% grep("^the latest.*!$", df$text, value = TRUE) ,]#8600
  df<-df[!df$from_user_name %like% "ETSKYWARN",]
  
  filename<-paste0(str_replace(i,".csv",""),"-testsample.csv")
  # format(head(learning.data$id, n=50), digits=22)
  # df %>% fwrite(paste0("./data/twitter/test-data-2M/",filename), quote = FALSE, row.names = FALSE, sep=",", qmethod = "double")
  df%>%write.csv(paste0(res_dir,filename), quote = TRUE, row.names = FALSE)
  # res <- rbind(res,new.sample)
}


#Climate: Hi: 46 Lo: 27 Precip: 0.0 Snow: 0.0
#Climate Report: High: 34 Low: 17 Precip: 0.03 Snow: 0.2"