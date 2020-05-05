library(dplyr)
library(data.table)



data_name<-"twitter-2M"
data_dir<-"./data/twitter/split-2M/"

data.files <- list.files(data_dir)
bots.file <- paste0("twitter-bots-ids.csv")
nfiles<-length(data.files)


gathered.ids<-data.frame()
for (i in data.files){
  print(i)
  df <- read.csv(paste0(data_dir,i), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8")
  df<-df[!is.na(df$text),] #empty tweets
  df$text<-tolower(df$text)
  #weatherbots
  high <- data.frame(df[df$text %like% "climate report: high: ",]$from_user_id)
  hi <- as.data.frame(df[df$text %like% "climate: hi: ",]$from_user_id)
  clap <- as.data.frame(df[df$from_user_name %like% "ClapRobot",]$from_user_id)
  ok <-as.data.frame(df[df$from_user_name %like% "OKCStormWatcher",]$from_user_id)
  names<-c("from_user_id")
  colnames(high)<-names
  colnames(hi)<-names
  colnames(clap)<-names
  colnames(ok)<-names
  #dailybots
  daily <- as.data.frame(df[df$text %like% "daily is out!",]$from_user_id) #660
  daily2 <- as.data.frame(df[df$text %in% grep("^the.*daily!$", df$text, value = TRUE) ,]$from_user_id) #4283
  latest <- as.data.frame(df[df$text %in% grep("^the latest.*!$", df$text, value = TRUE) ,]$from_user_id)#8600
  ets <- as.data.frame(df[df$from_user_name %like% "ETSKYWARN",]$from_user_id)
  colnames(daily)<-names
  colnames(daily2)<-names
  colnames(latest)<-names
  colnames(ets)<-names
  
  gathered.ids<-rbind(gathered.ids,high,hi,clap,ok,daily,daily2,latest,ets)
}

as.data.frame(unique(gathered.ids$from_user_id)) %>% write.csv(bots.file, quote = FALSE, row.names = FALSE, sep=",")

#Climate: Hi: 46 Lo: 27 Precip: 0.0 Snow: 0.0
#Climate Report: High: 34 Low: 17 Precip: 0.03 Snow: 0.2"