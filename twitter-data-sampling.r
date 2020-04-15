library(dplyr)
library(data.table)


max.data<-1000000

data_name<-"twitter-1M"
data_dir<-"./data/twitter/split-1M/"

data.files <- list.files(data_dir)
sampled.file <- paste0(data_dir, data_name, "-sampled.csv")
nfiles<-length(data.files)
lines.per.file<-round(max.data/nfiles)


sampled<-data.frame()
for (i in data.files){
  print(i)
  df <- read.csv(paste0(data_dir,i), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8")
                  #colClasses = c("factor","character"))
  df<-df[!is.na(df$text),] #empty tweets
  # df<-df[!df$from_user_name %like% "iembot_",]
  # df<-df[!df$from_user_name %like% "WxBotUSA",]
  df<-df[!df$text %like% "Climate Report: High: ",] #61770
  df<-df[!df$text %like% "Climate: Hi: ",] #96986
  df<-df[!df$from_user_name %like% "ClapRobot",]
  df<-df[!df$from_user_name %like% "OKCStormWatcher",]
  
  new.sample <- sample_n(df, lines.per.file)
  
  sampled <- rbind(sampled,new.sample)
}

# sampled %>% write.table(sampled.file, quote = TRUE, row.names = FALSE, sep=",")
sampled %>% arrange(date)%>% fwrite(sampled.file, quote = TRUE, row.names = FALSE, sep=",", qmethod = "double")

#Climate: Hi: 46 Lo: 27 Precip: 0.0 Snow: 0.0
#Climate Report: High: 34 Low: 17 Precip: 0.03 Snow: 0.2"