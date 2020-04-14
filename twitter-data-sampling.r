library(dplyr)
library(data.table)


max.data<-2000000

data_name<-"twitter-2M"
data_dir<-"./data/twitter/split-2M/"

data.files <- list.files(data_dir)
sampled.file <- paste0(data_dir, data_name, "-sampled.csv")
nfiles<-length(data.files)
lines.per.file<-round(max.data/nfiles)


sampled<-data.frame()
for (i in data.files){
  print(i)
  df <- read.csv2(paste0(data_dir,i), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8")
                  #colClasses = c("factor","character"))
  df<-df[!is.na(df$text),] #empty tweets
  # df<-df[!df$from_user_id=="34939003",] #meteo tweets from user id = 34939003 name =  iembot_lox
  #iembot_mlb
  df<-df[!df$from_user_name %like% "iembot_",]
  sampled <- rbind(sampled,sample_n(df, lines.per.file))
}

sampled %>% write.csv2(sampled.file, quote = FALSE, row.names = FALSE)