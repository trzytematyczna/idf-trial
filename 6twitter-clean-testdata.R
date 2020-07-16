library(dplyr)
library(data.table)
library(stringr)
library(readr)

#### get rid of weather bots-like tweets from train data before LDA prediction
### Example: Climate: Hi: 46 Lo: 27 Precip: 0.0 Snow: 0.0
### Example: Climate Report: High: 34 Low: 17 Precip: 0.03 Snow: 0.2"

data_name<-"twitter-500K"
data_dir<-"./data/twitter/split-500K/"

data.files <- list.files(data_dir)
nfiles<-length(data.files)
# learning.data.file<-"./data/twitter/split-2M/twitter-2M-sampled.csv"
res_dir<-"./data/twitter/test-data-500K/"

# res<-data.frame()
for (i in data.files){
  print(i)
  df<-read_csv(paste0(data_dir,i), col_types = cols (id = col_character()))
  
  newdf <- df %>% 
    filter(text!="", #emty tweets
           !grepl("Climate Report: High: ", text), #bots
           !grepl("Climate: Hi: ", text),
           !grepl("climate report: high: ", text),
           !grepl("climate: hi: ", text),
           !grepl("ClapRobot", text),
           !grepl("OKCStormWatcher", text),
           !grepl("daily is out!", text),
           !grepl("^the.*daily!$", text),
           !grepl("^the latest.*!$", text)
    )
  #learning data used for LDA -- uncomment if you dont want training data in test sample
  # learning.data <- read.csv(paste0(learning.data.file), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8")
  # df<-df[!df$id %in% learning.data$id,]

  
  filename<-paste0(str_replace(i,".csv",""),"-testsample.csv")
  newdf%>%write_csv(paste0(res_dir,filename))
}


