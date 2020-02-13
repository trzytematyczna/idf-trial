library(lubridate)
source(paste(getwd(),"/tfidf.R", sep=''))

csv_data <- read.csv2(paste(getwd(),"/data/guardian-articles.csv",sep=''))
data<-csv_data
names(data)[names(data) == "X_id..oid"] <- "id"
names(data)[names(data) == "date_published..date"] <- "date"
tfidf_data <- run_tfidf(data,"id", roots = TRUE)

# top_data <- run_topn(tfidf_data, "id", 5)

data <-tfidf_data %>% 
  inner_join(data %>% select(date, id) %>% distinct(), by = "id")

data$date<-as.POSIXct(data$date)

top_num<-15
top_n_per_month<-data %>% 
  group_by(month=floor_date(date, "month")) %>%   
  arrange(month, desc(tf_idf)) %>%
  top_n(top_num,tf_idf)


top_n_per_month %>% write.csv2(paste(getwd(),"/results/guardian_monthly_top_",top_num,".csv", sep=""))

top_n_per_month %>% write.csv2(paste(getwd(),"/results/roots_guardian_monthly_top_",top_num,".csv", sep=""))

