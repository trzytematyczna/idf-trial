library(lubridate)
source(paste(getwd(),"/tfidf.R", sep=''))

csv_data <- read.csv2(paste(getwd(),"/data/guardian-articles.csv",sep=''))
selected_articles<-read.csv2("./results/guardian_articles_selected.csv")
names(csv_data)[names(csv_data) == "X_id..oid"] <- "id"
names(csv_data)[names(csv_data) == "date_published..date"] <- "date"
csv_data$date<-as.POSIXct(csv_data$date)

data<-csv_data
tfidf_data <- run_tfidf(data,"id", roots = FALSE)

top_num<-15
##### Document = Single Article, top 15 tf-idf results for all artiles

top_tfidf_data <- tfidf_data %>% group_by(id) %>% arrange(id, desc(tf_idf)) %>% top_n(n=15)
top_tfidf_data <-top_tfidf_data %>% 
  inner_join(data %>% select(date, id) %>% distinct(), by = "id") %>%
  arrange(date)
top_tfidf_data %>% write.csv2(paste(getwd(),"/results/SingleArticle-Top",top_num,"-noaggreg.csv", sep=""), row.names = FALSE)

###
selected_articles_info<-
  top_tfidf_data%>%
  filter(as.character(id) %in% as.character(selected_articles$id))

 selected_articles$singleArtNoAgg<-NA

 aid<-list()
  for(aid in unique(selected_articles$id)){
    words<-selected_articles_info%>%group_by(id)%>%filter(id==aid)%>%select(word)
    asd<-list(words$word)
    selected_articles<-selected_articles %>% mutate(singleArtNoAgg = replace(singleArtNoAgg, id==aid,values=asd))
  }

##### Document = Single Article, tf-idf results aggregated by month - top 15 words with highest tfidf score from all articles/month  
tfidf_aggregated <-tfidf_data %>% 
  inner_join(data %>% select(date, id) %>% distinct(), by = "id")

top_tfidf_aggregated<-tfidf_aggregated %>% 
  group_by(month=floor_date(date, "month")) %>%   
  arrange(month, desc(tf_idf)) %>%
  top_n(top_num,tf_idf)

top_tfidf_aggregated %>% write.csv2(paste(getwd(),"/results/SingleArticle-Top",top_num,"-aggreg-month.csv", sep=""), row.names = FALSE)

selected_articles_info<-NULL
selected_articles_info<-
  top_tfidf_aggregated%>%
  filter(as.character(id) %in% as.character(selected_articles$id))

selected_articles$singleArtAggMonth<-NA

aid<-list()
for(aid in unique(selected_articles$id)){
  words<-selected_articles_info%>%group_by(id)%>%filter(id==aid)%>%select(word)
  asd<-list(words$word)
  selected_articles<-selected_articles %>% mutate(singleArtAggMonth = replace(singleArtAggMonth, id==aid,values=asd))
}


##### Document = Single Article, STEMMED tf-idf results aggregated by month - top 15 words with highest tfidf score from all articles/month  
data<-csv_data

tfidf_data_roots <- run_tfidf(data,"id", roots = TRUE)

tfidf_aggregated_roots <-tfidf_data_roots %>% 
  inner_join(data %>% select(date, id) %>% distinct(), by = "id")

top_tfidf_aggregated_roots<-tfidf_aggregated_roots %>% 
  group_by(month=floor_date(date, "month")) %>%   
  arrange(month, desc(tf_idf)) %>%
  top_n(top_num,tf_idf)

top_tfidf_aggregated_roots %>% write.csv2(paste(getwd(),"/results/SingleArticle-Roots-Top",top_num,"-aggreg-month.csv", sep=""), row.names = FALSE)

##### Document =Articles from Month,  tf-idf results
data<-csv_data

data<-data %>% 
  group_by(month=floor_date(date, "month")) %>%   
  arrange(month)

tfidf_data_month <- run_tfidf(data,"month", roots = FALSE)

top_monthly<-run_topn(tfidf_data_month, "month", top_num)

top_monthly %>% write.csv2(paste(getwd(),"/results/MonthArticles-Top",top_num,"-noaggreg.csv", sep=""), row.names = FALSE)



selected_articles_info<-NULL
selected_articles_info<-
  top_monthly%>%
  filter(format(date, "%Y-%m") %in% as.character(selected_articles$id))

selected_articles$singleArtAggMonth<-NA

aid<-list()
for(aid in unique(selected_articles$id)){
  words<-selected_articles_info%>%group_by(id)%>%filter(id==aid)%>%select(word)
  asd<-list(words$word)
  selected_articles<-selected_articles %>% mutate(singleArtAggMonth = replace(singleArtAggMonth, id==aid,values=asd))
}