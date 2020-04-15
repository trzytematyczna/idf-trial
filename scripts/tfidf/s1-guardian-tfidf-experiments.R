library(lubridate)
source(paste(getwd(),"/tfidf.R", sep=''))
library(ggplot2)
library(ggwordcloud)
library(dplyr)
library(data.table)

csv_data <- read.csv2(paste(getwd(),"/data/full_articles_guardian.csv",sep=''), stringsAsFactors = FALSE)
csv_data<-csv_data[!is.na(csv_data$text),]
selected_articles<-read.csv2("./results/guardian_articles_selected.csv", stringsAsFactors = FALSE)
csv_data$date_published<-as.numeric(csv_data$date_published)
csv_data$date_published<- as.Date(as.POSIXct((csv_data$date_published/1000), origin = "1970-01-01"))


data<-csv_data
tfidf_data <- run_tfidf(data,"id", roots = FALSE)



top_num<-15
##### Document = Single Article, top 15 tf-idf results for all artiles

top_tfidf_data <- tfidf_data %>% group_by(id) %>% arrange(id, desc(tf_idf)) %>% top_n(n=15)
top_tfidf_data <-top_tfidf_data %>% 
  inner_join(data %>% select(date_published, id) %>% distinct(), by = "id") %>%
  arrange(date_published)
top_tfidf_data %>% write.csv2(paste(getwd(),"/results/SingleArticle-Top",top_num,"-noaggreg.csv", sep=""), row.names = FALSE)

### Description

tfidf_data.desc<-run_tfidf.decs(data,"id")

top_tfidf_data.desc <- tfidf_data.desc %>% group_by(id) %>% arrange(id, desc(tf_idf)) %>% top_n(n=15)
top_tfidf_data.desc <-top_tfidf_data.desc %>% 
  inner_join(data %>% select(date_published, id) %>% distinct(), by = "id") %>%
  arrange(date_published)
top_tfidf_data.desc %>% write.csv2(paste(getwd(),"/results/SingleArticle-Desc-Top",top_num,"-noaggreg.csv", sep=""), row.names = FALSE)
selected.SingleArticleNoAggreg.desc<-top_tfidf_data.desc[which(top_tfidf_data.desc$id %in% selected_articles$id),]
####
selected.SingleArticleNoAggreg<-top_tfidf_data[which(top_tfidf_data$id %in% selected_articles$id),]



###
selected_articles_info<-
  top_tfidf_data%>%
  filter(as.character(id) %in% as.character(selected_articles$id))

 selected_articles$singleArtNoAgg<-NA

 aid <- list()
  for(aid in unique(selected_articles$id)){
    words <- selected_articles_info%>%
      group_by(id)%>%
      filter(id==aid)%>%
      select(word)
    article_words<-list(words$word)
    selected_articles<-selected_articles %>% 
      mutate(singleArtNoAgg = replace(singleArtNoAgg, id==aid, values=article_words))
  }

##### Document = Single Article, tf-idf results aggregated by month - top 15 words with highest tfidf score from all articles/month  
tfidf_aggregated <-tfidf_data %>% 
  inner_join(data %>% select(date_published, id) %>% distinct(), by = "id")

top_tfidf_aggregated<-tfidf_aggregated %>% 
  group_by(month=floor_date(date_published, "month")) %>%   
  arrange(month, desc(tf_idf)) %>%
  top_n(top_num,tf_idf)

top_tfidf_aggregated %>% write.csv2(paste(getwd(),"/results/SingleArticle-Top",top_num,"-aggreg-month.csv", sep=""), row.names = FALSE)


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
  inner_join(data %>% select(date_published, id) %>% distinct(), by = "id")

top_tfidf_aggregated_roots<-tfidf_aggregated_roots %>% 
  group_by(month=floor_date(date_published, "month")) %>%   
  arrange(month, desc(tf_idf)) %>%
  top_n(top_num,tf_idf)

top_tfidf_aggregated_roots %>% write.csv2(paste(getwd(),"/results/SingleArticle-Roots-Top",top_num,"-aggreg-month.csv", sep=""), row.names = FALSE)

##### Document=Articles from Month,  tf-idf results
data<-csv_data


data<-data %>% 
  mutate(amonth=floor_date(date_published, "month")) %>%   
  arrange(amonth)

tfidf_data_month <- run_tfidf(data,"amonth", roots = FALSE)

top_monthly<-run_topn(tfidf_data_month, "amonth", top_num)

top_monthly %>% write.csv2(paste(getwd(),"/results/MonthArticles-Top",top_num,"-noaggreg.csv", sep=""), row.names = FALSE)


selected_articles_info<-
  top_monthly%>%
  filter(amonth %in% floor_date(as.Date.character(selected_articles$date_published), unit='month'))

selected_articles$MonthArticles<-NA

mon<-list()
for(mon in unique(floor_date(as.Date.character(selected_articles$date_published), unit='month'))){
  words<-selected_articles_info%>%
    group_by(amonth)%>%
    filter(amonth==mon)%>%
    select(word)
  asd<-list(words$word)
   selected_articles<-selected_articles %>% 
   mutate(MonthArticles = replace(MonthArticles, floor_date(as.Date.character(selected_articles$date_published), unit='month') == mon,
                                    values=asd))
}

# df.selected_articles<-as.data.frame(selected_articles)
df.selected_articles<-data.frame(matrix(ncol = ncol(selected_articles), nrow = nrow(selected_articles)), stringsAsFactors=FALSE)
colnames(df.selected_articles)<-names(selected_articles)
df.selected_articles[1:15]<-selected_articles[1:15]
for(i in 1:nrow(df.selected_articles)){
  df.selected_articles$MonthArticles[i]<-paste(unlist(selected_articles$MonthArticles[i]),collapse=", ", sep="")
  df.selected_articles$singleArtNoAgg[i]<-paste(unlist(selected_articles$singleArtNoAgg[i]), collapse=", ", sep="")
  df.selected_articles$singleArtAggMonth[i]<-paste(unlist(selected_articles$singleArtAggMonth[i]), collapse=", ", sep="")
}


####wordcloud
ggplot(
  selected.SingleArticleNoAggreg,
  aes(label = word, size = tf_idf)
) +
  geom_text_wordcloud_area() +
   scale_size_area(max_size = 15) +
  theme_minimal() +
  facet_wrap(~id)

ggsave("selected-articles_SingleArticleNoAggreg_wordcloud.png", plot = last_plot())


ggplot(
  selected.SingleArticleNoAggreg.desc,
  aes(label = word, size = tf_idf)
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  facet_wrap(~id)

ggsave("selected-articles_SingleArticle-desc-NoAggreg_wordcloud.png", plot = last_plot())


#############greta

##Greta words
text.greta<-data[data$text %like% "Greta Thunberg", ] #98
# data.greta<-rbind(data.greta, data[data$text %like% "Thunberg", ]) # 191 unique 98

descritpion.greta<-data[data$description %like% "Greta Thunberg", ] #14
# descritpion.greta<-rbind(descritpion.greta, data[data$description %like% "Thunberg", ]) # 27 total  unique 14s

grouped <- text.greta %>% 
  count(format(date_published,'%y-%m-%d')) 

colnames(grouped)<-c("art_date_published","art_count")
p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  xlab("Date") + 
  ylab("# of articles") +
  ggtitle(paste("#Greta Articles per date"))
p
ggsave(paste(getwd(),"/plots/greta-per-date.pdf", sep=''))

grouped <- text.greta %>% count(section) 

colnames(grouped)<-c("art_section","art_count")

p <- ggplot(grouped, aes(x = art_section, y = art_count)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Section") +
  ylab("# of greta articles") +
  ggtitle(paste("#Greta Articles per section"))
p
ggsave(paste(getwd(),"/plots/greta-per-serction.pdf", sep=''))
