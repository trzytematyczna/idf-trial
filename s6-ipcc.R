library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(wordcloud)
library(tidyr)
library(data.table)
library(tidyverse)


csv_data.comments <- read.csv2("./data/full_comments_guardian.csv", stringsAsFactors = FALSE)
csv_data.comments<-csv_data.comments[!is.na(csv_data.comments$text),]
data.comments<-csv_data.comments
data.comments$text<-as.character(data.comments$text)

csv_data.articles <- read.csv2("./data/full_articles_guardian.csv", stringsAsFactors = FALSE)
csv_data.articles<-csv_data.articles[!is.na(csv_data.articles$text),]
data.articles<-csv_data.articles
data.articles$text<-as.character(data.articles$text)


# nrow(data.comments[data.comments$text %like% "Greta Thunberg", ]) #51
# nrow(data.comments[data.comments$text %like% "Greta", ]) #126
# nrow(data.comments[data.comments$text %like% "greta", ]) #9
# nrow(data.comments[data.comments$text %like% "ipcc", ]) # 493
# nrow(data.comments[data.comments$text %like% "IPCC", ]) # 4507


# comments.greta <- data.comments[data.comments$text %like% "Greta Thunberg", ] %>% 
#   rbind(data.comments[data.comments$text %like% "Greta", ]) %>%
#   rbind(data.comments[data.comments$text %like% "greta", ])
# comments.greta <- comments.greta %>% distinct(id, .keep_all = TRUE)
# length(unique(comments.greta$article_id)) #47


# comments.ipcc <- data.comments[data.comments$text %like% "ipcc", ] %>%
#   rbind(data.comments[data.comments$text %like% "IPCC", ])
# comments.ipcc <- comments.ipcc %>% distinct(id, .keep_all = TRUE)
# length(unique(comments.ipcc$article_id)) #1216

comments.ipcc <- data.comments[data.comments$text %like% "IPCC", ] %>% 
  rbind(data.comments[data.comments$text %like% "ipcc", ]) %>% 
  rbind(data.comments[data.comments$text %like% "Intergovernmental Panel on Climate Change", ]) %>% 
  rbind(data.comments[data.comments$text %like% "intergovernmental panel on climate change", ]) %>% 
  rbind(data.comments[data.comments$text %like% "UN Environment Programme", ]) %>%
  distinct(id, .keep_all = TRUE)
length(unique(comments.ipcc$id)) #4838

length(unique(comments.ipcc$article_id)) #1226


# comments.ipcc$date<-as.numeric(comments.ipcc$date)
comments.ipcc$date<- as.Date(comments.ipcc$date)

grouped <- comments.ipcc %>% 
  count(format(date,'%y-%m')) 

colnames(grouped)<-c("com_date","count")
p <- ggplot(grouped, aes(x = com_date, y = count)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Year & month") +
  ylab("# of comments") +
  ggtitle(paste("#IPCC Comments per month"))
p
ggsave(paste(getwd(),"/plots/ipcc/ipcc-per-month_comments.pdf", sep=''))


articles.from.ipcc.comments<-data.articles[data.articles$id %in% comments.ipcc$article_id,]

articles.from.ipcc.comments$date_published<-as.numeric(articles.from.ipcc.comments$date_published)
articles.from.ipcc.comments$date_published<- as.Date(as.POSIXct((articles.from.ipcc.comments$date_published/1000), origin = "1970-01-01"))

grouped <- articles.from.ipcc.comments %>% 
  count(format(date_published,'%y-%m')) 

colnames(grouped)<-c("art_date_published","art_count")
p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Year & month") +
  ylab("# of articles") +
  ggtitle(paste("#Articles commented - comments mentioning IPCC (per month)"))
p
ggsave(paste(getwd(),"/plots/ipcc/ipcc-per-month_articles-from-comments.pdf", sep=''))



#####articles 
#?  UN Environment Programme and the World Meteorological Organisation (WMO)
#? World Climate Research Program

# nrow(data.articles[data.articles$text %like% "Greta Thunberg", ]) #92
# nrow(data.articles[data.articles$text %like% "Thunberg", ]) #93
# ##
# nrow(data.articles[data.articles$text %like% "IPCC", ]) # 234
# nrow(data.articles[data.articles$text %like% "UN Environment Programme", ]) #13


articles.ipcc <- data.articles[data.articles$text %like% "IPCC", ] %>% 
  rbind(data.articles[data.articles$text %like% "ipcc", ]) %>%
  rbind(data.articles[data.articles$text %like% "intergovernmental panel on climate change", ]) %>%
  rbind(data.articles[data.articles$text %like% "Intergovernmental Panel on Climate Change", ]) %>%
  rbind(data.articles[data.articles$text %like% "UN Environment Programme", ]) %>%
  distinct(id, .keep_all = TRUE)
length(unique(articles.ipcc$id)) #352


articles.ipcc$date_published<-as.numeric(articles.ipcc$date_published)
articles.ipcc$date_published<- as.Date(as.POSIXct((articles.ipcc$date_published/1000), origin = "1970-01-01"))

grouped <- articles.ipcc %>% 
  count(format(date_published,'%y-%m')) 

colnames(grouped)<-c("art_date_published","art_count")
p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Year & month") +
  ylab("# of articles") +
  ggtitle(paste("#IPCC Articles per month"))
p
ggsave(paste(getwd(),"/plots/ipcc/ipcc-per-month.pdf", sep=''))



