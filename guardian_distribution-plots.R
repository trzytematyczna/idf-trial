library(timeDate)
library(dplyr)
library(ggplot2)

data <- read.csv2(paste(getwd(),"/data/guardian-articles.csv",sep=''))
names(data)[names(data) == "X_id..oid"] <- "id"
names(data)[names(data) == "date_published..date"] <- "date"
data$date<-as.POSIXct(data$date)

grouped <- data %>% 
  count(format(date,'%y-%V')) 

colnames(grouped)<-c("art_date_published","art_count")
p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + geom_col() 
p + theme(axis.text.x = element_text(angle = 45)) + xlab("Year & week") + ylab("# of articles")
ggsave(paste(getwd(),"/plots/articles-per-week.pdf", sep=''))

grouped <- data %>% 
  count(format(date,'%y-%m')) 

colnames(grouped)<-c("art_date_published","art_count")
p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + geom_col() 
p + theme(axis.text.x = element_text(angle = 45)) + xlab("Year & month") + ylab("# of articles")
ggsave(paste(getwd(),"/plots/articles-per-month.pdf", sep=''))


grouped <- data %>% 
  count(format(date,'%y-%m-%d')) 
colnames(grouped)<-c("art_date_published","art_count")

p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + geom_col() 
p + theme(axis.text.x = element_text(angle = 45)) + xlab("Date") + ylab("# of articles")
ggsave(paste(getwd(),"/plots/articles-per-date.pdf", sep=''))


grouped <- data %>% count(article_section) 

colnames(grouped)<-c("art_section","art_count")

p <- ggplot(grouped, aes(x = art_section, y = art_count)) + geom_col() 
p + theme(axis.text.x = element_text(angle = 90)) + xlab("Section") + ylab("# of articles")
ggsave(paste(getwd(),"/plots/articles-per-section.pdf", sep=''))

#grouped_limited %>% filter(art_count== min(grouped_limited$art_count))

grouped_limited<-grouped[!(grouped$art_section=="Environment"),]

p <- ggplot(grouped_limited, aes(x = art_section, y = art_count)) + geom_col() 
p + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Section") + ylab("# of articles")
ggsave(paste(getwd(),"/plots/articles-per-section-noenv.pdf", sep=''))




distribution_article_section <- function(data, sectionName, saveToFile=FALSE){
  section_data <- data %>% 
    filter(article_section==sectionName) %>% 
    count(format(date,'%y-%m-%d')) 
  colnames(section_data)<-c("art_date_published","art_count")
  
  p <- ggplot(section_data, aes(x = art_date_published, y = art_count)) + 
    geom_col() + 
    theme(axis.text.x = element_text(angle = 45)) + 
    xlab("Date") + ylab("# of articles") +
    ggtitle(paste("Section:", sectionName))
  
  if(saveToFile){
    ggsave(paste("./plots/sectionTEST",sectionName,".pdf", sep=''), plot = last_plot())
  }
  p
}


#####Articles selection
##min number of articles: Children's Books (1) 
##max number of articles: Environment --> 1 from max day  17-01-19 19-04-23
##max number of articles (2nd): Opiniton 19-03-15 
##australia news
#music

selected_articles<-data[data$article_section=="Children's books",]%>%
  rbind(data%>%filter(article_section=="Music")%>% filter(text!="")%>% top_n(1)) %>%
  rbind(data%>%filter(article_section=="Australia news")%>%top_n(1))


section_data <- data %>% 
  filter(article_section=="Environment") %>% 
  count(format(date,'%y-%m-%d')) 
colnames(section_data)<-c("art_date_published","art_count")
env_date<-filter(section_data,art_count== max(section_data$art_count))%>%
  pull(art_date_published)

selected_articles<-rbind(selected_articles, top_n(data%>%
                           filter(format(date,"%y-%m-%d")==env_date[1])%>%
                           filter(article_section=="Environment"),1))

section_data <- data %>% 
  filter(article_section=="Opinion") %>% 
  count(format(date,'%y-%m-%d')) 
colnames(section_data)<-c("art_date_published","art_count")
opin_date<-filter(section_data,art_count== max(section_data$art_count))%>%
  pull(art_date_published)

selected_articles<-rbind(selected_articles, top_n(data%>%
                                                    filter(format(date,"%y-%m-%d")==opin_date[1])%>%
                                                    filter(article_section=="Opinion"),1))


## 17 articles without text length(data%>%filter(text==""))

write.csv2(selected_articles,"./results/guardian_articles_selected.csv", row.names = FALSE)
