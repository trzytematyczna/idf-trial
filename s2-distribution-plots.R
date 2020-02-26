library(timeDate)
library(dplyr)
library(ggplot2)

data <- read.csv2("./data/full_articles_guardian.csv", stringsAsFactors = FALSE)
data$date_published<-as.numeric(data$date_published)
data$date_published<- as.Date(as.POSIXct((data$date_published/1000), origin = "1970-01-01"))

grouped <- data %>% 
  count(format(date_published,'%y-%V')) 

colnames(grouped)<-c("art_date_published","art_count")
p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  xlab("Year & week") + 
  ylab("# of articles") +
  ggtitle(paste("#Articles per week"))
p
ggsave(paste(getwd(),"/plots/articles-per-week.pdf", sep=''))v

grouped <- data %>% 
  count(format(date_published,'%y-%m')) 

colnames(grouped)<-c("art_date_published","art_count")
p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45)) + 
  xlab("Year & month") +
  ylab("# of articles") +
  ggtitle(paste("#Articles per month"))
p
ggsave(paste(getwd(),"/plots/articles-per-month.pdf", sep=''))


grouped <- data %>% 
  count(format(date_published,'%y-%m-%d')) 
colnames(grouped)<-c("art_date_published","art_count")

p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab("Date") + 
  ylab("# of articles") +
  ggtitle(paste("#Articles per date_published"))
ggsave(paste(getwd(),"/plots/articles-per-date.pdf", sep=''))


grouped <- data %>% count(section) 

colnames(grouped)<-c("art_section","art_count")

p <- ggplot(grouped, aes(x = art_section, y = art_count)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Section") +
  ylab("# of articles") +
  ggtitle(paste("#Articles per section"))

ggsave(paste(getwd(),"/plots/articles-per-section.pdf", sep=''))

#grouped_limited %>% filter(art_count== min(grouped_limited$art_count))

grouped_limited<-grouped[!(grouped$art_section=="Environment"),]

p <- ggplot(grouped_limited, aes(x = art_section, y = art_count)) + 
  geom_col()+
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Section") + 
  ylab("# of articles") +
  ggtitle(paste("#Articles per section (no Environment"))
ggsave(paste(getwd(),"/plots/articles-per-section-noenv.pdf", sep=''))




distribution_article_section <- function(data, sectionName, saveToFile=FALSE){
  section_data <- data %>% 
    filter(section==sectionName) %>% 
    count(format(date_published,'%y-%m-%d')) 
  colnames(section_data)<-c("art_date_published","art_count")
  
  p <- ggplot(section_data, aes(x = art_date_published, y = art_count)) + 
    geom_col() + 
    theme(axis.text.x = element_text(angle = 45)) + 
    xlab("Date") + ylab("# of articles") +
    ggtitle(paste("#Articles in Section ", sectionName))
  
  if(saveToFile){
    ggsave(paste("./plots/section-distr-",sectionName,".pdf", sep=''), plot = last_plot())
  }
  p
}


#####Articles selection
##min number of articles: Children's Books (1) 
##max number of articles: Environment --> 1 from max day  17-01-19 19-04-23
##max number of articles (2nd): Opiniton 19-03-15 
##australia news
#music

selected_articles<-data[data$section=="Children's books",]%>%
  rbind(data%>%filter(section=="Music")%>% filter(text!="")%>% top_n(1)) %>%
  rbind(data%>%filter(section=="Australia news")%>%top_n(1))


section_data <- data %>% 
  filter(section=="Environment") %>% 
  count(format(date_published,'%y-%m-%d')) 
colnames(section_data)<-c("art_date_published","art_count")
env_date<-filter(section_data,art_count== max(section_data$art_count))%>%
  pull(art_date_published)

selected_articles<-rbind(selected_articles, top_n(data%>%
                           filter(format(date_published,"%y-%m-%d")==env_date[1])%>%
                           filter(section=="Environment"),1))

section_data <- data %>% 
  filter(section=="Opinion") %>% 
  count(format(date_published,'%y-%m-%d')) 
colnames(section_data)<-c("art_date_published","art_count")
opin_date<-filter(section_data,art_count== max(section_data$art_count))%>%
  pull(art_date_published)

selected_articles<-rbind(selected_articles, top_n(data%>%
                                                    filter(format(date_published,"%y-%m-%d")==opin_date[1])%>%
                                                    filter(section=="Opinion"),1))


## 17 articles without text length(data%>%filter(text==""))

write.csv2(selected_articles,"./results/guardian_articles_selected.csv", row.names = FALSE)
