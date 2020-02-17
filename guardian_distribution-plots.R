library(timeDate)

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


grouped <- data %>% group_by(article_section) %>%   
  count() 

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




distribution_article_section <- function(data, sectionName, save=FALSE){
  print(class(sectionName))
  section_data <- data[data$article_section==sectionName,]
  section_data <- section_data %>% 
    count(format(date,'%y-%m-%d')) 
  colnames(section_data)<-c("art_date_published","art_count")
  
  p <- ggplot(section_data, aes(x = art_date_published, y = art_count)) + geom_col() 
  p + theme(axis.text.x = element_text(angle = 45)) + xlab("Date") + ylab("# of articles") +ggtitle(paste("Section:", sectionName))
  
  # if(save==TRUE){
  #   ggsave(paste(getwd(),"/plots/section",sectionName,".pdf", sep=''), plot = last_plot())
  # }
}


#####Articles selection
##min number of articles: Children's Books (1)
##max number of articles: Environment --> 1 from max day
##max number of articles (2nd): Opiniton
##australia news
#music
selected_articles<-data[data$article_section=="Children's books",]



###section_data%>% filter(art_count== max(section_data$art_count))
