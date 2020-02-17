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
