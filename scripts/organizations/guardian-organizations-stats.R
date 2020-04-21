library(topicmodels)
library(ggplot2)
library(textmineR)
library(dplyr)
library(data.table)
source("./scripts/organizations/functions-stats.R")


##################data read
#####guardian comments
data.comments <- read.csv2("./data/guardian/full_comments_guardian.csv", stringsAsFactors = FALSE)
data.comments<-data.comments[!is.na(data.comments$text),]
data.comments$text<-as.character(data.comments$text)
data.comments$text<-tolower(data.comments$text)
data.comments$date<- as.Date(data.comments$date)



data.articles <- read.csv2("./data/guardian/full_articles_guardian.csv", stringsAsFactors = FALSE)
data.articles<-data.articles[!is.na(data.articles$text),]
data.articles$text<-as.character(data.articles$text)
data.articles$text<-tolower(data.articles$text)
data.articles$date_published<-as.numeric(data.articles$date_published)
data.articles$date_published<- as.Date(as.POSIXct((data.articles$date_published/1000), origin = "1970-01-01"))

###adding date of publishing of article (article_id) to each of comments
data.comments<-data.articles %>% 
  select(id,date_published) %>% 
  plyr::rename(c("id"="article_id"))%>%
  merge(data.comments, by="article_id")

#comments with date < date of article that it is commenting on
# wrong_comments<-data.comments[data.comments$date_published>data.comments$date,]###comments to erase 651?

#erasing the comments which are wrongly dated (date of comment < date of article commented)
data.comments<-data.comments[!data.comments$date_published>data.comments$date,]
##################</data read

data.comments$delay<-data.comments$date-data.comments$date_published
max(data.comments$delay)#34
min(data.comments$delay)#0
mean(data.comments$delay)#1.054882
median(data.comments$delay)#1



###number of comments wiht ipcc relative to all comments globally in month
grouped.ipcc <- comments.ipcc %>% 
  count(format(date,'%y-%m')) %>%
  plyr::rename(c("format(date, \"%y-%m\")"="date"))%>%
  plyr::rename(c("n"="ipcc.n"))


grouped.all <- data.comments %>% 
  count(format(date,'%y-%m'))%>%
  plyr::rename(c("format(date, \"%y-%m\")"="date"))%>%
  plyr::rename(c("n"="all.n"))

grouped<-merge(grouped.ipcc,grouped.all, by="date")
grouped$ratio<-grouped$ipcc.n/grouped$all.n

p <- ggplot(grouped, aes(x = date, y = ratio)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Year & month") +
  ylab("Ratio") +
  ggtitle(paste("Ratio ippc comments to all monthly comments"))
p
ggsave(paste(getwd(),"/plots/ipcc/guardian-ippccomments-to-monthly-ratio.pdf", sep=''))



articles.from.ipcc.comments<-data.articles[data.articles$id %in% comments.ipcc$article_id,]

comments.ipcc<-articles.from.ipcc.comments %>% 
  select(id,date_published) %>% 
  plyr::rename(c("id"="article_id"))%>%
  merge(comments.ipcc, by="article_id")

wrong_comments<-comments.ipcc[comments.ipcc$date_published>comments.ipcc$date,]###comments to erase


# org.names<-c("Extinction Rebellion","Citizens Climate Lobby",
#              "CSIRO",
#              "Commonwealth Scientific and Industrial Research Organisation",
#              "Climate Change in Australia","Climatic Research Unit",
#              "Hadley Centre for Climate Prediction and Research",
#              "Union of Concerned Scientists",
#              "Environmental Defense Fund",
#              "Global Change Research Program",
#              "Global Warming Policy Foundation",
#              "350.org",
#              "Yale Program on Climate Change Communication",
#              "IPCC",
#              "Intergovernmental Panel on Climate Change",
#              "Global Atmosphere Watch",
#              "United Nations Framework Convention on Climate Change",
#              "Green Climate Fund",
#              "Natural Resources Defense Council",
#              "Friends of the Earth International",
#              "Canadian Foundation for Climate and Atmospheric Sciences",
#              "IRI",
#              "International Research Institute for Climate and Society")

# org.num.arts<-checkOrganization(org.names, data.articles)
# org.num.coms<-checkOrganization(org.names, data.comments)
org.names<-read.csv("./data/organizations.csv")
stats<-statsOrganizations(org.names,data.articles,data.comments)
stats<-stats%>%arrange(desc(art_nb))
write.csv(stats, "./plots/ipcc/stats-organizations.csv", row.names = FALSE, quote = FALSE)


arts_commented<-getArticlesCommented("ipcc",data.articles,data.comments)
plotTimeline(arts_commented,"date_published","test articles commented by ipcc comments", "test1.pdf")
arts<-getDataByOrganization("IPCC",data.articles)
plotTimeline(arts,"date_published","IPCC articles per month", "ipcc_arts.pdf")
