library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(tidyr)
library(data.table)


csv_data.comments <- read.csv2("./data/guardian/full_comments_guardian.csv", stringsAsFactors = FALSE)
csv_data.comments<-csv_data.comments[!is.na(csv_data.comments$text),]
data.comments<-csv_data.comments
data.comments$text<-as.character(data.comments$text)
data.comments$text<-tolower(data.comments$text)
data.comments$date<- as.Date(data.comments$date)


csv_data.articles <- read.csv2("./data/guardian/full_articles_guardian.csv", stringsAsFactors = FALSE)
csv_data.articles<-csv_data.articles[!is.na(csv_data.articles$text),]
data.articles<-csv_data.articles
data.articles$text<-as.character(data.articles$text)
data.articles$text<-tolower(data.articles$text)
data.articles$date_published<-as.numeric(data.articles$date_published)
data.articles$date_published<- as.Date(as.POSIXct((data.articles$date_published/1000), origin = "1970-01-01"))


##########
data.comments<-data.articles %>% 
  select(id,date_published) %>% 
  plyr::rename(c("id"="article_id"))%>%
  merge(data.comments, by="article_id")

wrong_comments<-data.comments[data.comments$date_published>data.comments$date,]###comments to erase 651?

data.comments<-data.comments[!data.comments$date_published>data.comments$date,]
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

comments.ipcc<-comments.ipcc[!comments.ipcc$date_published>comments.ipcc$date,]
comments.ipcc$delay<-comments.ipcc$date-comments.ipcc$date_published
max(comments.ipcc$delay)#10
min(comments.ipcc$delay)#0
mean(comments.ipcc$delay)#1.788903
median(comments.ipcc$delay)#1

# top3 <- comments.ipcc %>%
#   arrange(desc(delay))%>%
#   top_n(3)

comments.ippc.per.article<-comments.ipcc%>%
  group_by(article_id)%>%
  count()%>%
  arrange(desc(n))

selected.article.max.comments.ipcc<-comments.ipcc[comments.ipcc$article_id=="5cc082dca2c3613145ed8fd4",]

cnt <-selected.article.max.comments.ipcc %>%
  group_by(date)%>%
  count()
  
  


plotTimeline<-function(data, dates_name, title, filename, isWeekly = FALSE){

  if(isWeekly == TRUE){
    grouped <- data %>% 
      count(format(!!!sym(dates_name),'%y-%V'))  #date_published = articles, date = comments
    xlab_name = "Year and week"
  }else{
    grouped <- data %>% 
      count(format(!!!sym(dates_name),'%y-%m'))  #date_published = articles, date = comments
    xlab_name = "Year and month"
  }
  
  colnames(grouped)<-c("date","nb")
  p <- ggplot(grouped, aes(x = date, y = nb)) + 
    geom_col() +
    theme(axis.text.x = element_text(angle = 90)) + 
    xlab(xlab_name) +
    ylab("#") +
    ggtitle(title)
  ggsave(filename)
 
}




checkOrganization<-function(organizations, data){
  organizations<-tolower(organizations)
  df<-data.frame(organization_name=character(0), art_nb=numeric(0), stringsAsFactors=FALSE)
    for (i in 1:length(organizations)){
      ndf<-data.frame(organization_name=organizations[i],
                      art_nb=nrow(data[data$text %like% organizations[i], ]))
      df<-rbind(df,ndf)
    }
  colnames(df)<-c("organization_name","art_nb")
  df
}


org.names<-c("Extinction Rebellion","Citizens Climate Lobby",
             "CSIRO",
             "Commonwealth Scientific and Industrial Research Organisation",
             "Climate Change in Australia","Climatic Research Unit",
             "Hadley Centre for Climate Prediction and Research",
             "Union of Concerned Scientists",
             "Environmental Defense Fund",
             "Global Change Research Program",
             "Global Warming Policy Foundation",
             "350.org",
             "Yale Program on Climate Change Communication",
             "IPCC",
             "Global Atmosphere Watch",
             "United Nations Framework Convention on Climate Change",
             "Green Climate Fund",
             "Natural Resources Defense Council",
             "Friends of the Earth International",
             "Canadian Foundation for Climate and Atmospheric Sciences",
             "IRI",
             "International Research Institute for Climate and Society")


getDataByOrganization <- function(organization,data){
  organization<-tolower(organization)
  things<-data[data$text %like% organization, ]
  things
}

getCommentsBelow <- function(organization, adata, cdata){
  organization<-tolower(organization)
  arts<-getDataByOrganization(organization, adata)
  commentsBelowOrgArticles <- cdata[cdata$article_id %in% arts$id,]
  commentsBelowOrgArticles
}
getArticlesCommented <- function(organization, adata, cdata){
  organization<-tolower(organization)
  coms<-getDataByOrganization(organization, cdata)
  articlesCommentedOrgComments <- adata[adata$id %in% coms$article_id,]
  articlesCommentedOrgComments
}

countCommentsBelow <- function(organization, adata, cdata){
  organization<-tolower(organization)
  arts<-getDataByOrganization(organization, adata)
  count_commentsBelowOrgArticles <- nrow(cdata[cdata$article_id %in% arts$id,]) 
  count_commentsBelowOrgArticles
}
countArticlesCommented <- function(organization, adata, cdata){
  organization<-tolower(organization)
  coms<-getDataByOrganization(organization, cdata)
  count_articlesCommentedOrgComments <- nrow(adata[adata$id %in% coms$article_id,]) 
  count_articlesCommentedOrgComments
}

statsOrganizations<-function(organizations, adata,cdata){
  organizations<-tolower(organizations)
  df<-data.frame()
  for (i in 1:length(organizations)){
    ndf<-data.frame(organization_name=organizations[i],
                    art_nb=nrow(adata[adata$text %like% organizations[i], ]),
                    comm_nb =nrow(cdata[cdata$text %like% organizations[i], ]),
                    comm_below_art = countCommentsBelow(organizations[i],adata,cdata),
                    art_commented = countArticlesCommented(organizations[i],adata,cdata)
                      )
    df<-rbind(df,ndf)
  }
  df<-df%>%arrange(desc(art_nb))
  df
}
# org.num.arts<-checkOrganization(org.names, data.articles)
# org.num.coms<-checkOrganization(org.names, data.comments)

arts_commented<-getArticlesCommented("ipcc",data.articles,data.comments)
plotTimeline(arts_commented,"date_published","test articles commented by ipcc comments", "test1.pdf")
stats<-statsOrganizations(org.names,data.articles,data.comments)
arts<-getDataByOrganization("IPCC",data.articles)
plotTimeline(arts,"date_published","IPCC articles per month", "ipcc_arts.pdf")


# ##########organizations
# nrow(data.articles[data.articles$text %like% "Extinction Rebellion", ])  # 107
# nrow(data.articles[data.articles$text %like% "Citizens Climate Lobby", ]) #2
# nrow(data.articles[data.articles$text %like% "Commonwealth Scientific and Industrial Research Organisation ", ]) #2
# nrow(data.articles[data.articles$text %like% "CSIRO", ]) #111
# nrow(data.articles[data.articles$text %like% "Climate Change in Australia", ]) #3
# nrow(data.articles[data.articles$text %like% "Climatic Research Unit", ]) # 4
# nrow(data.articles[data.articles$text %like% "Hadley Centre for Climate Prediction and Research", ]) #0
# nrow(data.articles[data.articles$text %like% "Union of Concerned Scientists", ]) #44
# nrow(data.articles[data.articles$text %like% "Environmental Defense Fund", ]) #17
# nrow(data.articles[data.articles$text %like% "Global Change Research Program", ]) #5
# nrow(data.articles[data.articles$text %like% "Global Warming Policy Foundation", ]) # 24
# nrow(data.articles[data.articles$text %like% "350.org", ]) # 83
# nrow(data.articles[data.articles$text %like% "Yale Program on Climate Change Communication", ]) #5
# nrow(data.articles[data.articles$text %like% "United Nations Framework Convention on Climate Change", ]) #20
# nrow(data.articles[data.articles$text %like% "Green Climate Fund", ]) # 33
# nrow(data.articles[data.articles$text %like% "COP21", ]) # 31
# nrow(data.articles[data.articles$text %like% "COP22", ]) # 18
# nrow(data.articles[data.articles$text %like% "COP23", ]) # 18
# nrow(data.articles[data.articles$text %like% "COP24", ]) # 21
# nrow(data.articles[data.articles$text %like% "COP25", ]) # 2
# nrow(data.articles[data.articles$text %like% "COP", ]) # 103
# nrow(data.articles[data.articles$text %like% "IUCN World Conservation Congress", ]) # 1
# nrow(data.articles[data.articles$text %like% "World Conservation Congress", ]) # 1
# nrow(data.articles[data.articles$text %like% "Climate Action Summit", ]) #12
# nrow(data.articles[data.articles$text %like% "InsideClimate News", ]) #2
# nrow(data.articles[data.articles$text %like% "Inside Climate News", ]) # 8
# nrow(data.articles[data.articles$text %like% "Friends of the Earth International", ]) #3 
# nrow(data.articles[data.articles$text %like% "Natural Resources Defense Council", ]) # 21
# nrow(data.articles[data.articles$text %like% "Environmental Defense Fund", ]) #17
# 
# comments.ipcc <- data.comments[data.comments$text %like% "ipcc", ] %>%
#   rbind(data.comments[data.comments$text %like% "IPCC", ])
# comments.ipcc <- comments.ipcc %>% distinct(id, .keep_all = TRUE)
# length(unique(comments.ipcc$article_id)) #1216
# 
# comments.ipcc <- data.comments[data.comments$text %like% "IPCC", ] %>% 
#   rbind(data.comments[data.comments$text %like% "ipcc", ]) %>% 
#   rbind(data.comments[data.comments$text %like% "Intergovernmental Panel on Climate Change", ]) %>% 
#   rbind(data.comments[data.comments$text %like% "intergovernmental panel on climate change", ]) %>% 
#   rbind(data.comments[data.comments$text %like% "UN Environment Programme", ]) %>%
#   distinct(id, .keep_all = TRUE)
# length(unique(comments.ipcc$id)) #4838
# 
# length(unique(comments.ipcc$article_id)) #1226

# 
# grouped <- comments.ipcc %>% 
#   count(format(date,'%y-%m')) 
# 
# colnames(grouped)<-c("com_date","count")
# p <- ggplot(grouped, aes(x = com_date, y = count)) + 
#   geom_col() +
#   theme(axis.text.x = element_text(angle = 90)) + 
#   xlab("Year & month") +
#   ylab("# of comments") +
#   ggtitle(paste("#IPCC Comments per month"))
# p
# ggsave(paste(getwd(),"/plots/ipcc/ipcc-per-month_comments.pdf", sep=''))



# 
# grouped <- articles.ipcc %>% 
#   count(format(date_published,'%y-%m')) 
# 
# 
# colnames(grouped)<-c("art_date_published","art_count")
# p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + 
#   geom_col() +
#   theme(axis.text.x = element_text(angle = 90)) + 
#   xlab("Year & month") +
#   ylab("# of articles") +
#   ggtitle(paste("#IPCC Articles per month"))
# p
# ggsave(paste(getwd(),"/plots/ipcc/ipcc-per-month.pdf", sep=''))
# 
# articles.ipcc <- data.articles[data.articles$text %like% "IPCC", ] %>% 
#   rbind(data.articles[data.articles$text %like% "ipcc", ]) %>%
#   rbind(data.articles[data.articles$text %like% "intergovernmental panel on climate change", ]) %>%
#   rbind(data.articles[data.articles$text %like% "Intergovernmental Panel on Climate Change", ]) %>%
#   rbind(data.articles[data.articles$text %like% "UN Environment Programme", ]) %>%
#   distinct(id, .keep_all = TRUE)
# length(unique(articles.ipcc$id)) #352
# 
# 
# articles.ipcc.commented.with.ipcc.comment <- articles.ipcc[articles.ipcc$id %in% comments.ipcc$article_id,] #145
# comments.ipcc.below.articles.ipcc <- comments.ipcc[comments.ipcc$article_id %in% articles.ipcc$id,] #145
# 
# grouped <- articles.from.ipcc.comments %>% 
#   count(format(date_published,'%y-%m')) 
# 
# colnames(grouped)<-c("art_date_published","art_count")
# p <- ggplot(grouped, aes(x = art_date_published, y = art_count)) + 
#   geom_col() +
#   theme(axis.text.x = element_text(angle = 90)) + 
#   xlab("Year & month") +
#   ylab("# of articles") +
#   ggtitle(paste("#Articles commented - comments mentioning IPCC (per month)"))
# p
# ggsave(paste(getwd(),"/test_ipcc-per-month_articles-from-comments.pdf", sep=''))
