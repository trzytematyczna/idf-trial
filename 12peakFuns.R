library(dplyr)
# Parameter propositions for probability peaks in timeline
# Selection of tweet(s) with max number of retweets
# Selection of tweet(s) from person with max followers
# Global
# In this peak
# #Tweets in timeslot
# #Retweets in timeslot
# #Users tweeting (spread of audience)
# Max # of tweets from the same user (selection of tweets?)
# Plot #users vs #tweets?
# Louvain groups for the timeslot
# Highlighted groups in the topic?
#   #usage of “climate change”
#   Most used hashtag (other than climatechange?)
# Tfidf of tweets in peak ?
  
UserMaxFollowersGlobally<-function(){
  
}

#what = string -> from_user_id = id user, from_user_name = name user, id= tweets of user

UserMaxFollowersPeak<-function(topn, what, all = F){
  
  tweet.tops.what <- peak.df %>% 
    top_n(from_user_followercount, n=topn) %>% 
    pull(!!sym(what))

#return all 
  if(all){
    res <- peak.df %>% 
      filter(!!sym(what) %in% tweet.tops.what)
    res  
#return only what column
  }else{
    res<-tweet.tops.what
    res
  }
}
UserMaxRetweetsGlobally<-function(){
  
}
UserMaxRetweetsPeak<-function(topn, what, all = F){
  
  tweet.tops.what <- peak.df %>% 
    top_n(retweetcount, n=topn) %>% 
    pull(!!sym(what))
  
  #return all 
  if(all){
    res <- peak.df %>% 
      filter(!!sym(what) %in% tweet.tops.what)
    res  
    #return only what column
  }else{
    res<-tweet.tops.what#?%>%unique()
    res
  }
}

TweetsNumber<-function(){
  # asd<-peak.df[duplicated(peak.df$id),] duplicates ?
  length(unique(peak.df$id))
}

RetweetsNumber<-function(){
  sum(peak.df$retweetcount)
}

UsersTweetingNumber<-function(){
  length(unique(peak.df$from_user_id))  
}

UsersRetweetingNumber<-function(){
  length(unique(peak.df[peak.df$retweetcount>0,]$from_user_id))  
}

MaxNumberTweetsFromSameUser<-function(topn, what, all = F){
  users.tops.what <- peak.df %>% 
    group_by(from_user_id) %>%
    summarise(tweetnb=n())%>%
    arrange(desc(tweetnb))%>%
    top_n(tweetnb, n=topn)
  
  res <- peak.df[peak.df$from_user_id %in% users.tops.what$from_user_id,]
  
  #return all 
  if(all){
    res  
    #return only what column
  }else{
    res<-res%>%
      pull(!!sym(what))%>%
      unique()
    res
  }
  
}


data_dir<-"./results/twitter-trained/sorted/"
m<-1
filename<-paste0("sorted-assign-",m,".csv")

res_dir<-"./results/twitter-trained/asd/"
df <- read.csv(paste0(data_dir,filename), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8", na.strings = NA)
df$date<-as.Date(df$date)
colnames(df)<-c("id","date","retweetcount","from_user_id","from_user_name","from_user_followercount","text","t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")

# cols<-c("t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
selected.topic<-c("t_5")
df<-df%>%filter_at(cols, any_vars(.>0.5))


dfrom<-as.Date('2016-07-28')
dto<-as.Date('2016-08-28')
cols<-c("date")
peak.df<-df%>%filter_at(cols, any_vars(between(.,dfrom,dto)))


divide.by<-'%y-%U'

# weeknb<-as.Date('16-19', divide.by)
# cols<-c("month")
peak.df<- df%>% mutate(month=format(date, divide.by))
peak.df<-peak.df[peak.df$month=="16-19",]

asd<-UserMaxFollowersPeak(5,'id',T)     
asd<-UserMaxRetweetsPeak(5,'id',T)     
TweetsNumber()
RetweetsNumber()
UsersTweetingNumber()
UsersRetweetingNumber()
