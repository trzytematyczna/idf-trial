library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# data_name<-"twitter-2M"

with.retweets<-T

  divide.by <- '%Y-%m-%d'
  xlabel<-'day'
  

topics.labs.fun<-function(labfilename){
  topic.labels<-read.csv(labfilename)
  topic.labels<-topic.labels%>% 
    group_by(topic) %>%
    summarise(s=paste(word, collapse = ", "))
  
  topic.labels<- topic.labels%>%
    group_by(topic) %>%
    summarize(label=paste(s,topic, collapse = ","))
}

col <- c("#CC6666", "#9999CC", "#66CC99") # colors for plots

topic.labels<-topics.labs.fun("./results/twitter-trained/k-9-topic-words.csv")
global.means.rt<-read.csv2("./results/twitter-trained/k9-global-means-retweets.csv")
global.means.rt <- global.means.rt %>% mutate(topic=factor(topic,levels = 1:9,labels=topic.labels$label))
global.means<-read.csv2("./results/twitter-trained/k9-global-means.csv")
global.means <- global.means %>% mutate(topic=factor(topic,levels = 1:9,labels=topic.labels$label))

data_dir<-"./results/twitter-trained/sorted/"

options <- commandArgs(trailingOnly = TRUE)
m<-options[1]
filename<-paste0("sorted-assign-",m,".csv")
# learning.data.file<-"./data/twitter/split-2M/twitter-2M-sampled.csv"
res_dir<-"./results/twitter-trained/asd/"
# data.files<-data.files[1:2]
# i<-data.files[1]
df <- read.csv(paste0(data_dir,filename), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8", na.strings = NA)
df$date<-as.Date(df$date)
colnames(df)<-c("id","date","retweetcount","from_user_id","from_user_name","from_user_followercount","text","t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
# df$date<-as_date(df$date)
df<-df[2000000:3000000,]

if(with.retweets==T){
  probs<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
    pivot_longer(names_to="topic", values_to="probability", c(t_1:t_9)) %>% ##topic_number k_list
    tidyr::separate(topic, into =c("t","topic")) %>%
    select(-t)%>%
    select(-id)
  
  
  probs2<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
    mutate(month=format(date, divide.by))%>%
    pivot_longer(names_to="topic", values_to="probability", c(t_1:t_9)) %>% ##topic_number k_list
    tidyr::separate(topic, into =c("t","topic")) %>%
    select(-t)%>%
    select(-id,-date)
  
  
  probs <- probs %>%
    mutate(nbtweet=retweetcount+1) %>%
    mutate(mprob=probability*nbtweet)

  probs2 <- probs2 %>%
    mutate(nbtweet=retweetcount+1) %>%
    mutate(mprob=probability*nbtweet)
  
  # plot of ptobability in time of tweets and retweets
  
  grouped.tweets.retweets <- probs %>%
    group_by(date,topic) %>%
    summarise(sum_probability=sum(mprob)/sum(nbtweet))
  
  grouped.tweets.retweets2 <- probs2 %>%
    group_by(month,topic) %>%
    summarise(sum_probability=sum(mprob)/sum(nbtweet))
  
  grouped.tweets.retweets<-grouped.tweets.retweets%>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
  grouped.tweets.retweets2<-grouped.tweets.retweets2%>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
  g<-ggplot(grouped.tweets.retweets, aes(x=date,y=sum_probability))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(grouped.tweets.retweets$sum_probability)+0.05))+
    geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
    geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle("Probability of tweets and retweets together")+
    xlab(xlabel)+
    ylab("Probability")+
    # scale_x_discrete(breaks = xbreaks)+
    facet_wrap(.~topic, ncol=2)
  g
  g2<-ggplot(grouped.tweets.retweets2, aes(x=month,y=sum_probability))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(grouped.tweets.retweets$sum_probability)+0.05))+
    geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
    geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle("Probability of tweets and retweets together")+
    xlab(xlabel)+
    ylab("Probability")+
    # scale_x_discrete(breaks = xbreaks)+
    facet_wrap(.~topic, ncol=2)
  g2
  ggsave(paste0(res_dir,"k9-", xlabel, "-tweets-retweets",m,".pdf"), device = "pdf")
  
  
  test1<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
    pivot_longer(names_to="topic", values_to="probability", c(t_1:t_9)) %>% ##topic_number k_list
    tidyr::separate(topic, into =c("t","topic")) %>%
    select(-t)#%>%
     #select(-id)
  rt.top.test1<-test1[test1$retweetcount>0,]

  # rt.top.test1 <- rt.top.test1 %>%
    # mutate(nbtweet=retweetcount+1) %>%
    # mutate(mprob=probability*nbtweet)

  g.rt.test1<- rt.top.test1 %>%
    select(-retweetcount)%>%
    group_by(date,topic) %>%
    summarise(sump=sum(probability),nb=n())
  
  plot.rt.test1<-g.rt.test1%>%
    mutate(sum_probability=sump/nb)
  
  plot.rt.test1 <- plot.rt.test1 %>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
  g<-ggplot(plot.rt.test1, aes(x=date,y=sum_probability))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(g.rt$sum_probability)+0.05))+
    geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
    geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
    # geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[3])+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(paste0("Probability of retweets only (rt>1)"))+
    xlab(xlabel)+
    ylab("Probability")+
    # scale_x_discrete(breaks = xbreaks)+
    facet_wrap(.~topic, ncol=2)
  
      
  # plot of only RT probability in time
  rt.top<-probs[probs$retweetcount>0,]
  g.rt<- rt.top %>%
    group_by(date,topic) %>%
    summarise(sum_probability=sum(mprob)/sum(nbtweet))
  
  g.rt <- g.rt %>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
  
  g<-ggplot(g.rt, aes(x=date,y=sum_probability))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(g.rt$sum_probability)+0.05))+
    geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
    geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
    # geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[3])+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(paste0("Probability of retweets only (rt>1)"))+
    xlab(xlabel)+
    ylab("Probability")+
    # scale_x_discrete(breaks = xbreaks)+
    facet_wrap(.~topic, ncol=2)
  ggsave(paste0(res_dir,"k9-", xlabel, "-retweets",m,".pdf"), device = "pdf")

  
  rt.top2<-probs2[probs2$retweetcount>0,]
  g.rt2<- rt.top2 %>%
    group_by(month,topic) %>%
    summarise(sum_probability=sum(mprob)/sum(nbtweet))
  
  g.rt2 <- g.rt2 %>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
  
  g<-ggplot(g.rt2, aes(x=month,y=sum_probability))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(g.rt2$sum_probability)+0.05))+
    geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
    geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
    # geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[3])+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(paste0("Probability of retweets only (rt>1)"))+
    xlab(xlabel)+
    ylab("Probability")+
    # scale_x_discrete(breaks = xbreaks)+
    facet_wrap(.~topic, ncol=2)
  ggsave(paste0(res_dir,"k9-", xlabel, "-retweets",m,".pdf"), device = "pdf")
  
  
} 
probs<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
  gather(topic, probability, t_1:t_9) %>% ##topic_number k_list
  tidyr::separate(topic, into =c("t","topic")) %>%
  select(-t)%>%
  select(-id)
grouped.sp <- probs%>%
  group_by(date,topic) %>%
  summarise(sum_probability=mean(probability))
# global.means<-read.csv2("./results/twitter-trained/k9-global-means.csv")



grouped.sp<-grouped.sp%>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))

# global.means <- global.means %>% mutate(topic=factor(topic,levels = 1:9,labels=topic.labels$label))

# grouped.sp%>%write.csv2(paste0(res_dir,"k9-week-probs",m,".csv")) # probabilities to file

g<-ggplot(grouped.sp, aes(x=date,y=sum_probability))+
  geom_bar(stat="identity",position="stack")+
  ylim(0.0, (max(grouped.sp$sum_probability)+0.05))+
  geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
  geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Probability of tweets only")+
  xlab(xlabel)+
  ylab("Probability")+
  # scale_x_discrete(breaks = xbreaks)+
  facet_wrap(.~topic, ncol=2)
ggsave(paste0(res_dir,"k9-", xlabel, "-tweets",m,".pdf"), device = "pdf")
