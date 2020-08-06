library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(scales)

xlabel<-'Date'

topics.labs.fun<-function(labfilename){
  topic.labels<-read_csv(labfilename)
  topic.labels<-topic.labels%>% 
    group_by(topic) %>%
    summarise(s=paste(word, collapse = ", "))
  
  topic.labels<- topic.labels%>%
    group_by(topic) %>%
    summarize(label=paste(s,topic, collapse = ","))
}

col <- c("#CC6666", "#9999CC", "#66CC99") # colors for plots

topic.labels<-topics.labs.fun("./results/twitter-trained/k-9-topic-words.csv")
global.means.rt<-read_csv("../results/twitter-trained/k9-global-means-retweets.csv")
global.means.rt <- global.means.rt %>% mutate(topic=factor(topic,levels = 1:9,labels=topic.labels$label))
global.means<-read_csv("./results/twitter-trained/k9-global-means.csv")
global.means <- global.means %>% mutate(topic=factor(topic,levels = 1:9,labels=topic.labels$label))

data_dir<-"./results/twitter-trained/sorted/"
res_dir<-"./results/twitter-trained/"


data.files <- list.files(data_dir)
nfiles<-length(data.files)

gathered<-data.frame()
for (i in data.files){
  print(i)
  df <- read_csv(paste0(data_dir,i), col_types = cols (id = col_character()))
  df$date<-as.Date(df$date)
  
  
  
  ###T
  probs.tweets<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
    gather(topic, probability, t_1:t_9) %>% ##topic_number k_list
    tidyr::separate(topic, into =c("t","topic")) %>%
    select(-t)%>%
    select(-id)
  gathered<-bind_rows(gathered,probs.tweets)
  
  
  ###RT
  probs.rt<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
    pivot_longer(names_to="topic", values_to="probability", c(t_1:t_9)) %>% ##topic_number k_list
    tidyr::separate(topic, into =c("t","topic")) %>%
    select(-t)%>%
    select(-id)

  retweets.only <- probs.rt %>%
    filter(retweetcount >0)
  retweets.only <- retweets.only %>%
    mutate(mprob=probability*retweetcount)


}


###T
print("grouping t...")
grouped.tweets <- gathered%>%
  group_by(date,topic) %>%
  summarise(sum_probability=mean(probability))
print("done!")

grouped.tweets<-grouped.tweets%>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))

print("ploting t")
g<-ggplot(grouped.tweets, aes(x=date,y=sum_probability))+
  geom_bar(stat="identity",position="stack")+
  ylim(0.0, (max(grouped.tweets$sum_probability)))+
  geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
  # geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Probability of tweets only")+
  xlab(xlabel)+
  ylab("Probability")+
  facet_wrap(.~topic, ncol=2)
ggsave(paste0(res_dir,"k9-all-", xlabel, "-tweets.pdf"), device = "pdf", scale=2)

###RT
  grouped.retweets.only<- retweets.only %>%
    group_by(date,topic) %>%
    summarise(sumpro=sum(mprob),sumtweet=sum(retweetcount))%>%
    mutate(sum_probability=sumpro/sumtweet)

  plot.retweets<-grouped.retweets.only%>%
    select(-sumpro,-sumtweet)

  plot.retweets <- plot.retweets %>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
  g<-ggplot(plot.retweets, aes(x=date,y=sum_probability))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(plot.retweets$sum_probability)))+
    # geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
    geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[1])+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(paste0("Probability of retweets only (rt>1)"))+
    xlab(xlabel)+
    ylab("Probability")+
    facet_wrap(.~topic, ncol=2)
ggsave(paste0(res_dir,"k9-all-", xlabel, "-retweets.pdf"), device = "pdf", scale=2)
  
# if(retweets.plots){ #plots for RT only
#   # plot of only RT probability in time
#   
#   probs.rt<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
#     pivot_longer(names_to="topic", values_to="probability", c(t_1:t_9)) %>% ##topic_number k_list
#     tidyr::separate(topic, into =c("t","topic")) %>%
#     select(-t)%>%
#     select(-id)
#   
#   retweets.only <- probs.rt %>% 
#     filter(retweetcount >0)
#   retweets.only <- retweets.only %>%
#     mutate(mprob=probability*retweetcount)
#   
#   grouped.retweets.only<- retweets.only %>%
#     group_by(date,topic) %>%
#     summarise(sumpro=sum(mprob),sumtweet=sum(retweetcount))%>%
#     mutate(sum_probability=sumpro/sumtweet)
#   
#   plot.retweets<-grouped.retweets.only%>%
#     select(-sumpro,-sumtweet)
#   
#   plot.retweets <- plot.retweets %>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
#   g<-ggplot(plot.retweets, aes(x=date,y=sum_probability))+
#     geom_bar(stat="identity",position="stack")+
#     ylim(0.0, (max(plot.retweets$sum_probability)))+
#     geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
#     geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
#     theme(axis.text.x = element_text(angle = 90))+
#     ggtitle(paste0("Probability of retweets only (rt>1)"))+
#     xlab(xlabel)+
#     ylab("Probability")+
#     facet_wrap(.~topic, ncol=2)
#   ggsave(paste0(res_dir,"k9-", xlabel, "-retweets",m,".pdf"), device = "pdf")
# } 

# if(tweets.plots){
#   #plot timeline tweets only
#   probs.tweets<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
#     gather(topic, probability, t_1:t_9) %>% ##topic_number k_list
#     tidyr::separate(topic, into =c("t","topic")) %>%
#     select(-t)%>%
#     select(-id)
#   
#   grouped.tweets <- probs.tweets%>%
#     group_by(date,topic) %>%
#     summarise(sum_probability=mean(probability))
#   
#   grouped.tweets<-grouped.tweets%>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
#   
#   g<-ggplot(grouped.tweets, aes(x=date,y=sum_probability))+
#     geom_bar(stat="identity",position="stack")+
#     ylim(0.0, (max(grouped.tweets$sum_probability)))+
#     geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
#     geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
#     theme(axis.text.x = element_text(angle = 90))+
#     ggtitle("Probability of tweets only")+
#     xlab(xlabel)+
#     ylab("Probability")+
#     facet_wrap(.~topic, ncol=2)
#   ggsave(paste0(res_dir,"k9-", xlabel, "-tweets",m,".pdf"), device = "pdf")
#   
# }  
