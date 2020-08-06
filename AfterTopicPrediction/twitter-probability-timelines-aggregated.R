library(quanteda)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)

# data_name<-"twitter-2M"

equals.yes<-F
with.retweets<-F
do.entropy<-F
week.yes<-F
month.yes<-T

if(week.yes == TRUE){
  divide.by <- '%y-%V'
  xlabel<-'week'
}else if(month.yes == TRUE){
  divide.by <- '%y-%m'
  xlabel<-'month'
}

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

topic.labels<-topics.labs.fun("../results/twitter-trained/k-9-topic-words.csv")
global.means.rt<-read_csv("../results/twitter-trained/k9-global-means-retweets.csv")
global.means.rt <- global.means.rt %>% mutate(topic=factor(topic,levels = 1:9,labels=topic.labels$label))
global.means<-read_csv("../results/twitter-trained/k9-global-means.csv")
global.means <- global.means %>% mutate(topic=factor(topic,levels = 1:9,labels=topic.labels$label))

data_dir<-"../results/twitter-trained/sorted/"

options <- commandArgs(trailingOnly = TRUE)
m<-options[1]
filename<-paste0("sorted-assign-",m,".csv")
res_dir<-"../results/twitter-trained/probabilities-month/"

  df <- read_csv(paste0(data_dir,filename), col_types = cols (id = col_character()))
  df$date<-as_date(df$date)
  
  if(equals.yes==TRUE){
    #equals <- df[df[,7:15]== 0.111111111111111,]
    cols<-c("t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
    equals <- df %>%  filter_at(cols, all_vars(.==0.111111111111111))
    equals <- equals[complete.cases(equals),]
    write.table(equals, paste0(res_dir, "equals.csv"), sep = ",", append = T, col.names = F,row.names = F)

    bf<-length(df$id)
    df<-df[!df$id %in% equals$id,]
    af<-length(df$id)
    b<-data.frame(m, bf,af,af/bf)
    write.table(b, paste0(res_dir,"df-equals-nb.csv"),append = T, sep=",", quote = F, row.names = F, col.names = F)
  }
  
  
  if(with.retweets==T){
    probs<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
      mutate(month=format(date, divide.by))%>%
      pivot_longer(names_to="topic", values_to="probability", c(t_1:t_9)) %>% ##topic_number k_list
      tidyr::separate(topic, into =c("t","topic")) %>%
      select(-t)%>%
      select(-id,-date)
    
    probs <- probs %>%
      mutate(nbtweet=retweetcount+1) %>%
      mutate(mprob=probability*nbtweet)
    
    
    # plot of ptobability in time of tweets and retweets
    grouped.tweets.retweets <- probs %>%
      group_by(month,topic) %>%
      summarise(sum_probability=sum(mprob)/sum(nbtweet))

    
    grouped.tweets.retweets<-grouped.tweets.retweets%>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
    
    g<-ggplot(grouped.tweets.retweets, aes(x=month,y=sum_probability))+
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
    ggsave(paste0(res_dir,"k9-", xlabel, "-tweets-retweets",m,".pdf"), device = "pdf")
    
    
    # plot of only RT probability in time
    rt.top<-probs[probs$retweetcount>0,]
    g.rt<- rt.top %>%
      group_by(month,topic) %>%
      summarise(sum_probability=sum(mprob)/sum(nbtweet))
    
    g.rt <- g.rt %>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
    
    g<-ggplot(g.rt, aes(x=month,y=sum_probability))+
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
    
    
  } 
    probs<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
      mutate(month=format(date,divide.by))%>%
      gather(topic, probability, t_1:t_9) %>% ##topic_number k_list
      tidyr::separate(topic, into =c("t","topic")) %>%
      select(-t)%>%
      select(-id,-date)
    grouped.sp <- probs%>%
      group_by(month,topic) %>%
      summarise(sum_probability=mean(probability))
    # global.means<-read_csv2("./results/twitter-trained/k9-global-means.csv")
  
  
  
  grouped.sp<-grouped.sp%>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
  
  # global.means <- global.means %>% mutate(topic=factor(topic,levels = 1:9,labels=topic.labels$label))
  
  # grouped.sp%>%write.csv2(paste0(res_dir,"k9-week-probs",m,".csv")) # probabilities to file

  g<-ggplot(grouped.sp, aes(x=month,y=sum_probability))+
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
  
  if(do.entropy==T){
  tweets.entropy<-df%>% 
    select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
    mutate(tentropy=-rowSums(df[,7:15]*log2(df[,7:15]))/9)
  
  
  
  entropy.plot<-tweets.entropy%>%select(id,date,tentropy)
  # hist(entropy.plot$tentropy, density = 100, ylim = range(1,2000))
  
  g<-qplot(tentropy, data=tweets.entropy, geom="histogram") +
    xlab("tweet entropy")+
    ylab("#")+
    ylim(0,1500)+
    theme(axis.text.x = element_text(angle = 90))+
    scale_x_continuous(n.breaks=30)
  g

  ggsave(plot = g,"./results/twitter-trained/tweets_entropy-histogram.pdf", device = "pdf")
  x<-entropy.plot$tentropy
  hist.scott(x, prob = TRUE, xlab = deparse(substitute(x)))
  }