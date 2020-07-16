library(ggplot2)
library(dplyr)
library(stringr)
library(Rfast)


# 6 - science 8 - hoax
topics.labs.fun<-function(labfilename){
  topic.labels<-read.csv(labfilename)
  topic.labels<-topic.labels%>% 
    group_by(topic) %>%
    summarise(s=paste(word, collapse = ", "))
  
  topic.labels<- topic.labels%>%
    group_by(topic) %>%
    summarize(label=paste(s,topic, collapse = ","))
}

df <- read.csv("/home/mra/Documents/UMPC/idf-trial/results/twitter-trained/sentiment-assign-1.csv")

col <- c("#CC6666", "#9999CC", "#66CC99") # colors for plots

topic.labels<-topics.labs.fun("./results/twitter-trained/k-9-topic-words.csv")

colnames(df)<-c("id","date","retweetcount","from_user_id","from_user_name","from_user_followercount",
                  "text","t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9", "compound", "neg","neu","pos")

df2<-df[,8:16]

df$max_t<-rowMaxs(as.matrix.data.frame(df2))

# check.df <- df %>% 
#   group_by(max_t) %>% 
#   summarize(freq=sum())
  


# positive sentiment: compound score >= 0.05
# neutral sentiment: (compound score > -0.05) and (compound score < 0.05)
# negative sentiment: compound score <= -0.05



trim <- df[df$max_t==6|df$max_t==8,]

trim.sentiment<-trim %>% 
  mutate(sentiment = ifelse(between(compound,0.05,1),
                            "pos",
                            ifelse(between(compound,-1,-0.05),
                                   "neg",
                                    "neut")))
trim.sentiment.sum<- trim.sentiment %>% group_by(max_t,sentiment)%>%
  summarise(n=n())


trim.sentiment.sum<-trim.sentiment.sum %>%
  ungroup()%>%
  mutate(max_t=factor(max_t,levels = c(6,8),  labels=c("hoax","science")))


g<-ggplot(trim.sentiment.sum, aes(fill= sentiment, x=max_t,y=n))+
  geom_bar(stat="identity",position="dodge")+
  # ylim(0.0, (max(grouped.sp$sum_probability)+0.05))+
  # geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[3])+
  # theme(axis.text.x = element_text(angle = 90))+
  # ggtitle(paste0(data_name,"-",exp_name))+
  xlab("")+
  ylab("count")
  # scale_x_discrete(breaks = xbreaks)+
  # facet_wrap(.~topic, ncol=2)
# ggsave(paste0(res_dir,"k9-week-probs",m,".pdf"))
