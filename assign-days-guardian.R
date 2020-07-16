library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(scales)

# selected.topic<-"world, action 6"

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

topic.labels<-topics.labs.fun("./results/guardian-articles/k-10-topic-words.csv")

info_path<-"./data/guardian/full_articles_guardian.csv"

pred_path<-"./results/guardian-articles/guardian-predicted.csv"
res_dir <-"./results/guardian-articles/probabilities/"

info <- read_delim(info_path, delim = ";")#, col_types = cols (id = col_character()))
info$date_published<-as.Date(as.POSIXct((info$date_published/1000), origin = "1970-01-01"))

pred <- read_csv(pred_path)

df<-merge(info,pred, by.x="id", by.y="document")

global.means <- df%>% 
  group_by(topic) %>%
  summarize(gtp=mean(value))

global.means <- global.means %>% mutate(topic=factor(topic,levels = 1:10,labels=topic.labels$label))

#plot timeline only
probs<-df%>%select(-type,-url,-authors,-authors_nb,-section,-tags,-tags_nb,-date_modified,-share_count,-comment_nb,
                            -title,-description,-text,-id)%>%
    rename(date = date_published)%>%
    rename(probability=value)%>%
    arrange(date,topic)
  
grouped.probs <- probs%>%
    group_by(date,topic) %>%
    summarise(sum_probability=mean(probability))
  
grouped.probs<-grouped.probs%>%  mutate(topic=factor(topic,levels = 1:10,  labels=topic.labels$label))
  

g<-ggplot(grouped.probs, aes(x=date,y=sum_probability))+
  geom_bar(stat="identity",position="stack")+
  ylim(0.0, (max(grouped.probs$sum_probability)))+
  geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
  # geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Probability of tweets only")+
  xlab(xlabel)+
  ylab("Probability")+
  facet_wrap(.~topic, ncol=2)
ggsave(paste0(res_dir,"day-probs.pdf"), device = "pdf", limitsize = FALSE, plot=last_plot(), scale = 2)
  
  
selected<-df%>%select(-type,-url,-authors,-authors_nb,-section,-tags,-tags_nb,-date_modified,-share_count,-comment_nb,
                        -title,-description)%>%
    rename(date = date_published)%>%
    rename(probability=value)%>%
    arrange(date,topic)
selected.period<-selected%>%filter(date>"2017-04-25",date<"2017-05-11")
selected.topic<-selected.period%>%filter(topic==6, probability>=0.4)
  
selc<-probs%>%
    group_by(date,topic) %>%
    summarise(sum_probability=mean(probability)) %>% 
    filter(date>as.Date("2017-03-01"),date < as.Date("2017-07-31"))

selc %>% write_csv(paste0(res_dir,"all-day-probs-mars17-jul17.csv"))
  # selc %>% write_csv(paste0(res_dir,"all-day-probs-mars17-may17.csv"))
  
selc<-selc%>%  mutate(topic=factor(topic,levels = 1:10,  labels=topic.labels$label))
  
g<-ggplot(selc, aes(x=date,y=sum_probability))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(grouped.probs$sum_probability)))+
    geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[1])+
    # geom_hline(data=global.means.rt, aes(yintercept = gtp), lty="dashed",color=col[3])+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle("Probability of tweets only")+
    xlab(xlabel)+
    ylab("Probability")+
    facet_wrap(.~topic, ncol=2)
ggsave(paste0(res_dir,"day-probs-mars17-jul17.pdf"), device = "pdf")
  
    