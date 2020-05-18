library(quanteda)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(tidyr)
library(ggplot2)
# data_name<-"twitter-2M"

topics.labs.fun<-function(labfilename){
  topic.labels<-read.csv(labfilename)
  topic.labels<-topic.labels%>% 
    group_by(topic) %>%
    summarise(s=paste(word, collapse = ", "))
  
  topic.labels<- topic.labels%>%
    group_by(topic) %>%
    summarize(label=paste(s,topic, collapse = ","))
}
equals.yes<-F
data_dir<-"./results/twitter-trained/assign-joined/"

options <- commandArgs(trailingOnly = TRUE)
m<-options[1]
filename<-paste0("assign-",m,".csv")

# learning.data.file<-"./data/twitter/split-2M/twitter-2M-sampled.csv"
res_dir<-"./results/twitter-trained/"
# data.files<-data.files[1:2]
# i<-data.files[1]
  df <- read.csv(paste0(data_dir,filename), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8", na.strings = NA)
  df$date<-as.Date(df$date)
  colnames(df)<-c("id","date","from_user_id","from_user_name","from_user_followercount","text","t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
  
  if(equals.yes==TRUE){
    equals <- df[df[,7:15]== 0.111111111111111,]
    write.table(equals, paste0(res_dir, "equals.csv"), sep = ",", append = T)
  
    print(paste0("before ", nrow(df)))
    df<-df[!df$id %in% equals$id,]
    print(paste0("after",nrow(df)))
    
  }
  

  probs<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
    mutate(month=format(date,'%y-%V'))%>%
    gather(topic, probability, t_1:t_9) %>% ##topic_number k_list
    tidyr::separate(topic, into =c("t","topic")) %>%
    select(-t)%>%
    select(-id,-date)
  
  
  grouped.sp <- probs%>%
    group_by(month,topic) %>%
    summarise(sum_probability=mean(probability))
  
  topic.labels<-topics.labs.fun("./results/twitter-trained/k-9-topic-words.csv")
  global.means<-read.csv2("./results/twitter-trained/k9-global-means.csv")
  
  # grouped.glob.top <- merge(grouped.sp, global.probabilities, by="topic")%>%
  #   mutate(topic=factor(topic,levels = 1:10))
  
  # fin <- global.means %>%
  grouped.sp<-grouped.sp%>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
  
  global.means <- global.means %>% mutate(topic=factor(topic,levels = 1:9,labels=topic.labels$label))
  col <- c("#CC6666", "#9999CC", "#66CC99")
  
  # g<-ggplot(grouped.glob.top, aes(x=month,y=sum_probability))+
  g<-ggplot(grouped.sp, aes(x=month,y=sum_probability))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(grouped.sp$sum_probability)+0.05))+
    # geom_hline(data=global.probabilities, aes(yintercept = avg_probability), lty="dashed", color=col[1])+
    geom_hline(data=global.means, aes(yintercept = gtp), lty="dashed",color=col[3])+
    theme(axis.text.x = element_text(angle = 90))+
    # ggtitle(paste0(data_name,"-",exp_name))+
    xlab("month")+
    ylab("probability")+
    # scale_x_discrete(breaks = xbreaks)+
    facet_wrap(.~topic, ncol=2)
  ggsave(paste0(res_dir,"k9-week-probs",m,".pdf"))
  
