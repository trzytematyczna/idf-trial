library(quanteda)
library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(tidyr)
library(ggplot2)
# data_name<-"twitter-2M"

equals.yes<-F
with.retweets<-F

topics.labs.fun<-function(labfilename){
  topic.labels<-read.csv(labfilename)
  topic.labels<-topic.labels%>% 
    group_by(topic) %>%
    summarise(s=paste(word, collapse = ", "))
  
  topic.labels<- topic.labels%>%
    group_by(topic) %>%
    summarize(label=paste(s,topic, collapse = ","))
}
data_dir<-"./results/twitter-trained/assign-joined/"

options <- commandArgs(trailingOnly = TRUE)
m<-options[1]
filename<-paste0("assign-",m,".csv")

# learning.data.file<-"./data/twitter/split-2M/twitter-2M-sampled.csv"
res_dir<-"./results/twitter-trained/week-probs-retweets/"
# data.files<-data.files[1:2]
# i<-data.files[1]
  df <- read.csv(paste0(data_dir,filename), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8", na.strings = NA)
  df$date<-as.Date(df$date)
  colnames(df)<-c("id","date","from_user_id","from_user_name","from_user_followercount","text","t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
  
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
      mutate(month=format(date,'%y-%V'))%>%
      pivot_longer(names_to="topic", values_to="probability", c(t_1:t_9)) %>% ##topic_number k_list
      tidyr::separate(topic, into =c("t","topic")) %>%
      select(-t)%>%
      select(-id,-date)
    
    probs <- probs %>%
      mutate(nbtweet=retweet_number+1) %>% ### TBD: HERE name of retweet column!! 
      mutate(mprob=probability*nbtweet)
    
    grouped.sp <- probs %>%
      group_by(month,topic) %>%
      summarise(sum_probability=sum(mprob)/sum(nbtweet))

  } else{
    probs<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
      mutate(month=format(date,'%y-%V'))%>%
      gather(topic, probability, t_1:t_9) %>% ##topic_number k_list
      tidyr::separate(topic, into =c("t","topic")) %>%
      select(-t)%>%
      select(-id,-date)
    grouped.sp <- probs%>%
      group_by(month,topic) %>%
      summarise(sum_probability=mean(probability))
  }
  
  topic.labels<-topics.labs.fun("./results/twitter-trained/k-9-topic-words.csv")
  global.means<-read.csv2("./results/twitter-trained/k9-global-means.csv")
  
  # grouped.glob.top <- merge(grouped.sp, global.probabilities, by="topic")%>%
  #   mutate(topic=factor(topic,levels = 1:10))
  
  # fin <- global.means %>%
  grouped.sp<-grouped.sp%>%  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))
  
  global.means <- global.means %>% mutate(topic=factor(topic,levels = 1:9,labels=topic.labels$label))
  col <- c("#CC6666", "#9999CC", "#66CC99")
  
  grouped.sp%>%write.csv2(paste0(res_dir,"k9-week-probs",m,".csv"))
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
  