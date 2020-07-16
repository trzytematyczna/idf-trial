library(topicmodels)
library(ggplot2)
library(textmineR)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(stringi)
library(readr)

ipcc-analyse<-F 
####selected parameters to check the results####
k_list<-9
# k_list <- c(5,10,15)
alpha<-0.1 # 0.alpha value
ngram<- 1#ngrams

data_name<-"twitter-2M"
data_dir<-"./data/twitter/texts-preformat-no-retweets/split-2M/twitter-2M-sampled.csv"

# data_name<-"guardian-comments"
# data_dir<-"./data/guardian/full_comments_guardian.csv"

# k_list<-10
# data_name <- "guardian-articles"
# data_dir <- "./data/guardian/full_articles_guardian.csv"

rds_dir <- paste0("./results/",data_name,"/")
model_dir <- paste0("./results/",data_name,"/")
res_dir <- paste0("./results/",data_name,"/")

exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)

dtm_file<-paste0(rds_dir,"/dtm-",data_name,"-ngram-",ngram,".Rds")
original_tf_file <- paste0(rds_dir,"/originaltf-",data_name,"-ngram-",ngram,".Rds")

model_name <- paste0("_topics-",exp_name, ".rda")


##################
if(data_name %like% "twitter"){
  # data <- read.csv(data_dir, stringsAsFactors = FALSE, sep=",", quote = "\"", header = TRUE, colClasses = c("character"))#, encoding = "UTF-8")
  data<-read_csv(data_dir, col_types = cols (id = col_character()))
  
  data$from_user_id<-NULL
  data$from_user_name<-NULL
  data$from_user_followercount<-NULL
}else{
  data<-read_csv2(data_dir, col_types = cols (id = col_character()))
  data$date_published<-as.numeric(data$date_published)
  data$date_published<- as.Date(as.POSIXct((data$date_published/1000), origin = "1970-01-01"))
  
  # data<- read.csv2(data_dir, stringsAsFactors = FALSE)
}
# dtm<-readRDS(dtm_file)
# original_tf <- readRDS(original_tf_file)

read.model.fun <- function(k){
  filename = file.path(model_dir, paste0(k, model_name))
  if (!file.exists(filename)) {
    print("Nofile!")
  } else {
    load(filename)
  }
  m
}

model_list <- TmParallelApply(X = k_list, FUN = read.model.fun)


# rows of phi = topics; columns = tokens (words). 
# rows of theta = documents; columns = topics.

model<-model_list[[1]]
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
top20_wide <- as.data.frame(model$top_terms)

#terms.summary -> word + topic + probability

terms.summary <-data.frame(t(model$phi))
terms.summary$word <- rownames(terms.summary) 
rownames(terms.summary) <- 1:nrow(terms.summary)
terms.summary <- terms.summary %>% 
  reshape2::melt(idvars = "word") %>%
  plyr::rename(c("variable" ="topic"))%>%  
  group_by(topic) %>% 
  arrange(desc(value))


# top20.summary -> word +topic + probability 
top20.summary <- terms.summary %>% group_by(topic) %>% top_n(20)

top20.summary <- top20.summary %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)

word_topic_freq <- left_join(top20.summary, original_tf, by = c("word" = "term"))

# document -> topic
document_topic <- data.frame(model$theta)
document_topic$document <-rownames(document_topic) 
rownames(document_topic) <- 1:nrow(document_topic)
document_topic <- document_topic %>% 
  reshape2::melt(id.vars = "document") %>% 
  rename(topic = variable) %>% 
  tidyr::separate(topic, into =c("t","topic")) %>% 
  select(-t) %>% 
  group_by(document) %>% 
  arrange(desc(value)) %>%
  filter(row_number() ==1)
# document_topic %>% str_replace("X","")
# if(data_name %like% "twitter"){
  document_topic$document<-document_topic$document %>%stri_replace_all_fixed("X","")
# }


sp <- data.frame(model$theta)
sp$document <-rownames(sp)
rownames(sp) <- 1:nrow(sp)
sp <- sp%>%  plyr::rename(c("document"="id"))
# if(data_name %like% "twitter"){
  sp$id<-sp$id %>%stri_replace_all_fixed("X","")
# }


if(!is.null(data$date_published)){
  sum.probab<-sp
  sum.probab<- cbind(sum.probab, date = data$date)
  # sum.probab <- merge(sp,(select(data,id,date_published)), by="id")
  sum.probab <- sum.probab%>%  plyr::rename(c("date_published"="date"))
  sum.probab$date<-as.numeric(sum.probab$date)
  sum.probab$date<- as.Date(as.POSIXct((sum.probab$date/1000), origin = "1970-01-01"))
} else{
  sum.probab<-sp
  sum.probab<- cbind(sum.probab, date = data$date)
  sum.probab$date<- as.Date(sum.probab$date)
  # sum.probab <- merge(sp,(select(data,id,date)), by="id")
  # sum.probab$date<- as.Date(sum.probab$date)
}


tmax<-paste0("t_",k_list)

global.topic.probab <- sum.probab %>%
  gather(topic, probability, t_1:!!sym(tmax)) %>% ##topic_number k_list
  tidyr::separate(topic, into =c("t","topic")) %>%
  select(-t)%>%
  select(-id,-date)

global.topic.probab1 <- global.topic.probab %>%
  group_by(topic) %>%
  summarise(avg_probability=mean(probability))

global.topic.probab2 <- global.topic.probab %>%
  group_by(topic) %>%
  summarise(med_probability=median(probability))

global.probabilities <- merge(global.topic.probab1, global.topic.probab2, by="topic")%>%
  mutate(topic=factor(topic,levels = 1:10))


g<-ggplot(global.probabilities, aes(x=reorder(topic, avg_probability),y=avg_probability))+
  geom_bar(stat="identity",position="stack")+
  ylim(0.0, (max(global.probabilities$avg_probability)))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle(paste0("Overall probability per topic"))
ggsave(paste0(res_dir,data_name,"-",exp_name,"-global-avg-probabilities.pdf"))



grouped.sp <- sum.probab %>%
  mutate(month=format(date,'%y-%U')) %>%
  gather(topic, probability, t_1:!!sym(tmax)) %>% ##topic_number k_list
  tidyr::separate(topic, into =c("t","topic")) %>%
  select(-t)%>%
  select(-id,-date)


grouped.sp<- grouped.sp%>%
  group_by(month,topic) %>%
  summarise(sum_probability=mean(probability))



grouped.glob.top <- merge(grouped.sp, global.probabilities, by="topic")%>%
  mutate(topic=factor(topic,levels = 1:10))



col <- c("#CC6666", "#9999CC", "#66CC99")

for(i in 1:max(as.numeric(grouped.sp$topic))){
  topic <- grouped.glob.top %>% filter(topic==i)
  tgp_a<- grouped.glob.top[grouped.glob.top$topic==i,]$avg_probability
  tgp_m<- grouped.glob.top[grouped.glob.top$topic==i,]$med_probability
  g<-ggplot(topic, aes(x=month,y=sum_probability))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(grouped.sp$sum_probability)))+
    geom_hline(yintercept = tgp_a, lty="dashed", color=col[1])+
    geom_hline(yintercept = tgp_m, lty="dashed",color=col[3])+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(paste0("Probability of topic ",i," per month"))+
    xlab("month")+
    ylab("probability")

  ggsave(paste0(res_dir,data_name,"-",exp_name,"-topic-",i,"-probabilities.pdf"))
  # ggsave(paste0(plots_dir,"Comments_Topic-",i,"+global.pdf"))

}


######### all topics in one graph
# labs<-grouped.glob.top$month
# labs = grouped.glob.top[seq(1, nrow(grouped.glob.top), 2), ]$month

labs<-grouped.glob.top[grouped.glob.top$topic==1,]%>% arrange(month)
xbreaks <- labs$month[seq(from = 1, to = nrow(labs), by = 10)]


g<-ggplot(grouped.glob.top, aes(x=month,y=sum_probability))+
  geom_bar(stat="identity",position="stack")+
  ylim(0.0, (max(grouped.sp$sum_probability)+0.05))+
  geom_hline(data=global.probabilities, aes(yintercept = avg_probability), lty="dashed", color=col[1])+
  geom_hline(data=global.probabilities, aes(yintercept = med_probability), lty="dashed",color=col[3])+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle(paste0(data_name,"-",exp_name))+
  xlab("week")+
  ylab("probability")+
  scale_x_discrete(breaks = xbreaks)+
  facet_wrap(.~topic, ncol=2)
g
ggsave(paste0(res_dir,data_name,"-",exp_name,"-all-topics-week-probabilities.pdf"))


if(ipcc-analyse){
  source("functions-stats.R")
  data$text<-tolower(data$text)
  
  arts<-getDataByOrganization("IPCC", data)
  arts<-arts%>%
    rbind(getDataByOrganization("Intergovernmental Panel on Climate Change",data))%>%
    unique()
  
  
  n.dt<- document_topic[document_topic$document %in% arts$id,]%>%
    plyr::rename(c("document"="id"))
  
  
  
  # arts.dt<-merge(n.dt,arts, by="id")%>% select(id,topic) %>% group_by(topic) %>% 
  topic.top1.word <- terms.summary %>% group_by(topic) %>% top_n(1)
  
  topic.top1.word <- topic.top1.word %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
    ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
  
  topic.top1.word$labs <- paste(topic.top1.word$word," (",topic.top1.word$topic,")",sep="")
  
  arts.dt<-as.data.frame(n.dt)
  arts.dt<- arts.dt%>%
    select(id,topic) %>%
    group_by(topic) %>%
    summarize(n=n())%>%
    mutate(topic=factor(topic,levels = 1:10, labels=topic.top1.word$labs))
  
  
  g<-ggplot(arts.dt, aes(x=reorder(topic,n), y=n))+
    geom_col( ) +
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle("Topics of IPCC articles")+
    coord_flip()+
    xlab("topics")+
    ylab("# articles")
  # geom_bar(stat="identity",position="stack")+
    # ylim(0.0, (max(grouped.sp$sum_probability)+0.05))+
    # scale_x_discrete(breaks = xbreaks)+
    # facet_wrap(.~topic, ncol=2)
  g
  
  
  
  #####
  
  topic.top1.word <- terms.summary %>% group_by(topic) %>% top_n(1)
  
  topic.top1.word <- topic.top1.word %>% group_by(topic, word) %>% filter(row_number() == 1) %>%
    ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
  
  topic.top1.word$labs <- paste(topic.top1.word$word," (",topic.top1.word$topic,")",sep="")
  
  arts.pb<-as.data.frame(n.dt)
  arts.pb<- arts.pb%>%
    # select(id,topic) %>%
    group_by(topic) %>%
    summarize(n=mean(value))%>%
    mutate(topic=factor(topic,levels = 1:10, labels=topic.top1.word$labs))
  
  
  g<-ggplot(arts.pb, aes(x=reorder(topic,n), y=n))+
  # g<-ggplot(arts.pb, aes(x=topic, y=n))+
    geom_col( ) +
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle("Topics of IPCC articles")+
    coord_flip()+
    xlab("topics")+
    ylab("probability of topic")
  # geom_bar(stat="identity",position="stack")+
  # ylim(0.0, (max(grouped.sp$sum_probability)+0.05))+
  # scale_x_discrete(breaks = xbreaks)+
  # facet_wrap(.~topic, ncol=2)
  ggsave(paste0(res_dir, data_name, "-ipcc-wtopic-probab.pdf"))
  

}