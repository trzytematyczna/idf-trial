library(topicmodels)
library(ggplot2)
library(textmineR)
library(dplyr)
library(data.table)
library(tidyr)


####selected parameters to check the results####
k_list<-10
# k_list <- c(5,10,15)
alpha<-0.1 # 0.alpha value
ngram<- 1#ngrams

# data_name<-"twitter-3M"
# data_dir<-"./data/twitter/split-3M/xaa.csv"

# data_name<-"guardian-comments"
# data_dir<-"./data/guardian/full_comments_guardian.csv"

data_name <- "guardian-articles"
data_dir <- "./data/guardian/full_articles_guardian.csv"

rds_dir <- paste0("./results/",data_name,"/")
model_dir <- paste0("./results/",data_name,"/k-",k_list)
res_dir <- paste0("./results/",data_name,"/k-",k_list,"/")

exp_name<-paste0("k-",k_list,"-alpha-",alpha,"-ngram-",ngram)
rds_name<-paste0(data_name,"-ngram-",ngram,".Rds")
dtm_file<-paste0(rds_dir,"dtm-",rds_name)
original_tf_file <- paste0(rds_dir,"originaltf-",rds_name)

##################
if(data_name %like% "twitter"){
  data <- read.csv2(data_dir, stringsAsFactors = FALSE, sep=",", quote = "\"", colClasses = c("factor","character"))#, encoding = "UTF-8")
  data$id<- 1:nrow(data)
}else{
  data<- read.csv2(data_dir, stringsAsFactors = FALSE)
}
dtm<-readRDS(dtm_file)
original_tf <- readRDS(original_tf_file)

read.model.fun <- function(k){
  filename = file.path(model_dir, paste0(k, "_topics_a",alpha,".rda"))
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
  data.table::melt(idvars = "word") %>%
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
  data.table::melt(id.vars = "document") %>% 
  rename(topic = variable) %>% 
  tidyr::separate(topic, into =c("t","topic")) %>% 
  select(-t) %>% 
  group_by(document) %>% 
  arrange(desc(value)) %>%
  filter(row_number() ==1)





sp <- data.frame(model$theta)
sp$document <-rownames(sp)
rownames(sp) <- 1:nrow(sp)
sp <- sp%>%  plyr::rename(c("document"="id"))

if(!is.null(data$date_published)){
  sum.probab <- merge(sp,(select(data,id,date_published)), by="id")
  sum.probab <- sum.probab%>%  plyr::rename(c("date_published"="date"))
  sum.probab$date<-as.numeric(sum.probab$date)
  sum.probab$date<- as.Date(as.POSIXct((sum.probab$date/1000), origin = "1970-01-01"))
} else{
  sum.probab <- merge(sp,(select(data,id,date)), by="id")
  sum.probab$date<- as.Date(sum.probab$date)
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
  mutate(month=format(date,'%y-%V')) %>%
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

g<-ggplot(grouped.glob.top, aes(x=month,y=sum_probability))+
  geom_bar(stat="identity",position="stack")+
  ylim(0.0, (max(grouped.sp$sum_probability)+0.05))+
  geom_hline(data=global.probabilities, aes(yintercept = avg_probability), lty="dashed", color=col[1])+
  geom_hline(data=global.probabilities, aes(yintercept = med_probability), lty="dashed",color=col[3])+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle(paste0(data_name,"-",exp_name))+
  xlab("week")+
  ylab("probability")+
  # scale_x_date(breaks = grouped.glob.top$month[seq(1, length(grouped.glob.top$month), by = 2)])+
  facet_wrap(.~topic, ncol=2)
ggsave(paste0(res_dir,data_name,"-",exp_name,"-all-topics-week-probabilities.pdf"))