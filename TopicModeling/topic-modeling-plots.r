library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(wordcloud)
library(tidyr)
library(ggwordcloud)
library(gridExtra)
library(readr)
library(jcolors)

####selected parameters to check the results####

####Twitter 
# data_name<-"twitter-2M"
# 
# k_list<-9
# alpha<-0.1 # 0.alpha value
# ngram<- 1#ngrams
# 
# rds_dir <- "./results/twitter-2M" #"./test/"
# exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
# model_dir <- "./results/twitter-2M/"
# model_name <- paste0("_topics-",exp_name, ".rda")
# res_dir<-model_dir
# dtm_file <- "./results/twitter-2M/dtm-twitter-2M-ngram-1.Rds"
# original_tf_file <- "./results/twitter-2M/originaltf-twitter-2M-ngram-1.Rds"
#####
  
# data_name<-"guardian-comments"
# data_dir<-"./data/guardian/full_comments_guardian.csv"
# rds_dir <- "./results/guardian-comments/"
# model_dir <- paste0("./results/guardian-comments/k-",k_list)
# res_dir <- "./results/guardian-comments"
# exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
# dtm_file<-paste0(rds_dir,"/dtm-",data_name,"-ngram-",ngram,".Rds")
# original_tf_file <- paste0(rds_dir,"/originaltf-",data_name,"-ngram-",ngram,".Rds")

####GUardian-articles
data_name<-"guardian-articles"

k_list<-10
alpha<-0.1 # 0.alpha value
ngram<- 1#ngrams
exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
dtm_file<-"./results/guardian-articles/dtm-guardian-articles-ngram-1.Rds"
original_tf_file<-"./results/guardian-articles/originaltf-guardian-articles-ngram-1.Rds"
model_name <- paste0("_topics-",exp_name, ".rda")
model_dir <- "./results/guardian-articles/"
res_dir<-model_dir


##################

dtm<-readRDS(dtm_file)
original_tf <- readRDS(original_tf_file)


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

# model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
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

# terms.summary %>% write_csv("./results/twitter-2M/twitter-term-topic-probab.csv")

# top20.summary -> word +topic + probability 
top20.summary <- terms.summary %>% group_by(topic) %>% top_n(20)

top20.summary <- top20.summary %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)

word_topic_freq <- left_join(top20.summary, original_tf, by = c("word" = "term"))

# top20.summary %>% arrange(topic, value) %>% group_by(topic) %>% top_n(2) %>% write_csv("./results/twitter-trained/k-9-topic-words.csv")
top20.summary %>% arrange(topic, value) %>% group_by(topic) %>% top_n(2) %>% write_csv("./results/guardian-articles/k-10-topic-words.csv")

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
  arrange(desc(value)) 

document_topic%>%write_csv("./results/guardian-predicted.csv") ## delete filter row number == 1 to have all probabilities

document_topic <- document_topic %>%
  filter(row_number() ==1)
#Visualising of topics in a dendrogram
#probability distributions called Hellinger distance, distance between 2 probability vectors
model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
pdf(paste0(res_dir,"/dendrogram-k-",k_list,"-",exp_name,".pdf"))
plot(model$hclust)
dev.off()

#visualising topics of words based on the max value of phi
# set.seed(1234)
pdf(paste0(res_dir,"/cluster-k-",k_list,"-",exp_name,".pdf"))
for(i in 1:length(unique(top20.summary$topic))){  
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.1, y=0.1, paste0("Topic ",i))
  wordcloud(words = subset(top20.summary, topic == i)$word, 
            freq = subset(top20.summary, topic == i)$value, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

dev.off()

### #1B9E77 green, #7570B3 violet, "#E7298A fuxia
wclist<-list()
for(i in 1:length(unique(top20.summary$topic))){  
 wclist[[i]]<- ggwordcloud(words = subset(top20.summary, topic == i)$word, 
                      freq = subset(top20.summary, topic == i)$value,
            scale = c(1.8, 0.5),
            min.freq = 1,
            max.words = 20, 
            random.order = FALSE, 
            random.color = FALSE,
            rot.per = 0.2,
 # scale_color_jcolors(palette = "pal7")+
            # colors = brewer.pal(8, "Dark2")) +
            colors = "#1B9E77")+ #green
            # colors = "#7570B3")+ #violet
            # colors = "#E7298A")+ #pink
          ggtitle(paste0('Topic ',i))
}
wcall<-grid.arrange(grobs=wclist, top=exp_name, ncol=2)
ggsave(file=paste0(res_dir,"/all-cluster-k-",k_list,"-",exp_name,".pdf"), wcall, device="pdf")  
