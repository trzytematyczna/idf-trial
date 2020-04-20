library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(wordcloud)
library(tidyr)

####selected parameters to check the results####
k_list<-10
alpha<-0.1 # 0.alpha value
ngram<- 1#ngrams

data_name<-"twitter-1M"
data_dir<-"./data/twitter/split-1M/twitter-1M-sampled.csv"

rds_dir <- "./results/twitter-1M" #"./test/"
model_dir<-"./results/twitter-1M" #"./test"
res_dir<-model_dir

# data_name<-"guardian-comments"
# data_dir<-"./data/guardian/full_comments_guardian.csv"
# rds_dir <- "./results/guardian-comments/"
# model_dir <- paste0("./results/guardian-comments/k-",k_list)
# res_dir <- "./results/guardian-comments"

exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)

dtm_file<-paste0(rds_dir,"/dtm-",data_name,"-ngram-",ngram,".Rds")
original_tf_file <- paste0(rds_dir,"/originaltf-",data_name,"-ngram-",ngram,".Rds")

model_name <- paste0("_topics-",exp_name, ".rda")


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

# pdf(paste0(res_dir,"/all-cluster-k-",k_list,"-",exp_name,".pdf"))
par(mfrow=c(5,2)) # for 1 row, 2 cols
for(i in 1:length(unique(top20.summary$topic))){  
  print(i)
  wordcloud(words = subset(top20.summary, topic == i)$word, 
            freq = subset(top20.summary, topic == i)$value, min.freq = 1,
            scale=c(seq(1,7, by=0.7)),
            max.words=10, random.order=FALSE, rot.per=0.4, 
            colors=brewer.pal(8, "Dark2"))
  text(x=-0.2, y=0.1, paste0("Topic ",i))
}
# dev.off()

wclist<-list()
for(i in 1:length(unique(top20.summary$topic))){  
 wclist[[i]]<- ggwordcloud(words = subset(top20.summary, topic == i)$word, 
                      freq = subset(top20.summary, topic == i)$value,
            scale = c(1.3, 0.5),
            min.freq = 1,
            max.words = 20, 
            random.order = FALSE, 
            random.color = FALSE,
            rot.per = 0.2, 
            colors = brewer.pal(8, "Dark2")) +
          ggtitle(paste0('Topic ',i))
}
wcall<-grid.arrange(grobs=wclist, top=exp_name, ncol=2)
ggsave(file=paste0(res_dir,"/all-cluster-k-",k_list,"-",exp_name,".pdf"), wcall, device="pdf")  
  

# tops<-top20.summary%>%mutate(topic=factor(topic,levels = 1:10))
# 
# g<-ggplot(tops,
#   aes(label = word, size = value)) +
#   geom_text_wordcloud_area() +
#   # scale_size_area(max_size = ) +
#   theme_minimal() +
#   facet_wrap(~topic)
# g

