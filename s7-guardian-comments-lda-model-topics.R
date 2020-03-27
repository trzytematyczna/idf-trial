library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(wordcloud)
library(tidyr)

####selected parameters to check the results####

k_list<-5
alpha<-0.1 # alpha value
ngram<-1

name<-paste0( "_K",k_list,"_ngram",ngram, "_al",alpha)
data_dir<-"./data/guardian/full_comments_guardian.csv"
plots_dir <- paste0("./plots/lda/comments/") ##directory of plots
model_dir <- paste0("./results/guardian-lda/comments/models/ngram_1:",ngram,"/alpha_",alpha)
res_dir <- paste0("./results/guardian-lda/comments/topics/K",k_list,"-ngram",ngram,"-al",alpha)

##################
if(!exists(res_dir)) {dir.create(res_dir)}

csv_data <- read.csv2(data_dir, stringsAsFactors = FALSE)
csv_data<-csv_data[!is.na(csv_data$text),]
data<-csv_data
data$text<-as.character(data$text)

data.corpus <- corpus(data, docid_field = "id", text_field = "text")
doc.tokens <- tokens(data.corpus)
doc.tokens <- doc.tokens%>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_select(stopwords(source='smart'),selection='remove')
data.dfm <- dfm(doc.tokens, ngrams=1:ngram)

# featnames(data.dfm)
# topfeatures(data.dfm, 5)
# head(kwic(doc.tokens, "love", window = 3))

data.trimmed <- data.dfm %>% dfm_trim(min_docfreq = 0.01, max_docfreq = 0.5, docfreq_type = "prop")
### min_termfreq = 0.9, termfreq_type = "quantile",
data.trimmed <- data.trimmed[ntoken(data.trimmed) > 0,]

# tf <- textstat_frequency(data.trimmed)
# colnames(tf)<-c("term","term_freq","rank","doc_freq","group")
# original_tf<-tf%>% select(term,term_freq, doc_freq)

dtm=as(as.matrix(data.trimmed), "dgCMatrix")
original_tf <- TermDocFreq(dtm = dtm)
# dtm<-dtm[ , original_tf$term_freq > 3 ]


# str(tf_mat)
# look at the most frequent bigrams
tf_bigrams <- original_tf[ stringr::str_detect(original_tf$term, "_") , ]
head(tf_bigrams[ order(tf_bigrams$term_freq, decreasing = TRUE) , ], 10)

# k_list <- seq(1, 25, by = 1)
# if (!dir.exists(model_dir)){ dir.create(model_dir)}
# if (!dir.exists(plots_dir)){ dir.create(plots_dir)}


run.model.fun <- function(k){
  filename = file.path(model_dir, paste0(k, "_topics_a",alpha,".rda"))
  if (!file.exists(filename)) {
    print("Nofile!")
  } else {
    load(filename)
  }
  m
}

model_list <- TmParallelApply(X = k_list, FUN = run.model.fun)


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
pdf(paste0(res_dir,"/dendrogram",name,".pdf"))
plot(model$hclust)
dev.off()

#visualising topics of words based on the max value of phi
# set.seed(1234)
pdf(paste0(res_dir,"/cluster",name,".pdf",sep=""))
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


######################
# 
# sp <- data.frame(model$theta)
# sp$document <-rownames(sp) 
# rownames(sp) <- 1:nrow(sp)
# sp <- sp%>%  plyr::rename(c("document"="id"))
# 
# sum.probab <- merge(sp,(select(data,id,date)), by="id")
# 
# sum.probab$date<- as.Date(sum.probab$date)
# 
# grouped.sp <- sum.probab %>% 
#   mutate(month=format(date,'%y-%m')) %>%
#   gather(topic, probability, t_1:t_5) %>% ##topic_number k_list
#   tidyr::separate(topic, into =c("t","topic")) %>% 
#   select(-t)%>%
#   select(-id,-date)
# 
# grouped.sp<- grouped.sp%>%  
#   group_by(month,topic) %>%
#   summarise(sum_probability=mean(probability))
# 
# 
# global.topic.probab <- sum.probab %>% 
#   gather(topic, probability, t_1:t_5) %>% ##topic_number k_list
#   tidyr::separate(topic, into =c("t","topic")) %>% 
#   select(-t)%>%
#   select(-id,-date)
# 
# global.topic.probab <- global.topic.probab %>%  
#   group_by(topic) %>%
#   summarise(global_probability=mean(probability))
# 
# g<-ggplot(global.topic.probab, aes(x=topic,y=global_probability))+ 
#   geom_bar(stat="identity",position="stack")+
#   ylim(0.0, (max(global.topic.probab$global_probability)+0.05))+
#   theme(axis.text.x = element_text(angle = 90))+
#   ggtitle(paste0("Overall probability per topic"))
# ggsave(paste0(plots_dir,"Comments_Global-probability-per-topic-t",k_list,".pdf"))
# 
# grouped.glob.top <- merge(grouped.sp, global.topic.probab, by="topic")
# 
# for(i in 1:max(as.numeric(grouped.sp$topic))){
#   topic <- grouped.glob.top %>% filter(topic==i)
#   tgp<- grouped.glob.top[grouped.glob.top$topic==i,]$global_probability
#   g<-ggplot(topic, aes(x=month,y=sum_probability))+ 
#     geom_bar(stat="identity",position="stack")+
#     ylim(0.0, (max(grouped.sp$sum_probability)+0.05))+
#     geom_hline(yintercept = tgp, lty="dashed")+
#     theme(axis.text.x = element_text(angle = 90))+
#     ggtitle(paste0("Probab of topic",i,"aggregated by month"))+
#     xlab("month")+
#     ylab("probability")
#   
#   ggsave(paste0(plots_dir,"Comments_Topic-",i,"+global.pdf"))
#   
# }
# 
# 
