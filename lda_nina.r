library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(wordcloud)


####selected parameters to check the results####
k_list <- c(5,10) #### vector with cluster numbers (here k=5 and k=10 in one run)
# k_list<-5 ### here only 1 exp with cluster number k = 5
alpha<-0.1 # 0.alpha value
ngram<- 1 #ngrams

data_name<-"guardian-articles"
data_dir<-"./data/guardian/full_articles_guardian.csv" #data path
model_dir<-paste0("./test") ###dir for model to be saved

exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram) 

coherence_name<- paste0("coherence-",exp_name,".pdf") ###name of file with coherence plot

model_name <- paste0("_topics-",exp_name, ".rda") ### name of trained model

custom.stopwords <- c("rt","amp","mt","climate","change","climatechange","jan","feb","mar","apr","may",
                      "june","july","aug","sept","oct","nov","dec", "say","said")
##################


if(data_name %like% "twitter"){
  data <- read.csv(data_dir, stringsAsFactors = FALSE, sep=",", quote = "\"",  header = TRUE)#colClasses = c("factor","character"))#, encoding = "UTF-8")
  # data$id<- 1:nrow(data)
  data$date<-NULL
  data$from_user_id<-NULL
  data$from_user_name<-NULL
  data$from_user_followercount<-NULL
}else{
  data<- read.csv2(data_dir, stringsAsFactors = FALSE)
}

#CreateDtm by default has all below parameters true:
# remove_punctuation = TRUE,
# remove_numbers = TRUE,
# lower = TRUE,

dtm <- CreateDtm(data$text, 
                 doc_names = data$id, 
                 stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart"), custom.stopwords),
                 ngram_window = c(1, ngram))
original_tf <- TermDocFreq(dtm = dtm)


# ##alpha=0.1
run.model.fun1 <- function(k){ 
  filename = file.path(model_dir, paste0(k, model_name)) 
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = alpha) ### iterations = 500 even out the coherence score but they are computationally and time consuming
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    print("model file found!")
    # load(filename)
  }
  m
}

model_list <- TmParallelApply(X = k_list, FUN = run.model.fun1, cpus=4) ### if memory problem - decrease cpu numbers

coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)),
                            coherence = sapply(model_list, function(x) mean(x$coherence)),
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,max(k_list),1)) + ylab("Coherence")

ggsave(file.path(model_dir,coherence_name),plot = g,  device = "pdf")




############### getting topics 


# rows of phi = topics; columns = tokens (words). 
# rows of theta = documents; columns = topics.


model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]] ### choose the k that has max coherence
# model<-model_list[[1]] ### alternative is to chose first/second/... position on the list of models (order matches order of k in k_list vector)
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

##Dendrogram
model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
pdf(paste0(model_dir,"/dendrogram-k-",k_list,"-",exp_name,".pdf"))
plot(model$hclust)
dev.off()

##Topics
#visualising topics of words based on the max value of phi
pdf(paste0(model_dir,"/cluster-k-",k_list,"-",exp_name,".pdf"))
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

