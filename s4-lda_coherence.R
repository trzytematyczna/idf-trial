library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(wordcloud)
library(tidyr)

####selected parameters to check the results####

k_list<-10 #cluster number
alpha<-1 # 0.alpha value
wid<-2 #for number of leading zeros in models
ngram<-2 #ngrams
al<- alpha%>% formatC(width=wid, flag = "0")
plots_dir <- paste0("./plots/lda/") ##directory of plots
model_dir <- paste0("./results/lda/models/ngram_1:",ngram,"/alpha_",al)
name<-paste("_ngram",ngram, "_al",formatC(al, width=2, flag = "0"), "_k",k_list, sep="")

##################


csv_data <- read.csv2("./data/full_articles_guardian.csv", stringsAsFactors = FALSE)
csv_data<-csv_data[!is.na(csv_data$text),]
data<-csv_data
data$text<-as.character(data$text)

data.corpus <- corpus(data, docid_field = "id", text_field = "text")
doc.tokens <- tokens(data.corpus)
stopwords1 <- c("said", "saying")
doc.tokens <- doc.tokens%>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE) %>%
  tokens_tolower() %>%
  tokens_select(c(stopwords(source='smart'),stopwords1, stopwords("french")),selection='remove')
data.dfm <- dfm(doc.tokens, ngrams=1:ngram)

# featnames(data.dfm)
# topfeatures(data.dfm, 5)
# head(kwic(doc.tokens, "love", window = 3))

data.trimmed <- data.dfm %>% dfm_trim(min_docfreq = 0.01, max_docfreq = 0.5, docfreq_type = "prop")
### min_termfreq = 0.9, termfreq_type = "quantile",

data.trimmed <- data.trimmed[ntoken(data.trimmed) > 0,]

# data.trimmed

# tf <- textstat_frequency(data.trimmed)
# colnames(tf)<-c("term","term_freq","rank","doc_freq","group")
# original_tf<-tf%>% select(term,term_freq, doc_freq)

dtm=as(as.matrix(data.trimmed), "dgCMatrix")
original_tf <- TermDocFreq(dtm = dtm)
# dtm<-dtm[ , original_tf$term_freq > 3 ]


# str(tf_mat)
# look at the most frequent bigrams
tf_bigrams <- original_tf[ stringr::str_detect(original_tf$term, "_") , ]
# head(tf_bigrams[ order(tf_bigrams$term_freq, decreasing = TRUE) , ], 10)

# k_list <- seq(1, 25, by = 1)
if (!dir.exists(model_dir)){ dir.create(model_dir)}
if (!dir.exists(coherence_dir)){ dir.create(coherence_dir)}


run.model.fun <- function(k){
  filename = file.path(model_dir, paste0(k, "_topics_a",al,".rda"))
  if (!file.exists(filename)) {
    print("Nofile!")
  } else {
    load(filename)
  }
  m
}

model_list <- TmParallelApply(X = k_list, FUN = run.model.fun)


# rows of phi = topics; columns = tokens. 
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
  melt(idvars = "word") %>%
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
  melt(id.vars = "document") %>% 
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
pdf(paste0("./results/lda/dendrogram",name,".pdf"))
plot(model$hclust)
dev.off()

#visualising topics of words based on the max value of phi
# set.seed(1234)
pdf(paste("./results/lda/cluster",name,".pdf",sep=""))
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

selected.articles<-read.csv2("./results/tfidf/guardian_articles_selected_tfidf.csv",stringsAsFactors = FALSE)

selected.topic <- document_topic %>% filter(document %in% selected.articles$id) %>%
  select(topic) %>%
  plyr::rename(c("document"="id"))

selected.articles<-merge(selected.topic,selected.articles,by="id")

selected.articles %>% write.csv2(paste0("./results/lda/guardian_articles_selected_lda", name,".csv"))

######

dt <- document_topic %>% select(document,topic) %>% plyr::rename(c("document"="id"))

topic.date <- merge(dt,data, by="id") %>% select(id,topic,date_published)
topic.date$date_published<-as.numeric(topic.date$date_published)
topic.date$date_published<- as.Date(as.POSIXct((topic.date$date_published/1000), origin = "1970-01-01"))


#####NORMALIZED YEARLY
grouped<-topic.date %>% 
  group_by(date_published,topic) %>% 
  summarize(n=n()) %>%
  mutate(f=n/sum(n)) %>% plyr::rename(c("f"="perc"))

for(i in 2016:2019){
  topics.yearly <- grouped %>% 
    filter(date_published >= as.Date(paste0(i,"-01-01")) &
             date_published < as.Date(paste0((i+1),"-01-01")))
  g <- ggplot(data=topics.yearly,aes(x=date_published,fill=factor(topic),y=perc)) + 
    geom_bar(stat="identity",position="stack") +
    xlab("Date") + 
    # ylab("# of articles") +
    ggtitle(paste0("Topics distribution per date for ",i," topics no ", k_list," ngram ",
                   ngram," alpha ", formatC(al, width=2, flag = "0")))
  
  ggsave(paste0(plots_dir,"topics-yearly-normalized-",i,".pdf"))
}
############not normalized


grouped<-topic.date %>% 
  group_by(date_published,topic) %>% 
  summarize(n=n()) #%>%
  # mutate(f=n/sum(n)) %>% plyr::rename(c("f"="perc"))

for(i in 2016:2019){
  topics.yearly <- grouped %>% 
    filter(date_published >= as.Date(paste0(i,"-01-01")) &
             date_published < as.Date(paste0((i+1),"-01-01")))
  g <- ggplot(data=topics.yearly,aes(x=date_published,fill=factor(topic),y=n)) + 
    geom_bar(stat="identity",position="stack") +
    xlab("Date") + 
    # ylab("# of articles") +
    ggtitle(paste0("Topics distribution per date for ",i," topics no ", k_list," ngram ",
                   ngram," alpha ", formatC(al, width=2, flag = "0")))
  
  ggsave(paste0(plots_dir,"topics-yearly-sum-",i,".pdf"))
}


####normalized by sum of articles in each month
grouped.m <- topic.date %>% 
  group_by(format(date_published,'%Y-%m'),topic) %>% 
  summarize(n=n()) %>%
  mutate(f=n/sum(n)) %>%
  plyr::rename(c("f"="perc"))%>%
  plyr::rename(c("format(date_published, \"%Y-%m\")"="month"))

for(i in 2016:2019){
  topics.yearly.m <- grouped.m %>% 
    filter(as.Date(paste0(month,"-01")) >= as.Date(paste0(i,"-01-01")) &
          as.Date(paste0(month,"-01")) < as.Date(paste0((i+1),"-01-01")))
  g <- ggplot(data=topics.yearly.m,aes(x=month,fill=factor(topic),y=perc)) + 
    geom_bar(stat="identity",position="stack")+
    theme(axis.text.x = element_text(angle = 45))+
  ggtitle(paste0("Topics distribution per month for ",i," topics no ", k_list," ngram ",
                 ngram," alpha ", formatC(al, width=2, flag = "0")))
  ggsave(paste0(plots_dir,"topics-yearly-monthly-grouped-normalized-",i,".pdf"))
}

#############not normalized -- number of articles agregated month in each topic
grouped.m <- topic.date %>% 
  group_by(format(date_published,'%Y-%m'),topic) %>% 
  summarize(n=n()) %>%
  # mutate(f=n/sum(n)) %>% 
  # plyr::rename(c("f"="perc"))%>%
  plyr::rename(c("format(date_published, \"%Y-%m\")"="month"))

for(i in 2016:2019){
  topics.yearly.m <- grouped.m %>% 
    filter(as.Date(paste0(month,"-01")) >= as.Date(paste0(i,"-01-01")) &
             as.Date(paste0(month,"-01")) < as.Date(paste0((i+1),"-01-01")))
  g <- ggplot(data=topics.yearly.m,aes(x=month,fill=factor(topic),y=n)) + 
    geom_bar(stat="identity",position="stack")+
    theme(axis.text.x = element_text(angle = 45))+
    ggtitle(paste0("Topics distribution per month for ",i," topics no ", k_list," ngram ",
                   ngram," alpha ", formatC(al, width=2, flag = "0")))
  ggsave(paste0(plots_dir,"topics-yearly-monthly-grouped-sum-",i,".pdf"))
}


###########aggregation month

sp <- data.frame(model$theta)
sp$document <-rownames(sp) 
rownames(sp) <- 1:nrow(sp)
sp <- sp%>%  plyr::rename(c("document"="id"))

sum.probab <- merge(sp,(select(data,id,date_published)), by="id")

sum.probab$date_published<-as.numeric(sum.probab$date_published)
sum.probab$date_published<- as.Date(as.POSIXct((sum.probab$date_published/1000), origin = "1970-01-01"))


grouped.sp <- sum.probab %>% 
  mutate(month=format(date_published,'%y-%m')) %>%
  gather(topic, probability, t_1:t_10) %>%
  tidyr::separate(topic, into =c("t","topic")) %>% 
  select(-t)%>%
  select(-id,-date_published)

grouped.sp<- grouped.sp%>%  
  group_by(month,topic) %>%
  summarise(sum_probability=mean(probability))


g<-ggplot(grouped.sp, aes(x=month,fill=topic,y=sum_probability))+ 
  geom_bar(stat="identity",position="stack")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle(paste0("Avg of probabilities of topics in articles aggregated by month"))
# facet_grid(~topic)
 ggsave(paste0(plots_dir,"sum-probabilities-per-month_normalized.pdf"), plot = g)
g

 
 
 
###########aggregation week

sp <- data.frame(model$theta)
sp$document <-rownames(sp) 
rownames(sp) <- 1:nrow(sp)
sp <- sp%>%  plyr::rename(c("document"="id"))

sum.probab <- merge(sp,(select(data,id,date_published)), by="id")

sum.probab$date_published<-as.numeric(sum.probab$date_published)
sum.probab$date_published<- as.Date(as.POSIXct((sum.probab$date_published/1000), origin = "1970-01-01"))


grouped.sp <- sum.probab %>% 
  mutate(week=format(date_published,'%y-%V')) %>%
  gather(topic, probability, t_1:t_10) %>%
  tidyr::separate(topic, into =c("t","topic")) %>% 
  select(-t)%>%
  select(-id,-date_published)

grouped.sp<- grouped.sp%>%  
  group_by(week,topic) %>%
  summarise(sum_probability=sum(probability))


g<-ggplot(grouped.sp, aes(x=week,fill=topic,y=sum_probability))+ 
  geom_bar(stat="identity",position="stack")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle(paste0("Sum of probabilities of topics in articles aggregated by week"))
# facet_grid(~topic)
ggsave(paste0(plots_dir,"sum-probabilities-per-week.pdf"), plot = g)
g
