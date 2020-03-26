library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(wordcloud)
library(tidyr)
library(data.table)

####selected parameters to check the results####

k_list<-10 #cluster number
alpha<-1 # 0.alpha value
wid<-2 #for number of leading zeros in models
ngram<-2 #ngrams
al<- alpha%>% formatC(width=wid, flag = "0")
plots_dir <- paste0("./plots/lda/") ##directory of plots
model_dir <- paste0("./results/lda/articles/models/ngram_1:",ngram,"/alpha_",al)
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

data.trimmed <- data.dfm %>% dfm_trim(min_docfreq = 0.01, max_docfreq = 0.5, docfreq_type = "prop")
### min_termfreq = 0.9, termfreq_type = "quantile",

data.trimmed <- data.trimmed[ntoken(data.trimmed) > 0,]

dtm=as(as.matrix(data.trimmed), "dgCMatrix")
original_tf <- TermDocFreq(dtm = dtm)
tf_bigrams <- original_tf[ stringr::str_detect(original_tf$term, "_") , ]

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


csv_data.comments <- read.csv2("./data/full_comments_guardian.csv", stringsAsFactors = FALSE)
csv_data.comments<-csv_data.comments[!is.na(csv_data.comments$text),]
data.comments<-csv_data.comments
data.comments$text<-as.character(data.comments$text)


comments.ipcc <- data.comments[data.comments$text %like% "IPCC", ] %>% 
  rbind(data.comments[data.comments$text %like% "ipcc", ]) %>% 
  rbind(data.comments[data.comments$text %like% "Intergovernmental Panel on Climate Change", ]) %>% 
  rbind(data.comments[data.comments$text %like% "intergovernmental panel on climate change", ]) %>% 
  rbind(data.comments[data.comments$text %like% "UN Environment Programme", ]) %>%
  distinct(id, .keep_all = TRUE)
length(unique(comments.ipcc$id)) #4838

length(unique(comments.ipcc$article_id)) #1226

articles.ipcc <- data[data$text %like% "IPCC", ] %>% 
  rbind(data[data$text %like% "ipcc", ]) %>%
  rbind(data[data$text %like% "intergovernmental panel on climate change", ]) %>%
  rbind(data[data$text %like% "Intergovernmental Panel on Climate Change", ]) %>%
  rbind(data[data$text %like% "UN Environment Programme", ]) %>%
  distinct(id, .keep_all = TRUE)
length(unique(articles.ipcc$id)) #352


articles.ipcc$date_published<-as.numeric(articles.ipcc$date_published)
articles.ipcc$date_published<- as.Date(as.POSIXct((articles.ipcc$date_published/1000), origin = "1970-01-01"))


articles.from.ipcc.comments<-document_topic[document_topic$document %in% comments.ipcc$article_id,]
names(articles.from.ipcc.comments)[names(articles.from.ipcc.comments) == "document"] <- "id" 
articles.from.ipcc.comments <- merge(articles.from.ipcc.comments,(select(data,id,date_published)), by="id")
articles.from.ipcc.comments$date_published<-as.numeric(articles.from.ipcc.comments$date_published)
articles.from.ipcc.comments$date_published<- as.Date(as.POSIXct((articles.from.ipcc.comments$date_published/1000), origin = "1970-01-01"))

grouped<-articles.from.ipcc.comments %>% 
  group_by(date_published,topic) %>% 
  summarize(n=n()) #%>%
  # mutate(f=n/sum(n)) %>% plyr::rename(c("f"="perc"))
# ggplot(grouped, aes(fill=topic, y=n, x=date_published)) + 
#   geom_bar(position="dodge", stat="identity")

for(i in 2016:2019){
  topics.yearly <- grouped %>% 
    filter(date_published >= as.Date(paste0(i,"-01-01")) &
             date_published < as.Date(paste0((i+1),"-01-01")))
  g<-ggplot(topics.yearly, aes(fill=topic, y=n, x=date_published)) + 
    geom_bar(position="dodge", stat="identity")+
    xlab("Date") + 
    # ylab("# of articles") +
    ggtitle(paste0("Topics distribution per date for ",i," topics no ", k_list," ngram ",
                   ngram," alpha ", formatC(al, width=2, flag = "0")))
  
  ggsave(paste0(plots_dir,"topics-yearly-normalized-",i,".pdf"))
}

grouped.m <- articles.from.ipcc.comments %>% 
  group_by(format(date_published,'%Y-%m'),topic) %>% 
  summarize(n=n()) %>%
  plyr::rename(c("format(date_published, \"%Y-%m\")"="month"))

g<-ggplot(grouped.m, aes(fill=topic, y=n, x=month)) + 
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle(paste0("IPCC articles Topics distribution per month (topics no ", k_list," ngram ",
                 ngram," alpha ", formatC(al, width=2, flag = "0"),")"))
ggsave(paste0(plots_dir,"ipcc-articles-per-topic-yearly.pdf"))

for(i in 2016:2019){
  topics.yearly.m <- grouped.m %>% 
    filter(as.Date(paste0(month,"-01")) >= as.Date(paste0(i,"-01-01")) &
             as.Date(paste0(month,"-01")) < as.Date(paste0((i+1),"-01-01")))
  g<-ggplot(topics.yearly.m, aes(fill=topic, y=n, x=month)) + 
    geom_bar(position="dodge", stat="identity")+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(paste0("Topics distribution per month for ",i," topics no ", k_list," ngram ",
                   ngram," alpha ", formatC(al, width=2, flag = "0")))
  ggsave(paste0(plots_dir,"ipcc-articles-per-topic-",i,".pdf"))
}



