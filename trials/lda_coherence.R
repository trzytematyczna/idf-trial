
library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)

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

data.dfm <- dfm(doc.tokens, ngrams=1:2)

# featnames(data.dfm)
# topfeatures(data.dfm, 5)
# head(kwic(doc.tokens, "love", window = 3))

data.trimmed <- data.dfm %>% dfm_trim(min_termfreq = 0.9, termfreq_type = "quantile", 
                                      min_docfreq = 0.01, max_docfreq = 0.5, docfreq_type = "prop")

data.trimmed <- data.trimmed[ntoken(data.trimmed) > 0,]

data.trimmed
# data.topicmodels <- convert(data.trimmed, to = "topicmodels")
doctermmatrix <- convert(data.trimmed, to = "tm")

# as(as.matrix(data.trimmed), "dgCMatrix")
# dfmSparse <- dfm(inaugTexts, verbose=FALSE)
# str(as.matrix(data.trimmed))
# class(as.matrix(data.trimmed))

# tf <- TermDocFreq(dtm = dtm) 
# original_tf <- tf %>% select(term, term_freq,doc_freq)
# rownames(original_tf) <- 1:nrow(original_tf)
tf <- textstat_frequency(data.trimmed)
colnames(tf)<-c("term","term_freq","rank","doc_freq","group")
original_tf<-tf%>% select(term,term_freq, doc_freq)

dtm=as(as.matrix(data.trimmed), "dgCMatrix") ####TOCHECK

# k_list <- seq(1, 30, by = 1)
k_list<-8
model_dir <- paste0("models_lda")

if (!dir.exists(model_dir)){ dir.create(model_dir)}

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines

coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

ggsave("./coherence.pdf",plot = g)

model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
top20_wide <- as.data.frame(model$top_terms)

model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
plot(model$hclust)


#1. Top 20 terms based on phi  ---------------------------------------------
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
top20_wide <- as.data.frame(model$top_terms)

#3. word, topic relationship ---------------------------------------------
#looking at the terms allocated to the topic and their pr(word|topic)
allterms <-data.frame(t(model$phi))
allterms$word <- rownames(allterms)
rownames(allterms) <- 1:nrow(allterms)
allterms <- melt(allterms,idvars = "word") 
allterms <- allterms %>% rename(topic = variable)
FINAL_allterms <- allterms %>% group_by(topic) %>% arrange(desc(value))


# 2. Topic,word,freq ------------------------------------------------------
final_summary_words <- data.frame(top_terms = t(model$top_terms))
final_summary_words$topic <- rownames(final_summary_words)
rownames(final_summary_words) <- 1:nrow(final_summary_words)
final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))
final_summary_words <- final_summary_words %>% rename(word = value) %>% select(-variable)
final_summary_words <- left_join(final_summary_words,allterms)
final_summary_words <- final_summary_words %>% group_by(topic,word) %>%
  arrange(desc(value))
final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))

#4. per-document-per-topic probabilities ----------------------------------------------
#trying to see the topic in each document
theta_df <- data.frame(model$theta)
theta_df$document <-rownames(theta_df) 
rownames(theta_df) <- 1:nrow(theta_df)
# theta_df$document <- as.numeric(theta_df$document)
theta_df <- melt(theta_df,id.vars = "document")
theta_df <- theta_df %>% rename(topic = variable) 
theta_df <- theta_df %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
FINAL_document_topic <- theta_df %>% group_by(document) %>% 
  arrange(desc(value)) %>% filter(row_number() ==1)

#5. Visualising of topics in a dendrogram ----------------------------------------------
#probability distributions called Hellinger distance, distance between 2 probability vectors
model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
plot(model$hclust)


#visualising topics of words based on the max value of phi
set.seed(1234)
pdf("cluster.pdf")
for(i in 1:length(unique(final_summary_words$topic)))
{  wordcloud(words = subset(final_summary_words ,topic == i)$word, freq = subset(final_summary_words ,topic == i)$value, min.freq = 1,
             max.words=200, random.order=FALSE, rot.per=0.35, 
             colors=brewer.pal(8, "Dark2"))
}

dev.off()
