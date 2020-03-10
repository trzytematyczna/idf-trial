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

data.dfm <- dfm(doc.tokens, ngrams=1:1)

# featnames(data.dfm)
# topfeatures(data.dfm, 5)
# head(kwic(doc.tokens, "love", window = 3))

data.trimmed <- data.dfm %>% dfm_trim(min_docfreq = 0.01, max_docfreq = 0.5, docfreq_type = "prop")
### min_termfreq = 0.9, termfreq_type = "quantile", 

data.trimmed <- data.trimmed[ntoken(data.trimmed) > 0,]

# data.trimmed

# data.topicmodels <- convert(data.trimmed, to = "topicmodels")

# as(as.matrix(data.trimmed), "dgCMatrix")
# dfmSparse <- dfm(inaugTexts, verbose=FALSE)
# str(as.matrix(data.trimmed))

# tf <- TermDocFreq(dtm = dtm) 
# original_tf <- tf %>% select(term, term_freq,doc_freq)
# rownames(original_tf) <- 1:nrow(original_tf)

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

k_list <- seq(1, 25, by = 1)
# k_list<-8
model_dir <- paste0("models_lda_ngram1")

if (!dir.exists(model_dir)){ dir.create(model_dir)}

run.model.fun1 <- function(k){
  filename = file.path(model_dir, paste0(k, "_topics_a01.rda"))
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = 0.1)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}

run.model.fun3 <- function(k){
  filename = file.path(model_dir, paste0(k, "_topics_a03.rda"))
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = 0.3)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}


run.model.fun5 <- function(k){
  filename = file.path(model_dir, paste0(k, "_topics_a05.rda"))
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = 0.5)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}




run.model.fun7 <- function(k){
  filename = file.path(model_dir, paste0(k, "_topics_a07.rda"))
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = 0.7)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}


model_list <- TmParallelApply(X = k_list, FUN = run.model.fun1)

coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

ggsave("./coherence_al01_ngram1.pdf",plot = g)



model_list <- TmParallelApply(X = k_list, FUN = run.model.fun3)


coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

ggsave("./coherence_al03_ngram1.pdf",plot = g)


model_list <- TmParallelApply(X = k_list, FUN = run.model.fun5)


coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

ggsave("./coherence_al05_ngram1.pdf",plot = g)


model_list <- TmParallelApply(X = k_list, FUN = run.model.fun7)


coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,max(k_list),1)) + ylab("Coherence")

ggsave("./coherence_al07_ngram1.pdf",plot = g)


