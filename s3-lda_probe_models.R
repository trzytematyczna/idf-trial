library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)

###### <parameters>
ngram<-2 #1:ngram
k_list <- seq(1, 10, by = 1) #nb of clusters to check
# k_list<-8
model_dir <- paste0("./results/lda/models/ngram_1:",ngram) ##directory of models

###### </parameters>


###<data>

stopwords1 <- c("said", "saying")

csv_data <- read.csv2("./data/full_articles_guardian.csv", stringsAsFactors = FALSE)
csv_data<-csv_data[!is.na(csv_data$text),]
data<-csv_data
data$text<-as.character(data$text)

data.corpus <- corpus(data, docid_field = "id", text_field = "text")
doc.tokens <- tokens(data.corpus)
doc.tokens <- doc.tokens%>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_select(c(stopwords(source='smart'),stopwords1, stopwords("french")),selection='remove')
data.dfm <- dfm(doc.tokens, ngrams=1:ngram)

data.trimmed <- data.dfm %>% dfm_trim(min_docfreq = 0.01, max_docfreq = 0.5, docfreq_type = "prop")
### min_termfreq = 0.9, termfreq_type = "quantile", 

data.trimmed <- data.trimmed[ntoken(data.trimmed) > 0,]

dtm=as(as.matrix(data.trimmed), "dgCMatrix") 
original_tf <- TermDocFreq(dtm = dtm)
# dtm<-dtm[ , original_tf$term_freq > 3 ]


####</data>

####<experiments>

if (!dir.exists(model_dir)){ dir.create(model_dir)}

##alpha=0.05

run.model.fun05 <- function(k){  ##alpha
  filename = file.path(model_dir, paste0(k, "_topics_a005.rda")) ##alpha
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = 0.05) ##alpha
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}

model_list <- TmParallelApply(X = k_list, FUN = run.model.fun05, cpus=1) ##alpha

coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)),
                            coherence = sapply(model_list, function(x) mean(x$coherence)),
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,max(k_list),1)) + ylab("Coherence")

ggsave(paste0("./results/lda/coherence/ngram_1:",ngram,"/coherence_al005_ngram",ngram,".pdf"),plot = g) ##alpha


##</alpha=0.05


##alpha=0.1

run.model.fun1 <- function(k){  ##alpha
  filename = file.path(model_dir, paste0(k, "_topics_a01.rda")) ##alpha
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = 0.1) ##alpha
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}

model_list <- TmParallelApply(X = k_list, FUN = run.model.fun1, cpus=1) ##alpha

coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,max(k_list),1)) + ylab("Coherence")

ggsave(paste0("./results/lda/coherence/ngram_1:",ngram,"/coherence_al01_ngram",ngram,".pdf"),plot = g) ##alpha


##</alpha=0.1


##alpha=0.3

run.model.fun3 <- function(k){ ##alpha
  filename = file.path(model_dir, paste0(k, "_topics_a03.rda")) ##alpha
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = 0.3) ##alpha
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}


model_list <- TmParallelApply(X = k_list, FUN = run.model.fun3, cpus=1) ##alpha


coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,max(k_list),1)) + ylab("Coherence")

ggsave(paste0("./results/lda/coherence/ngram_1:",ngram,"/coherence_al03_ngram",ngram,".pdf"),plot = g) ##alpha

##</alpha=0.3


##alpha=0.5


run.model.fun5 <- function(k){ ##alpha
  filename = file.path(model_dir, paste0(k, "_topics_a05.rda")) ##alpha
  if (!file.exists(filename)) { 
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = 0.5) ##alpha
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}
model_list <- TmParallelApply(X = k_list, FUN = run.model.fun5, cpus=1) ##alpha


coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

ggsave(paste0("./results/lda/coherence/ngram_1:",ngram,"/coherence_al05_ngram",ngram,".pdf"),plot = g) ##alpha

##</alpha=0.5


##alpha=0.7

run.model.fun7 <- function(k){ ##alpha
  filename = file.path(model_dir, paste0(k, "_topics_a07.rda")) ##alpha
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = 0.7) ##alpha
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}
model_list <- TmParallelApply(X = k_list, FUN = run.model.fun7, cpus=1) ##alpha


coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,max(k_list),1)) + ylab("Coherence")

ggsave(paste0("./results/lda/coherence/ngram_1:",ngram,"./coherence_al07_ngram",ngram,".pdf"),plot = g) ##alpha

##</alpha=0.7


