library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(wordcloud)
library(tidyr)
library(data.table)
library(tidyverse)


####selected parameters to check the results####
# k_list <- seq(1, 1, by = 1)
k_list<-seq(1,15,by=1) #cluster number
ngram<-1
alpha<-0.1 # 0.alpha value

coherence_dir <- paste0("./results/guardian-lda/comments/coherence/ngram_1:",ngram) ##directory of models
model_dir <- paste0("./results/guardian-lda/comments/models/ngram_1:",ngram,"/alpha_",alpha)
name<-paste0("_k",k_list,"_ngram",ngram, "_al",alpha)

##################
csv_data <- read.csv2("./data/full_comments_guardian.csv", stringsAsFactors = FALSE)
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


if (!dir.exists(model_dir)){ dir.create(model_dir)}
if (!dir.exists(coherence_dir)){ dir.create(coherence_dir)}


##alpha=0.1

run.model.fun1 <- function(k){  ##alpha
  filename = file.path(model_dir, paste0(k, "_topics_a",alpha,".rda")) ##alpha
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = alpha) ##alpha
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    # load(filename)
    print("files exist!")
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

ggsave(file.path(coherence_dir, paste0("coherence_al",alpha,"_ngram",ngram,".pdf")),plot = g) ##alpha
