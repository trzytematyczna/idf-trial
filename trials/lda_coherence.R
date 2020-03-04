
library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)

csv_data <- read.csv2("./data/full_articles_guardian.csv", stringsAsFactors = FALSE)
csv_data<-csv_data[!is.na(csv_data$text),]
data<-csv_data
data$text<-as.character(data$text)

data.corpus <- corpus(data, docid_field = "id", text_field = "text")

doc.tokens <- tokens(data.corpus)

stopwords1 <- c("said", "saying")

doc.tokens <- doc.tokens%>% tokens(remove_punct = TRUE, remove_numbers = TRUE, 
                                   remove_separators = TRUE) %>% 
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
data.topicmodels <- convert(data.trimmed, to = "topicmodels")

# as(as.matrix(data.trimmed), "dgCMatrix")
# dfmSparse <- dfm(inaugTexts, verbose=FALSE)
# str(as.matrix(data.trimmed))
# class(as.matrix(data.trimmed))

dtm=as(as.matrix(data.trimmed), "dgCMatrix")
# tf <- TermDocFreq(dtm = dtm) 
# original_tf <- tf %>% select(term, term_freq,doc_freq)
# rownames(original_tf) <- 1:nrow(original_tf)

# Eliminate words appearing less than 2 times or in more than half of the documents
# vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
# dtm = dtm

k_list <- seq(1, 30, by = 1)
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
