library(dplyr)
library(textmineR)
library(tidytext)
library(tidyr)
library(topicmodels)
library(ggplot2)

csv_data <- read.csv2("./data/full_articles_guardian.csv", stringsAsFactors = FALSE)
csv_data<-csv_data[!is.na(csv_data$text),]
data<-csv_data
data$text<-as.character(data$text)

# data$text <- sub("RT.*:", "", data$text)
# data$text <- sub("@.* ", "", data$text)
text_cleaning_tokens <- data %>% unnest_tokens(word, text, to_lower = TRUE)
# text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
# text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
# text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
#   anti_join(stop_words)
tokens <- text_cleaning_tokens %>% filter(!(word==""))

tokens <- tokens %>% mutate(ind = row_number())###
tokens <- tokens %>% group_by(id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] <- ""

tokens<-tokens%>% select(id,word,ind)
# tokens <-unite(tokens, text, )
tokens <- tidyr::unite(tokens, text,-id,sep =" " )
tokens$text <- trimws(tokens$text)

custom.stopwords <- c("said", "saying")

dtm <- CreateDtm(tokens$text, 
                 doc_names = tokens$id, 
                 remove_punctuation = TRUE,
                 remove_numbers = TRUE,
                 lower = TRUE,
                 stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart"), custom.stopwords),
                 ngram_window = c(1, 2))
# image(dtm[1:500,1:500])

tf <- TermDocFreq(dtm = dtm) 
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)

# data.trimmed <- data.dfm %>% dfm_trim(min_termfreq = 0.9, termfreq_type = "quantile", 
#                                       min_docfreq = 0.01, max_docfreq = 0.5, docfreq_type = "prop")
# as(as.matrix(data.trimmed), "dgCMatrix")
# dfmSparse <- dfm(inaugTexts, verbose=FALSE)
# str(as.matrix(data.trimmed))
# class(as.matrix(data.trimmed))


# Eliminate words appearing less than 2 times or in more than half of the documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
# dtm = dtm

k_list <- seq(1, 20, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))

if (!dir.exists(model_dir)) dir.create(model_dir)

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

#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

ggsave("./coherence.pdf",plot = g)
