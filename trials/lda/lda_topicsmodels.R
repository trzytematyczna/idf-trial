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


text_cleaning_tokens <- data %>% unnest_tokens(word, text, to_lower = TRUE)
text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>%
  anti_join(stop_words)
tokens <- text_cleaning_tokens %>% filter(!(word==""))
# 
tokens <- tokens %>% mutate(ind = row_number()) %>% group_by(id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] <- ""

tokens<-tokens%>% select(-type, -url, -authors, -authors_nb, -section, -tags, -tags_nb, -date_published, 
                         -date_modified, -share_count, -comment_nb, -title, -description)
tokens <-unite(tokens, text, -id, sep=" ")
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
 # original_tf <- tf %>% select(term, term_freq,doc_freq)
 # rownames(original_tf) <- 1:nrow(original_tf)


# Eliminate words appearing less than 2 times or in more than half of the documents max_doc_fq = 0.5 
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
 dtm = dtm

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
