library(jsonlite)
library(tidytext)
library(dplyr)
library(ggplot2)
library(quanteda)
library(topicmodels)
library(lubridate)

json_data <- fromJSON(txt = "uk_parliament_climatechange.json")$speeches

dfm_discussion_words <- json_data$text %>% 
  dfm(remove_punct = TRUE, remove =stopwords('en')) %>% 
  dfm_remove(c('^[0-9]*$')) %>%
  dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile", 
         max_docfreq = 0.1, docfreq_type = "prop")

dfm_discussion_words <- dfm_discussion_words[ntoken(dfm_discussion_words) > 0,]

dtm <- convert(dfm_discussion_words, to = "topicmodels")
lda <- topicmodels::LDA(dtm, k = 10)

terms(lda, 10)
