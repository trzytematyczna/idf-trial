require(jsonlite)
require(tidytext)
require(dplyr)
require(ggplot2)
require(stringr)
library(rlang)


# data <- df with corpus of the text
# 1. each document can be identified by column named documentColumn 
# 2. each document's text is in column named "text"
# 3. text of each document is a string --> data$text<-as.character(data$text)

run_tfidf <- function(data, documentColumn, roots = FALSE) {
  
  data$text<-as.character(data$text)
  
  if (roots == TRUE){
    wordsFreq <- data %>% 
      unnest_tokens(word, text, to_lower = TRUE) %>%
      filter(!str_detect(word, "^[0-9]*$")) %>%
      anti_join(stop_words) %>% #removing stop words
      mutate(word = SnowballC::wordStem(word)) %>% # tokens -> roots (Porter stemming algorithm)
      count(!!sym(documentColumn), word, sort = TRUE)
  }
  else{
  wordsFreq <- data %>% 
    unnest_tokens(word, text, to_lower = TRUE) %>%
    filter(!str_detect(word, "^[0-9]*$")) %>%
    anti_join(stop_words) %>% #removing stop words
    count(!!sym(documentColumn), word, sort = TRUE)
  }

    tfidf_data <- wordsFreq %>%
    bind_tf_idf(word, !!sym(documentColumn), n) %>%
    arrange(desc(!!sym(documentColumn), tf_idf))  
  
  return(tfidf_data)
}

run_topn <- function(tfidf_data, documentColumn, number_of_top){ ##can return more than n rows if there are ties in scores
 
    topn_data <- tfidf_data %>% 
      group_by(!!sym(documentColumn)) %>% 
      arrange(!!sym(documentColumn), desc(tf_idf)) %>% 
      top_n(n=number_of_top, wt=tf_idf)
    return(topn_data)
}


