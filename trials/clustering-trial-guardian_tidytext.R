library(tm)

csv_data <- read.csv2("./data/full_articles_guardian.csv", stringsAsFactors = FALSE)
data<-csv_data
data$text<-as.character(data$text)
# asd <- data %>% unnest_tokens(word, text, to_lower = TRUE)

# sentences <- sub("http://([[:alnum:]|[:punct:]])+", '', dataframe$content) 

corpus = tm::Corpus(tm::VectorSource(data$text)) 

# Cleaning up 
# Handling UTF-8 encoding problem from the dataset 
corpus.cleaned <- tm::tm_map(corpus, function(x) iconv(x, "ASCII", "UTF-8", sub=""))  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::removeWords, tm::stopwords('english')) # Removing stop-words
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::removePunctuation) # Removing stop-words
# corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "english") # Stemming the words  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces 

tdm <- tm::DocumentTermMatrix(corpus.cleaned) 


empty.rows <- tdm[rowTotals == 0, ]$dimnames[1][[1]] 
corpus_new <- corpus[-as.numeric(empty.rows)]


ap_lda <- LDA(tdm, k = 10)# control = list(seed = 1234))


# library(tm)
# library(topicmodels)
# library(Matrix)
# 
# # grab a character vector of text. Your source may be different
# text <- textmineR::nih_sample$ABSTRACT_TEXT
# 
# text_corpus <- SimpleCorpus(VectorSource(text))
# 
# text_dtm <- DocumentTermMatrix(text_corpus,
#                                control = list(tolower=TRUE,
#                                               removePunctuation = TRUE, 
#                                               removeNumbers= TRUE,
#                                               stopwords = TRUE,
#                                               sparse=TRUE))
# 
# text_dtm2 <- cast_sparse(text_dtm)
# 
# text_dtm2 <- Matrix::sparseMatrix(i=text_dtm$i, 
#                                   j=text_dtm$j,
#                                   x=text_dtm$v, 
#                                   dims=c(text_dtm$nrow, text_dtm$ncol), 
#                                   dimnames = text_dtm$dimnames)
# 
# doc_lengths <- Matrix::rowSums(text_dtm2)
# 
# text_dtm3 <- text_dtm2[doc_lengths > 0, ]
# 
# text_lda <- LDA(text_dtm3,  k = 2, method = "VEM", control = NULL)


# library(tidytext)
# 
# ap_topics <- tidy(ap_lda, matrix = "beta")
# ap_topics