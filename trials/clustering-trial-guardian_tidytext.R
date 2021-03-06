
library(quanteda)
library(topicmodels)
library(ggplot2)
library(reshape2)

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

data.topicmodels <- convert(data.trimmed, to = "topicmodels")
lda <- LDA(data.topicmodels, k = 10)


# min_termfreq, max_termfreq 	-> minimum/maximum values of feature frequencies across all
# documents, below/above which features will be removed

# termfreq_type -> how min_termfreq and max_termfreq are interpreted. "count" sums 
# the frequencies; "prop" divides the term frequencies by the total sum; "rank" is 
# matched against the inverted ranking of features in terms of overall frequency, 
# so that 1, 2, ... are the highest and second highest frequency features, and so on;
# "quantile" sets the cutoffs according to the quantiles (see quantile) of term frequencies.

# min_docfreq, max_docfreq 	-> minimum/maximum values of a feature's document frequency,
# below/above which features will be removed

# docfreq_type 	-> specify how min_docfreq and max_docfreq are interpreted. "count" is
# the same as docfreq(x, scheme = "count"); "prop" divides the document frequencies by the
# total sum; "rank" is matched against the inverted ranking of document frequency, so that
# 1, 2, ... are the features with the highest and second highest document frequencies, and 
# so on; "quantile" sets the cutoffs according to the quantiles (see quantile) of document 
# frequencies.
library(tm)


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


library(tm)
library(topicmodels)
library(Matrix)

# grab a character vector of text. Your source may be different
text <- textmineR::nih_sample$ABSTRACT_TEXT

text_corpus <- SimpleCorpus(VectorSource(data$text))

text_dtm <- DocumentTermMatrix(text_corpus,
                               control = list(tolower=TRUE,
                                              removePunctuation = TRUE,
                                              removeNumbers= TRUE,
                                              stopwords = TRUE,
                                              sparse=TRUE))

# text_dtm2 <- cast_sparse(text_dtm)

text_dtm<-tdm
text_dtm2 <- Matrix::sparseMatrix(i=text_dtm$i,
                                  j=text_dtm$j,
                                  x=text_dtm$v,
                                  dims=c(text_dtm$nrow, text_dtm$ncol),
                                  dimnames = text_dtm$dimnames)

doc_lengths <- Matrix::rowSums(text_dtm2)

text_dtm3 <- text_dtm2[doc_lengths > 0, ]

text_lda <- LDA(text_dtm3,  k = 10, method = "VEM", control = NULL)


library(tidytext)

ap_topics <- tidy(text_lda, matrix = "beta")
ap_topics
