
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





vocabulary<- data.trimmed$
k_list <- seq(1, 20, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)model_list <- TmParallelApply(X = k_list, FUN = function(k){
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
}, export=c("dtm", "model_dir")) # export only needed for Windows machines#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")