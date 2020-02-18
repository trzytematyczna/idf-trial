library(tm)
library(quateda)

csv_data <- read.csv2(paste(getwd(),"/data/guardian-articles.csv",sep=''))
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
  corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "english") # Stemming the words  
  corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces 
  
  tdm <- tm::DocumentTermMatrix(corpus.cleaned) 
  tdm.tfidf <- tm::weightTfIdf(tdm)
  
  tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
  tfidf.matrix <- as.matrix(tdm.tfidf) 
  
  # Cosine distance matrix (useful for specific clustering algorithms) 
  dist.matrix = proxy::dist(tfidf.matrix, method = "cosine") 
  
  clustering.kmeans <- kmeans(tfidf.matrix, truth.K) 
  clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
  clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10) 
  
  
  # master.cluster <- clustering.kmeans$cluster 
  # slave.hierarchical <- cutree(clustering.hierarchical, k = truth.K) 
  # slave.dbscan <- clustering.dbscan$cluster 
  # stacked.clustering <- rep(NA, length(master.cluster))  
  # names(stacked.clustering) <- 1:length(master.cluster) 
  # 
  # for (cluster in unique(master.cluster)) { 
  #   indexes = which(master.cluster == cluster, arr.ind = TRUE) 
  #   slave1.votes <- table(slave.hierarchical[indexes]) 
  #   slave1.maxcount <- names(slave1.votes)[which.max(slave1.votes)]   
  #   slave1.indexes = which(slave.hierarchical == slave1.maxcount, arr.ind = TRUE) 
  #   slave2.votes <- table(slave.dbscan[indexes]) 
  #   slave2.maxcount <- names(slave2.votes)[which.max(slave2.votes)]   
  #   stacked.clustering[indexes] <- slave2.maxcount 
  # } 
  
  points <- cmdscale(dist.matrix, k = 2) 
  palette <- colorspace::diverge_hcl(truth.K) # Creating a color palette 
  previous.par <- par(mfrow=c(1,3), mar = rep(1.5, 4)) 
  
  plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
       mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
  
  plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
       mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
  
  plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
       mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
  
  # plot(points, main = 'Stacked clustering', col = as.factor(stacked.clustering), 
  #      mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
  #      xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
  
  par(previous.par) # recovering the original plot space parameters 
######105614210 (tm) vs 13239675 (quanteda)
  
  library(quanteda)
  
  
  csv_data <- read.csv2(paste(getwd(),"/data/guardian-articles.csv",sep=''))
  data<-csv_data
  data$text<-as.character(data$text)
  
  names(data)[names(data) == "X_id..oid"] <- "docid"
  names(data)[names(data) == "text"] <- "text"
  
  dfm_data <- corpus(data)
  dfmat_data <- dfm(dfm_data, remove_punct = TRUE, remove = stopwords('en')) %>% 
    # dfm_remove(c('*-time', '*-timeUpdated', 'GMT', 'BST')) %>% 
    dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile", 
             max_docfreq = 0.1, docfreq_type = "prop")
  
  dfmat_data <- dfmat_data[ntoken(dfmat_data) > 0,]
  dfmat_data_tfidf <- dfm_tfidf(dfmat_data)
  
  tstat1 <- textstat_simil(dfmat_data_tfidf, method = "cosine", margin = "features", min_simil = 0.3)

  
  # wordDistMat <- dist(wordDfm)
  # wordCluster <- hclust(wordDistMat)
  # plot(wordCluster, labels = docnames(wordDfm),
  #      xlab="", main="tf-idf Frequency weighting")
  # df <- convert(dfmat_data_tfidf, to = "data.frame")
  