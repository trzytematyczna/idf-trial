# Creating the empty dataset with the formatted columns 
dataframe <- data.frame(ID=character(), 
                        datetime=character(), 
                        content=character(), 
                        label=factor()) 
source.url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00438/Health-News-Tweets.zip' 
target.directory <- '/tmp/clustering-r' 
temporary.file <- tempfile() 
download.file(source.url, temporary.file) 
unzip(temporary.file, exdir = target.directory) 

# Reading the files 
target.directory <- paste(target.directory, 'Health-Tweets', sep = '/') 
files <- list.files(path = target.directory, pattern='.txt$') 

# Filling the dataframe by reading the text content 
for (f in files) { 
  news.filename = paste(target.directory , f, sep ='/') 
  news.label <- substr(f, 0, nchar(f) - 4) # Removing the 4 last characters => '.txt' 
  news.data <- read.csv(news.filename, 
                        encoding = 'UTF-8', 
                        header = FALSE, 
                        quote = "", 
                        sep = '|', 
                        col.names = c('ID', 'datetime', 'content')) 
  
  # Trick to handle native split problem (cf. notebook for detail) 
  news.data <- news.data[news.data$content != "", ] 
  news.data['label'] = news.label # We add the label of the tweet  
  
  # Massive data loading memory problem : only loading a few (cf. notebook for detail) 
  news.data <- head(news.data, floor(nrow(news.data) * 0.05)) 
  dataframe <- rbind(dataframe, news.data) # Row appending 
} 
unlink(target.directory, recursive =  TRUE) # Deleting the temporary directory 

sentences <- sub("http://([[:alnum:]|[:punct:]])+", '', dataframe$content) 

corpus = tm::Corpus(tm::VectorSource(sentences)) 

# Cleaning up 
# Handling UTF-8 encoding problem from the dataset 
corpus.cleaned <- tm::tm_map(corpus, function(x) iconv(x, "ASCII", "UTF-8", sub=""))  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::removeWords, tm::stopwords('english')) # Removing stop-words 
corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "english") # Stemming the words  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces 

tdm <- tm::DocumentTermMatrix(corpus.cleaned) 
tdm.tfidf <- tm::weightTfIdf(tdm)

tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 

# Cosine distance matrix (useful for specific clustering algorithms) 
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine") 

