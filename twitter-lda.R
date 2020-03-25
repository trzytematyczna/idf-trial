library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(wordcloud)
library(tidyr)
library(data.table)
library(tidytext)
library(stringr)


####selected parameters to check the results####
# k_list <- seq(5, 5, by = 1)
k_list <- c(5,10,15)
alpha<-1 # 0.alpha value
wid<-2 #for number of leading zeros in models
ngram<- 1#ngrams
al<- alpha%>% formatC(width=wid, flag = "0")
coherence_dir <- paste0("./results/twitter/3M") ##directory of models
model_dir <- paste0("./results/twitter/3M")

##################


data <- read.csv2("xaa.csv", stringsAsFactors = FALSE, sep=",", quote = "\"",
                  colClasses = c("factor","character"), encoding = "UTF-8")
data$id<- 1:nrow(data)

custom.stopwords <- c("rt","amp","mt","climate","change","climatechange","jan","feb","mar","apr","may",
                      "june","july","aug","sept","oct","nov","dec")
# data$text <- data$text %>% str_replace_all("[^[:graph:]]", " ") 
dtm <- CreateDtm(data$text, 
                 doc_names = data$id, 
                 # remove_punctuation = TRUE,
                 # remove_numbers = TRUE,
                 # lower = TRUE,
                 stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart"), custom.stopwords),
                 # stopword_vec = stopwords::stopwords(source = "smart"),
                 ngram_window = c(1, ngram))
original_tf <- TermDocFreq(dtm = dtm)

saveRDS(dtm, file = "dtm_3M_ngram1.Rds")
saveRDS(original_tf, file = "original_tf_3M.Rds")

# ##alpha=0.1
# 
run.model.fun1 <- function(k){  ##alpha
  filename = file.path(model_dir, paste0(k, "_topics_a01.rda")) ##alpha
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = 0.1) ##alpha
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  m
}

model_list <- TmParallelApply(X = k_list, FUN = run.model.fun1, cpus=2) ##alpha 
# 
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)),
                            coherence = sapply(model_list, function(x) mean(x$coherence)),
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,max(k_list),1)) + ylab("Coherence")

ggsave(file.path(coherence_dir, paste0("coherence_al01_ngram",ngram,".pdf")),plot = g) ##alpha
