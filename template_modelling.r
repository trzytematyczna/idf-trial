library(quanteda)
library(topicmodels)
library(ggplot2)
library(textmineR)
library(reshape2)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)


####selected parameters to check the results####
k_list<-10
# k_list <- c(5,10,15)
alpha<-0.1 # 0.alpha value
ngram<- 1#ngrams

data_name<-"twitter-4M"
data_dir<-"./data/twitter/split-4M/twitter-4M-sampled.csv"

# data_name<-"guardian-comments"
# data_dir<-"./data/guardian/full_comments_guardian.csv"

# data_name <- "guardian-articles"
# data_dir <- "./data/guardian/full_articles_guardian.csv"

rds_dir <- paste0("./results/",data_name,"/")
model_dir <- paste0("./results/",data_name,"/k-",k_list)

exp_name<-paste0("k-",k_list,"-alpha-",alpha,"-ngram-",ngram)
coherence_name<- paste0("coherence-",exp_name,".pdf")
rds_name<-paste0(data_name,"-ngram-",ngram,".Rds")
dtm_file<-paste0(rds_dir,"dtm-",rds_name)
original_tf_file <- paste0(rds_dir,"originaltf-",rds_name)

  
custom.stopwords <- c("rt","amp","mt","climate","change","climatechange","jan","feb","mar","apr","may",
                        "june","july","aug","sept","oct","nov","dec", "say","said")
##################
  
  
if(data_name %like% "twitter"){
  data <- read.csv2(data_dir, stringsAsFactors = FALSE, sep=",", quote = "\"", colClasses = c("factor","character"))#, encoding = "UTF-8")
  data$id<- 1:nrow(data)
}else{
  data<- read.csv2(data_dir, stringsAsFactors = FALSE)
}

dtm <- CreateDtm(data$text, 
                   doc_names = data$id, 
                   # remove_punctuation = TRUE,
                   # remove_numbers = TRUE,
                   # lower = TRUE,
                   stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart"), custom.stopwords),
                   ngram_window = c(1, ngram))
original_tf <- TermDocFreq(dtm = dtm)
  
saveRDS(dtm, file = dtm_file)
saveRDS(original_tf, file = original_tf_file)

# ##alpha=0.1
run.model.fun1 <- function(k){  ##alpha
  filename = file.path(model_dir, paste0(k, "_topics_a",alpha,".rda")) ##alpha
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, alpha = alpha) ##alpha
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    print("model file found!")
  }
  m
}

model_list <- TmParallelApply(X = k_list, FUN = run.model.fun1, cpus=4) ##alpha
#
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)),
                            coherence = sapply(model_list, function(x) mean(x$coherence)),
                            stringsAsFactors = FALSE)
g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,max(k_list),1)) + ylab("Coherence")

ggsave(file.path(model_dir,coherence_name),plot = g) ##alpha

