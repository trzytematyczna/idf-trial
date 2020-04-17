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

# data_name<-"twitter-4M"
# data_file<-"./data/twitter/split-3M/twitter-3M-sampled.csv"

# data_name<-"guardian-comments"
# data_file<-"./data/guardian/full_comments_guardian.csv"

# data_name <- "guardian-articles"
# data_file <- "./data/guardian/full_articles_guardian.csv"
# res_dir <- "./test"

# do.exp(exp.data)

exp.data <- read.csv2("./test/test3.csv", stringsAsFactors = FALSE, sep=",", header = FALSE, quote = "\"", strip.white=TRUE)
# exp.data <- as.data.frame(matrix(ncol = 3))
colnames(exp.data)<-c("dataname","datafile","resdir")

do.exp <- function(expdata){
  for(i in 1:nrow(expdata)){
    print(expdata$dataname[i])
    go.lda(expdata$dataname[i],expdata$datafile[i], expdata$resdir[1])
  }
}




go.lda<-function(data_name, data_file, res_dir){
  
  rds_dir <- res_dir
  model_dir <- res_dir
  
  
  k_list <- c(5,10)
  alpha<-0.1 # 0.alpha value
  ngram<- 1#ngrams
  
  exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
  
  coherence_name<- paste0("coherence-",exp_name,".pdf")
  
  dtm_file<-paste0(rds_dir,"/dtm-",data_name,"-ngram-",ngram,".Rds")
  original_tf_file <- paste0(rds_dir,"/originaltf-",data_name,"-ngram-",ngram,".Rds")
  
  model_name <- paste0("_topics-",exp_name, ".rda")
  
  
  custom.stopwords <- c("rt","amp","mt","climate","change","climatechange","jan","feb","mar","apr","may",
                        "june","july","aug","sept","oct","nov","dec", "say","said")
  
  
  
  if(data_name %like% "twitter"){
    data <- read.csv(data_file, stringsAsFactors = FALSE, sep=",", quote = "\"",  row.names=NULL, header = TRUE)#,colClasses = c("factor","character"), encoding = "UTF-8")
    data$id<- 1:nrow(data)
    data$from_user_name<-NULL
    data$from_user_followercount<-NULL
  }else{
    data<- read.csv2(data_file, stringsAsFactors = FALSE)
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
  
  
  run.fun <- function(k){  ##alpha
    filename = file.path(model_dir, paste0(k, model_name)) ##alpha
    if (!file.exists(filename)) {
      m <- FitLdaModel(dtm = dtm, k = k, iterations = 1, alpha = alpha) ##alpha #qwe
      m$k <- k
      m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
      save(m, file = filename)
    } else {
      print("model file found!")
    }
    m
  }
  
  model_list <- TmParallelApply(X = k_list, FUN = run.fun, cpus=4) ##alpha
  #
  coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)),
                              coherence = sapply(model_list, function(x) mean(x$coherence)),
                              stringsAsFactors = FALSE)
  g<-ggplot(coherence_mat, aes(x = k, y = coherence)) +
    geom_point() +
    geom_line(group = 1)+
    ggtitle("Best Topic by Coherence Score") + theme_minimal() +
    scale_x_continuous(breaks = seq(1,max(k_list),1)) + ylab("Coherence")
  
  ggsave(file.path(model_dir,coherence_name),plot = g, device = "pdf") ##alpha
  
  rm(list=ls())

}

do.exp(exp.data)