library(textmineR)
library(dplyr)
library(data.table)


read.model.fun <- function(k){
  alpha<-0.1 # 0.alpha value
  ngram<- 1#ngrams
  data_name<-"twitter-2M"
  exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
  model_name <- paste0("_topics-",exp_name, ".rda")
  model_dir <- paste0("./results/",data_name,"/")
  filename = file.path(model_dir, paste0(k, paste0("_topics-",exp_name, ".rda")))
  if (!file.exists(filename)) {
    print("Nofile!")
  } else {
    load(filename)
  }
  m
}

k_list<-9
data_name<-"twitter-2M"
res_dir <- paste0("./results/",data_name,"/")
# exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
rds_dir <- paste0("./results/",data_name,"/")
# dtm_file<-paste0(rds_dir,"/dtm-",data_name,"-ngram-",ngram,".Rds")
data_dir<-"./data/twitter/test-data-2M/file_1-testsample.csv"

if(data_name %like% "twitter"){
  data <- read.csv(data_dir, stringsAsFactors = FALSE, sep=",", quote = "\"",  header = TRUE)#colClasses = c("factor","character"))#, encoding = "UTF-8")
  # data$id<- 1:nrow(data)
  data$date<-NULL
  data$from_user_id<-NULL
  data$from_user_name<-NULL
  data$from_user_followercount<-NULL
}else{
  data<- read.csv2(data_dir, stringsAsFactors = FALSE)
}

model_list <- TmParallelApply(X = k_list, FUN = read.model.fun, cpus=1)
model<-model_list[[1]]

custom.stopwords <- c("rt","amp","mt","climate","change","climatechange","jan","feb","mar","apr","may",
                      "june","july","aug","sept","oct","nov","dec", "say","said")

ngram<-1
newdtm <- CreateDtm(data$text, 
                 doc_names = data$id, 
                 stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart"), custom.stopwords),
                 ngram_window = c(1, ngram))
original_tf <- TermDocFreq(dtm = newdtm)

saveRDS(newdtm, file = "dtm_file_1.rda")
saveRDS(original_tf, file = "originaltf_file_1.rda")


assignments <- predict(model, newdtm,
                       method = "gibbs", 
                       iterations = 10, #200
                       burnin = 5, #180
                       cpus = 1)
assignments %>% write.csv("assignment_file_1.csv", row.names = TRUE, quote = FALSE, col.names = FALSE)
