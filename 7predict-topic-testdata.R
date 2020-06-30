#!/usr/bin/env Rscript
library(textmineR)
library(dplyr)
library(data.table)
library(readr)

# options <- commandArgs(trailingOnly = TRUE)
# filename<-paste0("file_",options[1],"-testsample")
filename<-"test"
print(filename)
#filename<-paste0("file_5-testsample")
k_list<-9

ngram<-1
data_name<-"twitter-trained"
# res_dir <- paste0("./results/",data_name,"/")####
res_dir <- paste0("./results/twitter-trained/")
# exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
rds_dir <- paste0("./results/twitter-2M/",data_name)
dtm_file<-paste0(rds_dir,"/dtm-",data_name,"-",filename,".Rds")#"dtm_file_1.rda"
data_dir<-paste0("./data/twitter/test-data-500K/",filename,".csv")
assignment_file<- paste0(res_dir,"assignment-",filename,".csv")

exp_name <- "twitter-2M-alpha-0.1-ngram-1"
model.file <- paste0("./results/twitter-2M/",k_list,"_topics-",exp_name,".rda")


custom.stopwords <- c("rt","amp","mt","climate","change","climatechange","jan","feb","mar","apr","may",
                      "june","july","aug","sept","oct","nov","dec", "say","said")

if(data_name %like% "twitter"){
  # data <- read.csv(data_dir, stringsAsFactors = FALSE, sep=",", quote = "\"",  header = TRUE)#colClasses = c("factor","character"))#, encoding = "UTF-8")
  data<-read_csv(data_dir, col_types = cols (id = col_character()))
  data$date<-NULL
  data$from_user_id<-NULL
  data$from_user_name<-NULL
  data$from_user_followercount<-NULL
}else{
  # data<- read.csv2(data_dir, stringsAsFactors = FALSE)
  data<-read_csv2(data_dir, col_types = cols (id = col_character()))
}

read.model.fun <- function(k){
  filename = model.file
  if (!file.exists(filename)) {
    print("Nofile!")
  } else {
    load(filename)
  }
  m
}

model_list <- TmParallelApply(X = k_list, FUN = read.model.fun, cpus=1)
model<-model_list[[1]]

newdtm <- CreateDtm(data$text, 
                 doc_names = data$id, 
                 stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart"), custom.stopwords),
                 ngram_window = c(1, ngram))
# original_tf <- TermDocFreq(dtm = newdtm)

# saveRDS(newdtm, file = dtm_file)####
# saveRDS(original_tf, file = "originaltf_file_1.rda")

print("predict go...")
assignments <- predict(model, newdtm,
                       method = "gibbs", 
                       iterations = 2,#500, #200
                       burnin = 1,#75, #180
                       cpus = 4)
# assignments %>% write.csv(assignment_file, row.names = TRUE, quote = FALSE)
# print("write go1...")
as.matrix(assignments) %>% write.csv("test-write.csv", row.names = TRUE, quote = FALSE)
# print("write go2...")
# as.matrix(assignments) %>% write_csv("test-write_csv.csv")
