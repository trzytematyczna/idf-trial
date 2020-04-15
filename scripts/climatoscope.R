library(topicmodels)
library(ggplot2)
library(textmineR)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)

####selected parameters to check the results####

# data_name<-"twitter-3M"
# data_dir<-"./data/twitter/split-3M/xaa.csv"

data_name<-"guardian-articles"
data_dir<-"./data/guardian/full_articles_guardian.csv"

# data_name <- "guardian-comments"
# data_dir <- "./data/guardian/full_comments_guardian.csv"

res_dir <- "./plots/"
res_file <-paste0(res_dir,data_name,"-x-climatoscope-titles.csv")

climatoscope_file<-"./data/climatoscope-keywords.csv"


ngram<- 2#ngrams
# rds_dir <- paste0("./results/",data_name,"/")
# rds_name<-paste0(data_name,"-ngram-",ngram,".Rds")
# dtm_file<-paste0(rds_dir,"dtm-",rds_name)
# original_tf_file <- paste0(rds_dir,"originaltf-",rds_name)

custom.stopwords <- c("rt","amp","mt","climate","change","climatechange","jan","feb","mar","apr","may",
                      "june","july","aug","sept","oct","nov","dec", "say","said")


##################
if(data_name %like% "twitter"){
  data <- read.csv2(data_dir, stringsAsFactors = FALSE, sep=",", quote = "\"", colClasses = c("factor","character"))#, encoding = "UTF-8")
  data$id<- 1:nrow(data)
}else{
  data<- read.csv2(data_dir, stringsAsFactors = FALSE)
}
climatoscope<- read.csv2(climatoscope_file, stringsAsFactors = FALSE, sep=",")


dtm <- CreateDtm(data$title, 
                 doc_names = data$id, 
                 # remove_punctuation = TRUE,
                 # remove_numbers = TRUE,
                 # lower = TRUE,
                 stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart"), custom.stopwords),
                 ngram_window = c(1, ngram))
original_tf <- TermDocFreq(dtm = dtm)
rownames(original_tf)<-1:nrow(original_tf)
saveRDS(dtm, paste0("dtm-",data_name,"-ngram-",ngram,".rds"))
saveRDS(original_tf, paste0("originaltf-",data_name,"-ngram-",ngram,".rds"))

fq <- original_tf %>% 
  select(term, term_freq) %>% 
  # str_replace(keyword,pattern="_", replacement=" ") %>%
  rename(keyword = term) 

fq$keyword<-str_replace(fq$keyword, pattern = "_", replacement = " ")

cross <- merge(x = climatoscope, y = fq,  by="keyword") %>% 
  select(keyword, term_freq) %>%
  arrange(desc(term_freq))

write.csv2(cross, file = res_file, quote =FALSE, row.names = FALSE)

