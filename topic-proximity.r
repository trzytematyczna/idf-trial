library(topicmodels)
library(ggplot2)
library(textmineR)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(stringi)

# data_name<-"guardian-comments"
# data_dir<-"./data/guardian/full_comments_guardian.csv"

# data_name <- "guardian-articles"
# data_dir <- "./data/guardian/full_articles_guardian.csv"


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

terms.summary.fun<-function(model){
  
  terms.summary <-data.frame(t(model$phi))
  terms.summary$word <- rownames(terms.summary) 
  rownames(terms.summary) <- 1:nrow(terms.summary)
  terms.summary <- terms.summary %>% 
    reshape2::melt(idvars = "word") %>%
    plyr::rename(c("variable" ="topic"))%>%  
    group_by(topic) %>% 
    arrange(desc(value))
  
  terms.summary
}

document_topic.fun<-function(model){
  
  document_topic <- data.frame(model$theta)
  document_topic$document <-rownames(document_topic) 
  rownames(document_topic) <- 1:nrow(document_topic)
  document_topic <- document_topic %>% 
    reshape2::melt(id.vars = "document") %>% 
    rename(topic = variable) %>% 
    tidyr::separate(topic, into =c("t","topic")) %>% 
    select(-t) %>% 
    group_by(document) %>% 
    arrange(desc(value)) %>%
    filter(row_number() ==1)
  document_topic$document<-document_topic$document %>%stri_replace_all_fixed("X","")
  
  document_topic
}
labels.fun<-function(terms.summary){
  topic.labels <- terms.summary %>% 
    group_by(topic) %>% 
    top_n(2)
  
  topic.labels <- topic.labels %>% 
    group_by(topic, word) %>% 
    filter(row_number() == 1) %>%
    ungroup() %>% 
    tidyr::separate(topic, into =c("t","topic")) %>% 
    select(-t)
  
  topic.labels<-topic.labels%>% 
    group_by(topic) %>%
    summarise(s=paste(word, collapse = ", "))
  
  topic.labels<- topic.labels%>%
    group_by(topic) %>%
    summarize(z=paste(s,topic, collapse = ","))
  
  topic.labels
}


compare.fun<-function(k_list){
  model_list <- TmParallelApply(X = k_list, FUN = read.model.fun, cpus=1)
  base.model<-model_list[[1]]
  comp.model<-model_list[[2]]
  
  base.dt<-document_topic.fun(base.model)
  comp.dt<-document_topic.fun(comp.model)
  
  base.labels<-labels.fun(terms.summary.fun(base.model))
  comp.labels<-labels.fun(terms.summary.fun(comp.model))
  
  dt_full<- merge(base.dt,comp.dt, by="document")
  colnames(dt_full)<-c("document","base.topic","base.value","comp.topic","comp.value")
  
  comparison.dt <- dt_full%>% 
    group_by(base.topic,comp.topic)%>% 
    summarise(n=n())%>%
    mutate(perc=n/sum(n))%>%
    ungroup()%>%
    mutate(base.topic=factor(base.topic,levels = 1:k_list[1], labels=base.labels$z))%>%
    mutate(comp.topic=factor(comp.topic,levels = 1:k_list[2], labels=comp.labels$z))
  
  data_name<-"twitter-2M"
  res_dir <- paste0("./results/",data_name,"/")
  alpha<-0.1 # 0.alpha value
  ngram<- 1#ngrams
  exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)

  g<-ggplot(comparison.dt, aes(x=comp.topic,y=perc))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(comparison.dt$perc)+0.05))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4))+
    ggtitle(paste0(data_name,"-",exp_name))+
    xlab(paste0("topics from k=",k_list[2]))+
    ylab(paste0("percentage of topics from k=",k_list[1]," included in topic from k=",k_list[2]))+
    facet_wrap(.~base.topic, ncol=2)#, labeller = topic.labels$z)
  
  ggsave(paste0(res_dir,data_name,"comparison-k-",k_list[1],"x-k-",k_list[2],".pdf"), device = "pdf")

}

k_list <- c(7,10)
compare.fun(k_list)
