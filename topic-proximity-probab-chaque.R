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
document_topic.part.fun<-function(model, thistopic){
  
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
    filter(topic==thistopic)
    #filter(row_number() ==1)
  document_topic$document<-document_topic$document %>%stri_replace_all_fixed("X","")
  
  # dt-res<-document_topic %>% filter(topic==thistopic)
  
  document_topic
}


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


document_topic.all.fun<-function(model){
  
  document_topic <- data.frame(model$theta)
  document_topic$document <-rownames(document_topic) 
  rownames(document_topic) <- 1:nrow(document_topic)
  document_topic <- document_topic %>% 
    reshape2::melt(id.vars = "document") %>% 
    rename(topic = variable) %>% 
    tidyr::separate(topic, into =c("t","topic")) %>% 
    select(-t) %>% 
    group_by(document) %>% 
    arrange(desc(value)) 
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

labels.topic.fun<-function(terms.summary, thistopic){
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
  
  topic.labels<-topic.labels %>% filter(topic==thistopic)
  
  topic.labels
}

compare.fun<-function(k_list){
  data_name<-"twitter-2M"
  res_dir <- paste0("./results/",data_name,"/")
  alpha<-0.1 # 0.alpha value
  ngram<- 1#ngrams
  exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
  
  base.t<-k_list[1]
  comp.t<-k_list[2]
  model_list <- TmParallelApply(X = k_list, FUN = read.model.fun, cpus=1)
  base.model<-model_list[[1]]
  comp.model<-model_list[[2]]
  
  print("< models")
  doc.nb=nrow(base.model$theta)
  
  base.full.dt<-document_topic.all.fun(base.model)
  comp.full.dt<-document_topic.all.fun(comp.model)

  comp.full.labels<-labels.fun(terms.summary.fun(comp.model))
  base.full.labels<-labels.fun(terms.summary.fun(base.model))

  all.tops<-data.frame()
    for(t in 1:base.t){
      for(s in 1:comp.t){
      print(paste0("base -> topic ", t, " & ",s))

      print("document topic")
      base.dt<-base.full.dt %>% filter(topic==t)
      
      comp.dt<-comp.full.dt %>% filter(topic==s)
      
      print("labs")
      base.labels<-base.full.labels %>% filter(topic==t)
      comp.labels<-comp.full.labels %>% filter(topic==s)

      print("inner join dt full")
      dt_full<- dplyr::inner_join(base.dt,comp.dt, by="document")
      colnames(dt_full)<-c("document","base.topic","base.value","comp.topic","comp.value")

      base.global.topic.probab<-dt_full %>%
        group_by(base.topic)%>%
        summarise(base.gtp=sum(base.value)/doc.nb)
      
      comparison.dt <- dt_full%>% 
        group_by(base.topic, comp.topic)%>% 
        summarise(sum.prob=sum(base.value*comp.value)) %>%
        mutate(ratio.sum.docs=sum.prob/doc.nb)
      
      
      print("me inner join")
      
      me<-dplyr::inner_join(comparison.dt, base.global.topic.probab, by = "base.topic")  
      
      me <- me %>% mutate(proxi=ratio.sum.docs/base.gtp)
      
      print("all tops bind")
      
      all.tops<-rbind(all.tops, as.data.frame(me))
      
      }
    }



  all.tops <- all.tops %>%
    #ungroup()%>%
    mutate(base.topic=factor(base.topic,levels = 1:k_list[1], labels=base.labels$z))%>%
    mutate(comp.topic=factor(comp.topic,levels = 1:k_list[2], labels=comp.labels$z))
      
  g<-ggplot(all.tops, aes(x=comp.topic,y=proxi))+
    geom_bar(stat="identity",position="stack")+
    ylim(0.0, (max(me$proxi)+0.05))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4))+
    ggtitle(paste0(data_name,"-",exp_name))+
    xlab(paste0("topics from k=",k_list[2]))+
    ylab(paste0("proximity of probabilities to k="))+
    facet_wrap(.~base.topic, ncol=2)#, labeller = topic.labels$z)
  
  ggsave(paste0(res_dir,data_name,"-probabv2-comparison-k-",k_list[1],"x-k-",k_list[2],".pdf"), device = "pdf")
  
}

k_list <- c(6,5)
compare.fun(k_list)



