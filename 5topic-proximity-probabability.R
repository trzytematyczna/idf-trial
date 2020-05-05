library(topicmodels)
library(ggplot2)
library(textmineR)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(stringi)
library(DescTools)

# data_name<-"guardian-comments"
# data_dir<-"./data/guardian/full_comments_guardian.csv"

# data_name <- "guardian-articles"
# data_dir <- "./data/guardian/full_articles_guardian.csv"
document_topic.topic.fun<-function(model, thistopic){
  
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
document_topic.top1.fun<-function(model){
  
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

### compares two clusters (base vs comparison) simoultaneously (vectorized version)
## #uses top1 topic for each document in the corpus
### calculates percentage (number) of documents being in each of topic in comparion cluster against
### the base topics (number) -- seems (!) same to probability version with using top1 topic per document
compare.top1.percentage.two.clusters.fun<-function(k_list){
  model_list <- TmParallelApply(X = k_list, FUN = read.model.fun, cpus=1)
  base.model<-model_list[[1]]
  comp.model<-model_list[[2]]
  
  base.dt<-document_topic.top1.fun(base.model)
  comp.dt<-document_topic.top1.fun(comp.model)
  
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
  
  ggsave(paste0(res_dir,data_name,"comparison-top1-k-",k_list[1],"x-k-",k_list[2],".pdf"), device = "pdf")
  
}

### compares two clusters simoultaneously (base vs comp), uses all topic probabilities per document
### vectorised version -- not suitable for lots of data as it gets killed in inner_join part
compare.all.topics.two.clusters.fun<-function(k_list){
  
  if(length(k_list)!=2){
    print("Must be two cluster numbers to compare!")
  } else{ 
    model_list <- TmParallelApply(X = k_list, FUN = read.model.fun, cpus=1)
    base.model<-model_list[[1]]
    comp.model<-model_list[[2]]
    
    base.dt<-document_topic.all.fun(base.model)
    comp.dt<-document_topic.all.fun(comp.model)
  
    doc.nb = length(unique(base.dt$document))
  
    base.labels<-labels.fun(terms.summary.fun(base.model))
    comp.labels<-labels.fun(terms.summary.fun(comp.model))
    
    dt_full<- dplyr::inner_join(base.dt,comp.dt, by="document")
    colnames(dt_full)<-c("document","base.topic","base.value","comp.topic","comp.value")
    
    base.global.topic.probab<-base.dt %>%
      group_by(topic)%>%
      summarise(base.gtp=mean(value))
    colnames(base.global.topic.probab)<-c("base.topic","base.gtp")
    
    comparison.dt <- dt_full%>% 
      group_by(base.topic, comp.topic)%>% 
      summarise(sum.prob=sum(base.value*comp.value)) %>%
      mutate(ratio.sum.docs=sum.prob/doc.nb)
  
    me<-dplyr::inner_join(comparison.dt, base.global.topic.probab, by = "base.topic")  
    
    me <- me %>%
      mutate(proxi=ratio.sum.docs/base.gtp)%>%
      ungroup()%>%
      mutate(base.topic=factor(base.topic,levels = 1:k_list[1], labels=base.labels$z))%>%
      mutate(comp.topic=factor(comp.topic,levels = 1:k_list[2], labels=comp.labels$z))
    
    data_name<-"twitter-2M"
    res_dir <- paste0("./results/",data_name,"/")
    alpha<-0.1 # 0.alpha value
    ngram<- 1#ngrams
    exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
    
    g<-ggplot(me, aes(x=comp.topic,y=proxi))+
      geom_bar(stat="identity",position="stack")+
      ylim(0.0, (max(me$proxi)+0.05))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.4))+
      ggtitle(paste0(data_name,"-",exp_name))+
      xlab(paste0("topics from k=",k_list[2]))+
      ylab(paste0("asd"))+
      facet_wrap(.~base.topic, ncol=2)#, labeller = topic.labels$z)
    
    ggsave(paste0(res_dir,data_name,"-probab-comparison-k-",k_list[1],"x-k-",k_list[2],".pdf"), device = "pdf")
  }
  
}

### compares two clusters one by one (base vs comp), uses all topic probabilities per document
### for version -- takes each of topic from base (outside for) and compares probabilities to
### each of the comp topic probabilities; much slower but doesn't have memory overhead 
compare.each.fun<-function(k_list){
  
  if(length(k_list)!=2){
    print("Must be two cluster numbers to compare!")
  } else{ 
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
  
    base.global.topic.probab<-base.full.dt %>%
      group_by(topic)%>%
      summarise(base.gtp=mean(value))
    colnames(base.global.topic.probab)<-c("base.topic","base.gtp")

    all.tops<-data.frame()
      for(t in 1:base.t){
        for(s in 1:comp.t){
        print(paste0("base -> topic ", t, " & ",s))
  
        print("document topic")
        base.dt<-base.full.dt %>% filter(topic==t)
        
        comp.dt<-comp.full.dt %>% filter(topic==s)
        
        print("labs")
        # base.labels<-base.full.labels %>% filter(topic==t)
        # comp.labels<-comp.full.labels %>% filter(topic==s)
  
        print("inner join dt full")
        dt_full<- dplyr::inner_join(base.dt,comp.dt, by="document")
        colnames(dt_full)<-c("document","base.topic","base.value","comp.topic","comp.value")
  
          
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
  
  #asd<-all.tops
  
    all.tops <- all.tops %>%
      #ungroup()%>%
      mutate(base.topic=factor(base.topic,levels = 1:k_list[1], labels=base.full.labels$z))%>%
      mutate(comp.topic=factor(comp.topic,levels = 1:k_list[2], labels=comp.full.labels$z))
        
    g<-ggplot(all.tops, aes(x=comp.topic,y=proxi))+
      geom_bar(stat="identity",position="stack")+
      ylim(0.0, (max(all.tops$proxi)+0.05))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.4))+
      ggtitle(paste0(data_name,"-",exp_name))+
      xlab(paste0("topics from k=",k_list[2]))+
      ylab(paste0("proximity of probabilities to k=",k_list[1]))+
      facet_wrap(.~base.topic, ncol=2)#, labeller = topic.labels$z)
    
    # g
    ggsave(paste0(res_dir,data_name,"-probabv2-comparison-k-",k_list[1],"x-k-",k_list[2],".pdf"), device = "pdf")
  }  
}


global.prob<-function(kluster, save.plots = FALSE){
  
  data_name<-"twitter-2M"
  res_dir <- paste0("./results/",data_name,"/")
  alpha<-0.1 # 0.alpha value
  ngram<- 1#ngrams
  exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
  
  base.t<-kluster
  model_list <- TmParallelApply(X = kluster, FUN = read.model.fun, cpus=1)
  base.model<-model_list[[1]]
  
  base.full.dt<-document_topic.all.fun(base.model)
  
  doc.nb=nrow(base.model$theta)
  
  global.probabilities<-base.full.dt %>%
    group_by(topic)%>%
    summarise(gtp=mean(value))
      # summarise(gtp=sum(value)/doc.nb)
  
  if(save.plots==TRUE){
    g<-ggplot(global.probabilities, aes(x=reorder(topic, gtp),y=gtp))+
      geom_bar(stat="identity",position="stack")+
      ylim(0.0, (max(global.probabilities$gtp)))+
      theme(axis.text.x = element_text(angle = 90))+
      ggtitle(paste0("Global probability per topic with clusters k=",kluster))+
      xlab("Topics")+
      ylab("Probability")
    ggsave(paste0(res_dir,"k-",kluster,"-",exp_name,"-global-avg-probabilities.pdf"))
  }
  global.probabilities
}

entropy.run<-function(save.global.probab.plots=FALSE){
  df<-data.frame()
  for(i in 5:10){
    gp <- global.prob(i, save.plots = save.global.probab.plots)
    en <- Entropy(gp$gtp)
    
    df<-rbind(df, c(i,en))
  }
  colnames(df)<-c("clusters","entropy")
  
  df
}  



generality.fun<-function(kluster){
  
  data_name<-"twitter-2M"
  res_dir <- paste0("./results/",data_name,"/")
  alpha<-0.1 # 0.alpha value
  ngram<- 1#ngrams
  exp_name<-paste0(data_name,"-alpha-",alpha,"-ngram-",ngram)
  
  base.t<-kluster
  model_list <- TmParallelApply(X = kluster, FUN = read.model.fun, cpus=1)
  base.model<-model_list[[1]]
  
  doc.nb=nrow(base.model$theta)
  # normalize=1/log2(base.model$k)
  
  base.full.dt<-document_topic.all.fun(base.model)
  
  global.probabilities<-base.full.dt %>%
    group_by(topic)%>%
    summarise(gtp=mean(value))
  
  base.inf <-dplyr::inner_join(base.full.dt, global.probabilities, by = "topic")  %>%
    arrange(document)
  
  # base.inf <- base.inf %>% mutate(brackets=value*(log2(value)-log2(doc.nb*gtp))*normalize)
  base.inf <- base.inf %>% mutate(brackets=(value/doc.nb)*1/gtp*(log2(value/(doc.nb*gtp))))
  
  gen<-base.inf%>%
    group_by(topic)%>%
    summarize(gen=-sum(brackets))#*(1/normalize))
  
  generality <-dplyr::inner_join(gen, global.probabilities, by = "topic") %>% 
    mutate(generality=gen*(1/log2(doc.nb)))#*1/normalize)
    
  # generality%>%filter(topic,gen.value) %>% write.csv(paste0(res_dir,"k-",kluster,"-topic-generality.csv"))

  generality<-generality %>%select(-gen,-gtp)
  
  generality
}


generality.run<-function(){
  df<-data.frame()
  for(i in 5:10){
    gen <- generality.fun(kluster = i) %>% mutate(cluster.nb=i)
    
    df<-rbind(df,gen)
  }
  colnames(df)<-c("topic","generality", "cluster.nb")
  
  df
}  

data_name<-"twitter-2M"
res_dir <- paste0("./results/",data_name,"/")
# generality_vals<-generality.run()
# write.csv(generality_vals, paste0(res_dir,"all-clusters-topic-generalities.csv"), quote=FALSE, row.names = FALSE)
# 
# entropy_values<-entropy.run(FALSE)
# write.csv(entropy_values, paste0(res_dir,"entropy-per-kclusters.csv"), row.names = FALSE, quote = FALSE)
k_list <- c(10,9)
compare.each.fun(k_list)
# compare.all.topics.two.clusters.fun(k_list)

