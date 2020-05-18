library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(ggplot2)

data_name<-"twitter-500K"
data_dir<-"./results/twitter-trained/assign/"

data.files <- list.files(data_dir)
res_dir<-"./results/twitter-trained/"
# res<-data.frame()
file.means<-data.frame()
file.sums<-data.frame()
file.mults<-data.frame()
doc.nb<-0
buckets<-data.frame()
for (i in data.files){
  print(i)
  df <- read.csv(paste0(data_dir,i), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8", na.strings = NA)
    colnames(df)<-c("id","t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")

  means <- df %>% select(-id) %>%colMeans()
  sums<-df%>%select(-id) %>% colSums()
  
  logdf<-df%>%select(-id)%>%log2()
  s<-df%>%select(-id)
  mults<-logdf*s
  mults<-mults%>%colSums()
  
  file.means<-bind_rows(file.means,means)  
  file.sums<-bind_rows(file.sums,sums)
  file.mults<-bind_rows(file.mults,mults)
  
  doc.nb<-doc.nb+nrow(df)
  
  
  
  cols<-c("t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
  for(i in  seq(1,0.5,by=-0.1)){
    lo<-i-0.1
    up<-i
    bp<-df %>%  filter_at(cols, any_vars(between(.,lo,up)))
    if(lo==0.4) lo<-0
    buckets<-rbind(buckets, c(nrow(bp),lo,up))
  }
  
}

global.means <- file.means %>% colMeans()

global.mults <- file.means %>% colSums()
global.sums <- file.sums %>% colSums()

global.means<-global.means%>%reshape2::melt()
global.means$topic<-rownames(global.means)
rownames(global.means) <- 1:nrow(global.means)
global.means <- global.means %>%
  tidyr::separate(topic, into =c("t","topic")) %>% 
  select(-t) 

global.mults<-global.mults%>%reshape2::melt()
global.mults$topic<-rownames(global.mults)
rownames(global.mults) <- 1:nrow(global.mults)
global.mults <- global.mults %>%
  tidyr::separate(topic, into =c("t","topic")) %>% 
  select(-t) 

global.sums<-global.sums%>%reshape2::melt()
global.sums$topic<-rownames(global.sums)
rownames(global.sums) <- 1:nrow(global.sums)
global.sums <- global.sums %>%
  tidyr::separate(topic, into =c("t","topic")) %>% 
  select(-t) 

colnames(global.mults)<-c("mult","topic")
colnames(global.sums)<-c("p","topic")
colnames(global.means)<-c("gtp","topic")

global.means %>% write.csv2("./results/twitter-trained/k9-global-means.csv", row.names= F, quote = F)

gen<-merge(global.mults,global.sums, by="topic")
gen<-merge(gen,global.means, by="topic")

generality<-gen%>%
  mutate(diff=(1/doc.nb)*(1/gtp)*(mult-(p*log2(doc.nb*gtp))))
generality<-generality%>%
  mutate(gen.val=-diff/log2(doc.nb))

print(doc.nb)

generality%>% select(topic,gen.val)%>%write.csv("./results/twitter-trained/generality-topics.csv",row.names = FALSE, quote = FALSE)

topic.labels<-read.csv("./results/twitter-trained/k-9-topic-words.csv")

topic.labels<-topic.labels%>% 
  group_by(topic) %>%
  summarise(s=paste(word, collapse = ", "))

topic.labels<- topic.labels%>%
  group_by(topic) %>%
  summarize(label=paste(s,topic, collapse = ","))

fin <- global.means %>%
  mutate(topic=factor(topic,levels = 1:9,  labels=topic.labels$label))


g<-ggplot(fin, aes(x=reorder(topic,gtp), y=gtp))+
  geom_col( ) +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Mean probability of each topic in tweets")+
  # coord_flip()+
  xlab("Topics")+
  ylab("Global probability of topic")
ggsave(paste0(res_dir, "mean-probability-k9-all-tweets.pdf"))


#########################
colnames(buckets)<-c("nb","lower","upper")
buckets<-buckets%>%group_by(lower)%>%summarise(total=sum(nb))%>%arrange(lower)
buckets<-cbind(buckets,seq(1:6))
colnames(buckets)<-c("lower","total","buck")
buckets<-buckets%>%mutate(buck=factor(buck,levels = 1:6,  labels=c("<0.5","0.6-0.5","0.7-0.6","0.8-0.7","0.9-0.8",">0.9")))

g<-ggplot(buckets, aes(y=total, x=buck))+
  geom_bar(stat="identity",position="stack")+
  # theme(axis.text.x = element_text(angle = 90))+
  xlab("probability value")+
  ylab("nb tweets")+ 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggsave(paste0(res_dir,"k9-prob-buckets.pdf"))



# file.info<-data.frame()
# for (i in data.files){
#   print(i)
#   document_topic <- read.csv(paste0(data_dir,i), stringsAsFactors = FALSE, sep=",", quote = "\"", fileEncoding = "UTF-8", na.strings = NA)
#   colnames(document_topic)<-c("id","t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
#   
#   document_topic$id<-document_topic$id%>%format( digits=20)
#   document_topic <- document_topic %>% 
#     reshape2::melt(id.vars = "id") %>% 
#     rename(topic = variable) %>% 
#     tidyr::separate(topic, into =c("t","topic")) %>% 
#     select(-t) %>% 
#     group_by(id) %>% 
#     arrange(desc(value))
#   
#   base.inf <- base.inf %>% mutate(brackets=(value/doc.nb)*1/gtp*(log2(value/(doc.nb*gtp))))
#   
#   gen<-base.inf%>%
#     group_by(topic)%>%
#     summarize(gen=-sum(brackets))#*(1/normalize))
#   
#   generality <-dplyr::inner_join(gen, global.probabilities, by = "topic") %>% 
#     mutate(generality=gen*(1/log2(doc.nb)))#*1/normalize)
#   
#   file.info<-bind_rows(file.info,means)  
#   
#   
#   # df%>%write.csv(paste0(res_dir,filename), quote = TRUE, row.names = FALSE)
# }