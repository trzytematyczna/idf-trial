library(dplyr)
library(data.table)
library(stringr)
library(DescTools)
library(ggplot2)
library(readr)

data_dir<-"./results/twitter-trained/assign-joined/"

data.files <- list.files(data_dir)
res_dir<-"./results/twitter-trained/"
file.means<-data.frame()
doc.nb<-0
for (i in data.files){
  print(i)
  df <- read_csv(paste0(data_dir,i), col_types = cols (id = col_character()))
  df <- df %>% mutate(tcount = retweetcount+1)
  df[8:16] <- df[["tcount"]] * df[(8:16)]
  means<-df %>% select(8:16) %>% colSums()
  means <- means / sum(df$tcount)
  file.means<-bind_rows(file.means,means)  
  doc.nb<-doc.nb+sum(df$tcount)
  
}

global.means <- file.means %>% colMeans()
global.means<-global.means%>%reshape2::melt()
global.means$topic<-rownames(global.means)
rownames(global.means) <- 1:nrow(global.means)
global.means <- global.means %>%
  tidyr::separate(topic, into =c("t","topic")) %>% 
  select(-t) 
colnames(global.means)<-c("gtp","topic")

global.means %>% write_csv("./results/twitter-trained/k9-global-means-retweets.csv")

topic.labels<-read_csv("./results/twitter-trained/k-9-topic-words.csv")

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
ggsave(paste0(res_dir, "mean-probability-k9-all-tweets-retweets.pdf"))
