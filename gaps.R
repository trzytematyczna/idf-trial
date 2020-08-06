library(data.table)
library(dplyr)
library(readr)
library(tidyr)

data_dir<-"./results/twitter-trained/assign-joined/"


options <- commandArgs(trailingOnly = TRUE)
m<-options[1]
# filename<-paste0("sorted-assign-",m,".csv")
filename<-paste0("sorted-assign-",m,".csv")
df <- read_csv(paste0(data_dir,filename), col_types = cols (id = col_character()))
df$date<-as.Date(df$date)

probs.tweets<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
    gather(topic, probability, t_1:t_9) %>% ##topic_number k_list
    tidyr::separate(topic, into =c("t","topic")) %>%
    select(-t)%>%
    select(-id)
  
  grouped.tweets <- probs.tweets%>%
    group_by(date) %>%
    summarise(nbtweets=n())
  
  all.dates<-data.table(date=seq.Date(min(df$date),max(df$date), by='day'))
  
  gaps <- full_join(grouped.tweets,all.dates) %>% 
    arrange(date)
  
  as.data.frame(gaps) %>% write_csv(paste0("./results/twitter-trained/gaps/gaps-",m,".csv"))
  