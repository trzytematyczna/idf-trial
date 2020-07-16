# library(changepoint)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

selected.topic<-6
tweets<-F
data_dir <- "./results/twitter-trained/assign-joined/assign-3.csv"
df<-read_csv(data_dir, col_types = cols (id = col_character()))
df$date<-as.Date(df$date)
# colnames(df)<-c("id","date","retweetcount","from_user_id","from_user_name","from_user_followercount","text","t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
# df<-df[1:2000000,]
if(tweets){
  what<-"tweets"
  probs<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
  # mutate(month=format(date,divide.by))%>%
  gather(topic, probability, t_1:t_9) %>% ##topic_number k_list
  tidyr::separate(topic, into =c("t","topic")) %>%
  select(-t)%>%
  select(-id)

grouped.sp <- probs%>%
  group_by(date,topic) %>%
  summarise(sum_probability=mean(probability))
}else{ ##retweets
  what<-"RT"
  
  probs.rt<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
    pivot_longer(names_to="topic", values_to="probability", c(t_1:t_9)) %>% ##topic_number k_list
    tidyr::separate(topic, into =c("t","topic")) %>%
    select(-t)%>%
    select(-id)
  
  retweets.only <- probs.rt %>% 
    filter(retweetcount >0)
  retweets.only <- retweets.only %>%
    mutate(mprob=probability*retweetcount)
  
  grouped.retweets.only<- retweets.only %>%
    group_by(date,topic) %>%
    summarise(sumpro=sum(mprob),sumtweet=sum(retweetcount))%>%
    mutate(sum_probability=sumpro/sumtweet)
  
  grouped.sp<-grouped.retweets.only%>%
    select(-sumpro,-sumtweet)
  
  
}

data <- grouped.sp %>%
  group_by(topic) %>%
  mutate(index=dplyr::row_number()) #%>%
  #select(-date)


avg_data <- data %>%
  group_by(topic) %>%
  summarise(mean=mean(sum_probability),
            median=median(sum_probability))

p <- ggplot(data, aes(x=index, y=sum_probability)) +
  geom_hline(data=avg_data, aes(yintercept=mean), lty=2, color="red") +
  geom_step() +
  facet_grid(topic~., scales = "free")
p

segments_data <- data %>%
  group_modify(function(d, ...) {
    # Compute average
    avg_probability <- d %>% pull(sum_probability) %>% mean()
    min_probability <- d %>% pull(sum_probability) %>% min()
    max_probability <- d %>% pull(sum_probability) %>% max()
    # Create mask
    with_mask <- d %>% mutate(mask = sum_probability > avg_probability)
    # Find segments
    with_segments <- with_mask %>%
      mutate(segment = ifelse(mask, 1, 0))
    current_segment <- 1
    for (n in 2:nrow(with_segments)) {
      if (with_segments[n, ]$mask) {
        if (with_segments[n-1, ]$segment > 0) {
          with_segments[n, ]$segment <- current_segment
        } else {
          current_segment <- current_segment + 1
          with_segments[n, ]$segment <- current_segment
        }
      }
    }
    # Find segments start/end points
    only_segments <- with_segments %>%
      filter(segment > 0)
    
    coord_segments_min <- only_segments %>%
      group_by(segment) %>%
      filter(index == min(index)) %>%
      mutate(seg_start=index) %>%
      ungroup() %>%
      select(seg_start, segment)
    coord_segments_max <- only_segments %>%
      group_by(segment) %>%
      filter(index == max(index)) %>%
      mutate(seg_end=index) %>%
      ungroup() %>%
      select(seg_end) ##segment already in min tibble
    all_coords <- cbind(coord_segments_min, coord_segments_max) %>%
      mutate(seg_end=seg_end+1) %>% ### for geom_step --> visualisation moved by 1
      mutate(min_prob = min_probability,
             max_prob = max_probability)
    
    all_coords
  })

##seg_len is length of the segment-1 (!) so one-time peaks will have seg_len = 0
non_unitary_segments_data <- segments_data %>%
  mutate(seg_len=seg_end-seg_start)%>%
  filter(seg_len>2)



trends_segments <- non_unitary_segments_data %>%
  group_by(topic,segment) %>%
  group_modify(function(d, ...) {
    
    avg_prob <- avg_data %>% 
      filter(topic == d$topic) %>%
      pull(mean)
    
    data %>%
      filter(topic == d$topic) %>%
      ungroup() %>%
      select(-topic) %>% 
      filter(index >= d$seg_start,index <= d$seg_end) %>%
      mutate(diff = sum_probability - lag(sum_probability, n=1, default= avg_prob))
    # data %>% filter_at(index, between(.,d$seg_start, d$seg_end))
  }, keep = TRUE)

trends_area <- trends_segments %>% 
  group_by(topic,segment) %>%
  summarise(area = sum(sum_probability))
trends_trend <- trends_segments %>% 
  mutate(trend = ifelse(abs(diff)<0.012, "same", ifelse(diff<0, "down", "up")))

non_unitary_segments_data.selected<-non_unitary_segments_data%>%filter(topic==selected.topic)
data.selected<-data%>%filter(topic==selected.topic)
trends_trend.selected<-trends_trend%>%filter(topic==selected.topic)
avg_data.selected<-avg_data%>%filter(topic==selected.topic)

# Prepare labels for dates
label_dates <- data %>%
  filter(topic == 1) %>%
  pull(date) %>%
  as.character()
num_of_indices <- data %>%
  pull(index) %>%
  max()


#only segments
p <- ggplot(data.selected, aes(x=index, y=sum_probability, ymax=0.38)) +
  geom_step() +
  # geom_line()+
  geom_hline(data=avg_data.selected, aes(yintercept=mean), lty=2, color="red") +
  geom_rect(data=non_unitary_segments_data.selected,
            inherit.aes = FALSE,
            aes(xmin=seg_start, xmax=seg_end, ymin=min_prob, ymax=max_prob),
            alpha=0.3,
            fill="green") +
  facet_grid(topic~., scales = "free_y") +
  ylab("Probability")+
  xlab("Date")+
  # ylim(0.3, 0.37)+
  scale_x_continuous(breaks = seq(1,num_of_indices,4), labels=label_dates[seq(1,num_of_indices,4)]) +
  theme(axis.text.x = element_text(angle = 90, hjust=1))
p
ggsave(paste0("./results/twitter-trained/twitter",what,"-green-t6-solo.pdf"), device = "pdf")
# save(p,file = "twitter-tweets-green-solo.Rds")
# save(p,file = "twitter-RT-green-solo.Rds")
#segments with trends
q <- ggplot(data.selected, aes(x=index, y=sum_probability)) +
  geom_step() +
  # geom_line()+
  geom_hline(data=avg_data.selected, aes(yintercept=mean), lty=2, color="red") +
  geom_rect(data=trends_trend.selected,
            inherit.aes = FALSE,
            aes(xmin=index, xmax=index+1, ymin=0, ymax=0.37, fill=trend),
            alpha=0.3)

  # facet_grid(topic~., scales = "free_y")
q
ggsave("./results/twitter-trained/trands-events-t6.pdf", device = "pdf")


# q<-load("twitter-tweets-green-solo.Rds")
library(gridExtra)

pdf("green-both-t6.pdf")
grid.arrange(p,p,nrow=2)
dev.off()
# 
# 
# peak.df.fun <- function(df,topic, threshold, weeknb){
#   divide.by<-'%y-%U'
#   cols<-c("t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
#   selected.topic<-c(topic)
#   
#   # df.selected <- df %>% 
#   # filter_at(cols, any_vars(.>threshold))
#   
#   peak.df<- df %>% 
#     mutate(month=format(date, divide.by)) %>%
#     filter_at(cols, any_vars(.>threshold))
#   
#   peak.df <- peak.df[peak.df$month==weeknb,]
#   
#   peak.df
# }