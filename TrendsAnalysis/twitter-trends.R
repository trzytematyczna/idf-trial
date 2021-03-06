# library(changepoint)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)


data_dir <- "../results/twitter-trained/sorted/"
res_dir<-"../results/twitter-trained/trends/"
options <- commandArgs(trailingOnly = TRUE)
m<-options[1]
# filename<-paste0("sorted-assign-",m,".csv")
filename<-paste0("sorted-assign-",m,".csv")

df<-read_csv(paste0(data_dir,filename), col_types = cols (id = col_character()))
df$date<-as.Date(df$date)
# colnames(df)<-c("id","date","retweetcount","from_user_id","from_user_name","from_user_followercount","text","t_1","t_2","t_3","t_4","t_5","t_6","t_7","t_8","t_9")
# df<-df[1:2000000,]

probs<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
  # mutate(month=format(date,divide.by))%>%
  gather(topic, probability, t_1:t_9) %>% ##topic_number k_list
  tidyr::separate(topic, into =c("t","topic")) %>%
  select(-t)%>%
  select(-id)

grouped.sp <- probs%>%
  group_by(date,topic) %>%
  summarise(sum_probability=mean(probability))


data <- grouped.sp %>%
  group_by(topic) %>%
  mutate(index=dplyr::row_number())# %>%
#  select(-date)

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


# Prepare labels for dates
label_dates <- data %>%
  filter(topic == 1) %>%
  pull(date) %>%
  as.character()
num_of_indices <- data %>%
  pull(index) %>%
  max()

# p <- ggplot(data.selected, aes(x=index, y=sum_probability, ymax=0.38)) +
#   geom_step() +
#   # geom_line()+
#   geom_hline(data=avg_data.selected, aes(yintercept=mean), lty=2, color="red") +
#   geom_rect(data=non_unitary_segments_data.selected,
#             inherit.aes = FALSE,
#             aes(xmin=seg_start, xmax=seg_end, ymin=min_prob, ymax=max_prob),
#             alpha=0.3,
#             fill="green") +
#   facet_grid(topic~., scales = "free_y") +
#   ylab("Probability")+
#   xlab("Date")+
#   # ylim(0.3, 0.37)+
#   scale_x_continuous(breaks = seq(1,num_of_indices,4), labels=label_dates[seq(1,num_of_indices,4)]) +
#   theme(axis.text.x = element_text(angle = 90, hjust=1))

#only segments
p <- ggplot(data, aes(x=index, y=sum_probability)) +
  geom_step() +
  # geom_line()+
  geom_hline(data=avg_data, aes(yintercept=mean), lty=2, color="red") +
  geom_rect(data=non_unitary_segments_data,
            inherit.aes = FALSE,
            aes(xmin=seg_start, xmax=seg_end, ymin=min_prob, ymax=max_prob),
            alpha=0.3,
            fill="green") +
  facet_grid(topic~., scales = "free_y")+
  ylab("Probability")+
  xlab("Date")+
  # ylim(0.3, 0.37)+
  scale_x_continuous(breaks = seq(1,num_of_indices,4), labels=label_dates[seq(1,num_of_indices,4)]) +
  theme(axis.text.x = element_text(angle = 90, hjust=1))

p
ggsave(paste0(res_dir,"events-",m,".pdf"), device = "pdf",scale=2)

#segments with trends
q <- ggplot(data, aes(x=index, y=sum_probability)) +
  geom_step() +
  # geom_line()+
  geom_hline(data=avg_data, aes(yintercept=mean), lty=2, color="red") +
  geom_rect(data=trends_trend,
            inherit.aes = FALSE,
            aes(xmin=index, xmax=index+1, ymin=0, ymax=0.3, fill=trend),
            alpha=0.3) +
  facet_grid(topic~., scales = "free_y")
ggsave(paste0(res_dir,"trends-",m,".pdf"), device = "pdf",scale=2)

