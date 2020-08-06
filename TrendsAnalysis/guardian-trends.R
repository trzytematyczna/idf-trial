# library(changepoint)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)


year<-2019
# grouped.sp <- read_csv("./results/guardian-articles-old/all-day-probs-mars17-may17.csv")
guardian.data <- read_csv("./results/guardian-articles/probabilities/guardian-probs-daily-all-years.csv", col_types = cols(date = col_date()))
# grouped.sp<-selc
grouped.sp.filtered <- guardian.data %>% filter(date>as.Date(paste0(year,"-01-01")),date<as.Date(paste0(year,"-12-31")))
grouped.sp <- grouped.sp.filtered %>% 
  filter(!is.na(grouped.sp.filtered$date))

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
  mutate(trend = ifelse(abs(diff)<0.01, "same", ifelse(diff<0, "down", "up")))

# Prepare labels for dates
label_dates <- data %>%
  filter(topic == data$topic[1]) %>%
  pull(date) %>%
  as.character()
num_of_indices <- data %>%
  pull(index) %>%
  max()

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
  facet_grid(topic~., scales = "free_y") +
  xlab("Probability")+
  ylab("Date")+
  scale_x_continuous(breaks = seq(1,num_of_indices,3), labels=label_dates[seq(1,num_of_indices,3)]) +
  theme(axis.text.x = element_text(angle = 90, hjust=1))
p
ggsave(paste0("./results/guardian-articles/guardian-trends/guardian-events-",year,".pdf"), device = "pdf", scale = 2)

#segments with trends
q <- ggplot(data, aes(x=index, y=sum_probability)) +
  geom_step() +
  # geom_line()+
  geom_hline(data=avg_data, aes(yintercept=mean), lty=2, color="red") +
  geom_rect(data=trends_trend,
            inherit.aes = FALSE,
            aes(xmin=index, xmax=index+1, ymin=0, ymax=0.7, fill=trend),
            alpha=0.3) +
  facet_grid(topic~., scales = "free_y")
q
ggsave(paste0("./results/guardian-articles/guardian-trends/guardian-trends-",year,".pdf"), device = "pdf", scale = 2)

