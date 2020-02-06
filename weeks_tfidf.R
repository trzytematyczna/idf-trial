library(jsonlite)
library(tidytext)
library(dplyr)
#library(forcats)
library(ggplot2)
library(stringr)

json_data <- fromJSON(txt = "uk_parliament_climatechange.json")$speeches
json_data$date <- as.Date(json_data$date , format = "%Y-%m-%d")

outlier_dates <- c('2016-01-04','2016-01-18','2016-03-14','2016-07-11','2016-09-05','2019-02-25','2019-03-18','2019-04-22','2019-04-29','2019-06-10','2019-06-24','2019-07-01','2019-07-08','2019-10-14 ')
outlier_dates <- as.Date(outlier_dates, format = "%Y-%m-%d")

#t1<-as.Date(outlier_dates[1], format = "%Y-%m-%d")
#t2<-as.Date(outlier_dates[1]+7,  format = "%Y-%m-%d")

#asd<-json_data[json_data$date %in% t1:t2, ]

week_data <- data.frame() # 
for (dat in outlier_dates) {
  t1 <- as.Date(dat, origin = "1970-01-01")
  t2 <- as.Date(dat+6, origin = "1970-01-01")
  week_data <- rbind(week_data, json_data[json_data$date %in% t1:t2, ])
}


#this is wrong 
week_words <- week_data %>% 
  unnest_tokens(word, text, to_lower = TRUE) %>%
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>% #removing stop words
  count(here_Is_wrong, word, sort = TRUE)

#here_is_wrong -> should be the column with id of the WEEK

week <- week_words %>%
  bind_tf_idf(word, here_is_wrong, n) %>%
  arrange(desc(tf_idf))  

week <- week %>%  arrange(desc(tf_idf))

# number_of_top<-15 #number of top words
# week_top <- week %>% 
#   group_by(here_is_wrong) %>% 
#   arrange(id, desc(tf_idf)) %>% 
#   top_n(number_of_top, tf_idf)



discussions_top <-discussions_top %>% 
  inner_join(json_data%>% select(date, discussion_title) %>% distinct(), by = "discussion_title")

write.csv2(discussions_top, file = "/home/monika/Documents/UMPC/Timeline/idf-trial/top15_topics_per_discussion_title.csv", quote=FALSE, row.names = FALSE)

discussions_count <- json_data %>% 
  count(discussion_title, date)
# group_by(discussion_title, date) %>%
# summarise(count = n())


ggplot(discussions_count, aes(x=date)) + 
  geom_line(aes(y=n)) + 
  labs(title="Time Series Chart",
       x="Date",
       y="Number of speakers")


