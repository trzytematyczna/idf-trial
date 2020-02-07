library(jsonlite)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)

json_data <- fromJSON(txt = "uk_parliament_climatechange.json")$speeches
json_data$date <- as.Date(json_data$date , format = "%Y-%m-%d")

outlier_dates <- c('2016-01-04','2016-01-18','2016-03-14','2016-07-11','2016-09-05','2019-02-25','2019-03-18','2019-04-22','2019-04-29','2019-06-10','2019-06-24','2019-07-01','2019-07-08','2019-10-14 ')
outlier_dates <- as.Date(outlier_dates, format = "%Y-%m-%d")


week_data <- data.frame() #selected speeches only between outlier_dates (selected by Robin's alg)
for (dat in outlier_dates) {
  t1 <- as.Date(dat, origin = "1970-01-01")
  t2 <- as.Date(dat+6, origin = "1970-01-01")
  week_data <- rbind(week_data, cbind(json_data[json_data$date %in% t1:t2, ], c(t1)))
}

names(week_data)[names(week_data) == "c(t1)"] <- "week_start"

week_words <- week_data %>% 
  unnest_tokens(word, text, to_lower = TRUE) %>%
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>% 
  count(week_start, word, sort = TRUE)

week <- week_words %>%
  bind_tf_idf(word, week_start, n) %>%
  arrange(desc(tf_idf))  

week <- week %>%  arrange(desc(week_start, tf_idf))

number_of_top<-15 #number of top words
week_top <- week %>% 
  group_by(week_start) %>% 
  arrange(week_start, desc(tf_idf)) %>% 
  top_n(number_of_top, tf_idf)


write.csv2(week_top, file = paste(getwd(),"/top15_topics_per_week.csv", sep=""), quote=FALSE, row.names = FALSE)
