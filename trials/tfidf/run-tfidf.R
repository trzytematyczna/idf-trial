require(jsonlite)
require(tidytext)
require(dplyr)
require(ggplot2)
require(stringr)

json_data <- fromJSON(txt = "uk_parliament_climatechange.json")$speeches

discussions_words <- json_data %>% 
  unnest_tokens(word, text, to_lower = TRUE) %>%
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>% #removing stop words
#  mutate(word = SnowballC::wordStem(word)) %>% # tokens -> roots (Porter stemming algorithm) 
  count(discussion_title, word, sort = TRUE)


discussions <- discussions_words %>%
  bind_tf_idf(word, discussion_title, n) %>%
  arrange(desc(tf_idf))  

discussions <- discussions %>%  arrange(desc(discussion_title, tf_idf))

number_of_top<-15 #number of top words
discussions_top <- discussions %>% 
  group_by(discussion_title) %>% 
  arrange(discussion_title, desc(tf_idf)) %>% 
  top_n(number_of_top, tf_idf)


discussions_top <-discussions_top %>% 
  inner_join(json_data%>% select(date, discussion_title) %>% distinct(), by = "discussion_title")

write.csv2(discussions_top, file = "/home/monika/Documents/UMPC/Timeline/idf-trial/top15_topics_per_discussion_title.csv", quote=FALSE, row.names = FALSE)

discussions_count <- json_data %>% 
  count(discussion_title, date)
  
ggplot(discussions_count, aes(x=date)) + 
  geom_line(aes(y=n)) + 
  labs(title="Time Series Chart",
       x="Date",
       y="Number of speakers")




################### dataset info
# unique(json_data[c("date","discussion_title")]) ##same discusssions on different days 
# length(unique(json_data$date)) #344
# length(unique(json_data$discussion_title)) #560
# length(discussions_top$word)
# length(unique(discussions_top$word))

