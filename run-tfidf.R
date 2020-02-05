library(jsonlite)
library(tidytext)
library(dplyr)
#library(forcats)
library(ggplot2)
library(stringr)

json_data <- fromJSON(txt = "uk_parliament_climatechange.json")$speeches

discussions_words <- json_data %>% 
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>% #removing stop words
#  mutate(word = SnowballC::wordStem(word)) %>% # tokens -> roots (Porter stemming algorithm) 
  count(discussion_title, word, sort = TRUE)

discussions <- discussions_words %>%
  bind_tf_idf(word, discussion_title, n) %>%
  arrange(desc(tf_idf))  

discussions <- discussions %>%  arrange(desc(discussion_title, tf_idf))

number_of_top<-1 #number of top words
discussions_top <- discussions %>% 
  group_by(discussion_title) %>% 
  arrange(discussion_title, desc(tf_idf)) %>% 
  top_n(number_of_top, tf_idf)

 # length(discussions_top$word)
 # length(unique(discussions_top$word))


discussions_top <-discussions_top %>% 
  inner_join(json_data%>% select(date, discussion_title) %>% distinct(), by = "discussion_title")

json_data$id

discussions_count <- json_data %>% 
  count(discussion_title, date)
  # group_by(discussion_title, date) %>%
  # summarise(count = n())
  

ggplot(discussions_count, aes(x=date)) + 
  geom_line(aes(y=n)) + 
  labs(title="Time Series Chart",
       x="Date",
       y="Number of speakers")



