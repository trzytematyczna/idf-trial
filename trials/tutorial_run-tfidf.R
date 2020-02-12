library(jsonlite)
library(tidytext)
library(dplyr)
#library(forcats)
library(ggplot2)
library(stringr)

json_data <- fromJSON(txt = "uk_parliament_climatechange.json")$speeches


speeches_words <- json_data %>% 
                  unnest_tokens(word, text) %>% 
                  count(id, word, sort = TRUE)

#deleting stopwords
speeches_words <- anti_join(speeches_words, stop_words, 
                           by = "word")

speeches_total_words <- speeches_words %>% 
  group_by(id) %>% 
  summarize(total = sum(n))

#speeches_words <- left_join(speeches_words, total_words)


#speeches_frequency <- speeches_words %>% 
#                    group_by(id) %>%
#                    mutate("tf" = n/total)

speeches <- speeches_words %>%
            bind_tf_idf(word, id, n) %>%
            arrange(desc(tf_idf))


discussions_words <- json_data %>% 
                    group_by(discussion_title) %>%
                    unnest_tokens(word, text) %>%
                    filter(!str_detect(word, "^[0-9]*$")) %>%
                    anti_join(stop_words) %>% #removing stop words
                    mutate(word = SnowballC::wordStem(word)) # tokens -> roots (Porter stemming algorithm) 

discussions <- discussions_words %>%
              bind_tf_idf(word, discussion_title, n) %>%
              arrange(desc(tf_idf))  

discussions <- discussions %>%  arrange(desc(discussion_title, tf_idf))



top<-1 #number of top words
discussions_top <- discussions %>% 
                group_by(discussion_title) %>% 
                arrange(discussion_title, desc(tf_idf)) %>% 
                top_n(top, tf_idf)


speeches_plot <- speeches_words %>%
  bind_tf_idf(word, id, n) %>%
  mutate(word = fct_reorder(word, tf_idf))# %>%
  #mutate(author = factor(author, levels = c("Galilei, Galileo",
  ##                                          "Huygens, Christiaan", 
   #                                         "Tesla, Nikola",
   #                                         "Einstein, Albert")))
# speeches_plot %>% 
#   group_by(id) %>% 
#   top_n(15, tf_idf) %>% 
#   ungroup() %>%
#   mutate(word = reorder(word, tf_idf)) %>%
#   ggplot(aes(word, tf_idf, fill = id)) +
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "tf-idf") +
#   facet_wrap(~id, ncol = 2, scales = "free") +
#   coord_flip()