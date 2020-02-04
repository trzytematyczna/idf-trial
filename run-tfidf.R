library(jsonlite)
library(tidytext)
library(dplyr)
library(forcats)
library(ggplot2)

json_data <- fromJSON(txt = "uk_parliament_climatechange.json")$speeches


speeches_words <- json_data %>% 
                  unnest_tokens(word, text) %>% 
                  count(id, word, sort = TRUE)

total_words <- speeches_words %>% 
              group_by(id) %>% 
               summarize(total = sum(n))

#speeches_words <- left_join(speeches_words, total_words)


#speeches_frequency <- speeches_words %>% 
#                    group_by(id) %>%
#                    mutate("tf" = n/total)


speeches <- speeches_words %>%
            bind_tf_idf(word, id, n) %>%
            arrange(desc(tf_idf))


speeches_plot <- speeches_words %>%
  bind_tf_idf(word, id, n) %>%
  mutate(word = fct_reorder(word, tf_idf))# %>%
  #mutate(author = factor(author, levels = c("Galilei, Galileo",
  ##                                          "Huygens, Christiaan", 
   #                                         "Tesla, Nikola",
   #                                         "Einstein, Albert")))
speeches_plot %>% 
  group_by(id) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~id, ncol = 2, scales = "free") +
  coord_flip()