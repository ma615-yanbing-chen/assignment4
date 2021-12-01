######





##### Task 2

#--------------------------------------------------------

devtools::install_github("Truenumbers/tnum/tnum")
library(tnum)
library(gutenbergr)
library(tidytext)
library(textdata)
library(dplyr)
library(stringr)
library(tidyverse)
library(tidyr)
library(scales)
library(wordcloud)
library(reshape2)
library(ggplot2)

#####
#------------------------------------------------------------------------------
book<-gutenberg_download(11)
tidy_Alice <- book %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>% 
  unnest_tokens(word, text) 


get_sentiments("bing")
bing_neg <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")
bing_pos<-get_sentiments("bing") %>%
  filter(sentiment == "positive")

tidy_Alice %>%
  inner_join(bing_neg) %>%
  count(word, sort = TRUE)

##
Alice_sentiment <- tidy_Alice %>%
  inner_join(get_sentiments("bing")) %>%
  count( index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

p1<-ggplot(Alice_sentiment,aes(index,sentiment),color="lightblue")+
  geom_col(show.legend = FALSE)
p1


#2.3
#textdata::lexicon_nrc(delete = TRUE)
#nrc <- textdata::lexicon_nrc()

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

afinn <- tidy_Alice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  tidy_Alice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_Alice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


####2.4
bing_word_counts<-tidy_Alice %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort=TRUE) %>%
  ungroup()
bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

custom_stop_words<-bind_rows(tibble(word=c("miss"),
                                    lexicon=c("custom")),
                             stop_words)
custom_stop_words

###2.5 Wordclouds
##
tidy_Alice %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n,max.words = 100))

## reshape the wordcloud
tidy_Alice %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort=TRUE) %>%
  acast(word~sentiment,value.var = "n",fill=0) %>%
  comparison.cloud(colors=c("gray20","gray80"),
                   max.words = 100)
#------------------------------------------------------------------------------

#############Task 3
