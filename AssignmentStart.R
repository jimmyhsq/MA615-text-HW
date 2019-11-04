library(tidytext)
library(dplyr)
library(stringr)
library(magrittr)
library(gutenbergr)
library(ggplot2)
library(scales)
library(tidyverse)

#Include this for the start
data(stop_words)

# Read in this book.
book <- gutenberg_download(c(1184))

# Tidy the table by excluding common words and 
tidybook <- book %>% 
  mutate(linenumber=row_number(),
         chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",
                                                ignore_case=T)))) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)


tidybook %>% count(word,sort=T)

library(ggplot2)

tidybook %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#Sentiment Comparison
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidybook %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


sentiment <- tidybook %>%
  inner_join(get_sentiments("bing")) %>%
  count(chapter, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)

