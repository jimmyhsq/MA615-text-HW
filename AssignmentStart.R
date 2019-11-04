library(tidytext)
library(dplyr)
library(stringr)
library(magrittr)
library(gutenbergr)
library(ggplot2)
library(scales)

#Include this for the start
data(stop_words)

# Read in this book.
book <- gutenberg_download(c(768))

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


