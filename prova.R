library(wordcloud)
library(dplyr)

setwd('/home/nicole/Data Science/exam_big_data')
a <- read.delim("p.txt")

a %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
