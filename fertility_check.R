{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(plotly)
}

setwd('/home/nicole/Data Science/exam_big_data')
f <- read.csv("Clean/fertility_clean.csv")
f <- f %>%
  filter(Country=="World")
