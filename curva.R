{
  library(dplyr)
  library(ggplot2)
  library(rworldmap)
  library(countrycode)
  library(tidyr)
  library(stringr)
}

setwd('/home/nicole/Data Science/exam_big_data')
growth <- read.csv("Clean/growth_clean.csv")
growth <- growth[-nrow(growth),]

?glm

