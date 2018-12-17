{
  library(leaflet)
  library(dplyr)
  library(rgdal)
  library(ggplot2)
  library(rworldmap)
  library(countrycode)
  library(tidyr)
  library(stringr)
}

setwd('/home/nicole/Data Science/exam_big_data/Clean')
population <- read.csv("population_clean.csv")

mypalette = colorNumeric( palette="viridis", domain=world_spdf@data$POP2005, na.color="transparent")
mypalette(c(45,43))

leaflet(population) %>%
  addTiles()  %>%
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( fillColor = ~mypalette(POP2005), stroke=FALSE )
