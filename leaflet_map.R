{
  library(leaflet)
  library(dplyr)
  library(rgdal)
  library(ggplot2)
  library(geojsonio)
  library(rworldmap)
  library(countrycode)
  library(htmlwidgets)
  library(htmltools)
  library(tidyr)
  library(stringr)
}

setwd('/home/nicole/Data Science/exam_big_data/Clean')
population2 <- read.csv("population_clean.csv")
population <- read.csv("population_clean_with_NAs.csv")
density2 <- read.csv("density_clean.csv")
density <- read.csv("density_clean_with_NAs.csv")
population <- density


colnames(population)[2] <- "name"
{
levels(population$name) <- c(levels(population$name), "United States of America")
population$name[population$name == 'United States'] <- "United States of America"

levels(population$name) <- c(levels(population$name), "Russia")
population$name[population$name == 'Russian Federation'] <- "Russia"

levels(population$name) <- c(levels(population$name), "Democratic Republic of the Congo")
population$name[population$name == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"

levels(population$name) <- c(levels(population$name), "Iran")
population$name[population$name == "Iran, Islamic Rep."] <- "Iran"

levels(population$name) <- c(levels(population$name), "Republic of Serbia")
population$name[population$name == "Serbia"] <- "Republic of Serbia"

levels(population$name) <- c(levels(population$name), "Egypt")
population$name[population$name == "Egypt, Arab Rep."] <- "Egypt"

levels(population$name) <- c(levels(population$name), "Venezuela")
population$name[population$name == 'Venezuela, RB'] <- "Venezuela"

levels(population$name) <- c(levels(population$name), "United Republic of Tanzania")
population$name[population$name == "Tanzania"] <- "United Republic of Tanzania"

levels(population$name) <- c(levels(population$name), "Yemen")
population$name[population$name == 'Yemen, Rep.'] <- "Yemen"

levels(population$name) <- c(levels(population$name), "Ivory Coast")
population$name[population$name == "Cote d'Ivoire"] <- "Ivory Coast"

levels(population$name) <- c(levels(population$name), "Kyrgyzstan")
population$name[population$name == "Kyrgyz Republic"] <- "Kyrgyzstan"

levels(population$name) <- c(levels(population$name), "Syria")
population$name[population$name == "Syrian Arab Republic"] <- "Syria"

levels(population$name) <- c(levels(population$name), "Republic of the Congo")
population$name[population$name == "Congo, Rep."] <- "Republic of the Congo"

levels(population$name) <- c(levels(population$name), "Laos")
population$name[population$name == "Lao PDR"] <- "Laos"

levels(population$name) <- c(levels(population$name), "Slovakia")
population$name[population$name == "Slovak Republic"] <- "Slovakia"

levels(population$name) <- c(levels(population$name), "Macedonia")
population$name[population$name == "Macedonia, FYR"] <- "Macedonia"

levels(population$name) <- c(levels(population$name), "Guinea Bissau")
population$name[population$name == "Guinea-Bissau"] <- "Guinea Bissau"

levels(population$name) <- c(levels(population$name), "South Korea")
population$name[population$name == "Korea, Rep."] <- "South Korea"

levels(population$name) <- c(levels(population$name), "North Korea")
population$name[population$name == "Korea, Dem. People’s Rep."] <- "North Korea"
}

setwd('/home/nicole/Data Science/exam_big_data')
states <- geojsonio::geojson_read("prova.geo.json", what = "sp")
class(states)

prova <- merge(states, population, by="name")
#class(prova)

m <- leaflet(prova) %>%
  setView(-1, 42, zoom=1) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
#m %>% addPolygons()

bins <- c(0, 10, 20, 50, 100, 200, 400, 500, 1000, Inf)
#bins_totpop <- c(0, 100000, 200000, 500000, 1000000, 2000000, 5000000, 10000000, 100000000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)




labels <- sprintf(
  "<strong>%s</strong><br/>%g people / m<sup>2</sup>",
  prova$name, prova$Density2017
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = ~pal(Density2017),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))
m
# m <- m %>% addLayersControl(baseGroups = c("Density1990", "Density2017"),
#                        #overlayGroups = c("Density1990", "Density2017"),
#                        options = layersControlOptions(collapsed = FALSE))
#m
#chart_link = api_create(m, filename="worldmap")
#chart_link

setwd('/home/nicole/Data Science/exam_big_data')
#saveWidget(m, 'map.html', selfcontained = TRUE)
