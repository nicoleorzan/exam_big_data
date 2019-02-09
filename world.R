{
  library(dplyr)
  library(leaflet)
  library(ggplot2)
  library(RColorBrewer)
  library(countrycode)
  library(tidyr)
  library(stringr)
}

setwd('/home/nicole/Data Science/exam_big_data')

#### LOAD DATASETS ###
{
  countries_world <- read.csv("Datasets/countries of the world.csv", na.strings=c("","NA"))
  colnames(countries_world)[4]="Area"
  colnames(countries_world)[5]="Density"
  countries_world[,2] <- str_trim(countries_world[,2], "both") 
  immunization <- read.csv("Datasets/immunization.csv", skip=4)
  fert <- read.csv("Datasets/fertility.csv", skip=4)
  death <- read.csv("Datasets/death_rate.csv", skip=4)
  Gdp <- read.csv("Datasets/GDP_annual_growth.csv", skip=4)
  total_population <- read.csv("Datasets/total_population.csv", skip=4)
}



{ ##### START DATA CLEANING ####### --> DONE IN DATA_CLEANING

###====== Adding country code to world countries 
###====== using countrycode package !!!
  {
    countries_world$Country <- as.character(countries_world$Country)
    
    countries_world <- countries_world %>%
      mutate(Country=replace(Country, Country=="Central African Rep. ", "Central African Republic")) %>%
      mutate(Country=replace(Country, Country=="Netherlands Antilles ", "Netherlands Antilles"))
    
    countries_world$Country.Code <- 
      countrycode(countries_world$Country, origin='country.name', 
                  destination='iso3c')
    
    countries_world <- countries_world %>%
      mutate(Country.Code=replace(Country.Code, Country=="Netherlands Antilles", "NA")) %>%
      mutate(Country.Code=replace(Country.Code, Country=="Virgin Islands ", "VI")) %>%
      select(Country.Code, everything())
  }
  ###====== 
  
  #### taking piece of countries_world dataset 
  #### to deal with other datasets
  country_region <- data.frame(Country.Code=countries_world$Country.Code,
                               Country=countries_world$Country,
                               Region=countries_world$Region,
                               Area=countries_world$Area)
  country_region$Country.Code <- as.character(country_region$Country.Code)
  
  #######==== Dropping columns and rows which ONLY ======########
  #######==== CONTAIN NA's; adding Region and Continent ======########
  na_region_continent <- function(x, country_region){
    
    colnames(x) <- c("Country", "Country.Code", "Indicator.Name", "Indicator.Code", substring(colnames(x[,6:length(x)-1]), 2), "X")
    
    # eliminate NA
    x <- x %>% select_if(~sum(!is.na(.)) > 0)
    
    years <- x[,5:ncol(x)] %>%
      filter_all(any_vars(!is.na(.)))
    x <- semi_join(x, years, by=c("2014", "2015", "2016")) %>%
      select(-Indicator.Name, -Indicator.Code)
    
    # Add region
    x$Country.Code <- as.character(x$Country.Code)
    x <- left_join(x, country_region, "Country.Code") %>%
      select(Region, everything()) %>%
      select(-Country.y) %>%
      rename(Country = Country.x)
    
    # Add continent
    x <<- x %>%
      mutate(Continent = ifelse(Region=="NORTHERN AMERICA" | Region=="LATIN AMER. & CARIB", "America", ifelse(Region=="NORTHERN AFRICA" | Region=="SUB-SAHARAN AFRICA", "Africa", ifelse(Region=="BALTICS" | Region=="WESTERN EUROPE" | Region=="EASTERN EUROPE" | Region=="C.W. OF IND. STATES", "Europe", ifelse(Region=="NEAR EAST" | Region=="ASIA (EX. NEAR EAST)", "Asia", "Oceania"))))) %>%
      select(Country, Continent, everything())
  }
  
  {datas <- vector(mode="list", length=5)
    names(datas) <- c("immunization", "death", "Gdp", "total_population", "fert")
    datas[[1]] <- immunization; datas[[2]] <- death
    datas[[3]] <- Gdp; datas[[4]] <- total_population
    datas[[5]] <- fert
    
    for (i in (1:length(datas))){
      print(i);  print(names(datas)[i])
      tmp_data <- mget(names(datas)[i])
      sapply(tmp_data, na_region_continent, country_region=country_region)
      datas[[i]] <- x
    }
    immunization <- datas[[1]]; death <- datas[[2]]
    Gdp <- datas[[3]]; total_population <- datas[[4]]
    fert <- datas[[5]]
  }
  
  # cleaning additive data
  total_population <- total_population %>%
    mutate(Continent=replace(Continent, Country=="Kosovo", "Europe")) %>%
    mutate(Continent=replace(Continent, Country=="Virgin Islands (U.S.)", "America"))%>%
    mutate(Continent=replace(Continent, Country=="Vanuatu", "Oceania"))%>%
    mutate(Continent=replace(Continent, Country=="South Sudan", "Africa"))%>%
    mutate(Continent=replace(Continent, Country=="Montenegro", "Europe"))%>%
    mutate(Continent=replace(Continent, Country=="Sint Maarten (Dutch part)", "America"))%>%
    mutate(Continent=replace(Continent, Country=="Pacific island small states", "Asia"))%>%
    mutate(Continent=replace(Continent, Country=="St. Martin (French part)", "America"))%>%
    mutate(Continent=replace(Continent, Country=="Caribbean small states", "America"))%>%
    mutate(Continent=replace(Continent, Country=="Curacao", "America"))%>%
    mutate(Continent=replace(Continent, Country=="Channel Islands", "America"))

} ##### END DATA CLEANING #######

## HAVE A QUICK LOOK AT SOME VARIBALES

total_population %>%
  select(Country, Continent, `1960`) %>%
  na.omit()  %>%
  summarize(sum(`1960`))

na_pop <- total_population %>%
  select(Country, Continent, `1960`) %>%
  filter(is.na(Continent))#%>%
  filter(Country!="World")%>%
  summarize(sum(`1960`, na.rm=TRUE))

countries_world %>%
  group_by(Region) %>%
  count() %>%
  arrange(n)

countries_world %>%
  arrange(Population) %>%
  select(Country, Region, Population)

# Let's see how much population on total area per continent:
countries_world %>%
  group_by(Continent) %>%
  mutate(Continent_Area_km2 = sum(Area)*2.589988,Continent_Pop = sum(as.numeric(Population))) %>%
  mutate(Continent_Density_ppl_on_km2 = Continent_Pop/Continent_Area_km2) %>%
  select(Continent_Area_km2, Continent_Pop, Continent_Density_ppl_on_km2 , everything())%>%
  distinct(Continent_Area_km2, Continent_Pop, Continent_Density_ppl_on_km2) %>%
  arrange(desc(Continent_Area_km2))


# DENSITY VALUES OF COUNTRIES WORLD
{
  DensityValues <- as.double(gsub(",","",as.character(countries_world$Density),fixed=TRUE))
  countries_world$DensityValues <- DensityValues
  countries_world$StepDensity <- as.integer(rep(0, nrow(countries_world)))
  
  densitystep <- c(10, 25, 50, 60, 75, 100, 300, 500, 1000, 3000)
  densitystep <- as.integer(densitystep)
  for (i in 1:10){
    val <- countries_world$DensityValues>densitystep[i]
    countries_world$StepDensity[val]=i
  }
}


##################  SOME STARTING SIMPLE PLOTS #####################
ggplot(data = countries_world) +
  geom_bar(mapping = aes(x = StepDensity))

ggplot(countries_world, mapping=aes(x=log2(Population), y=log2(Area)))+
  geom_point(mapping=aes(color=StepDensity))

#png(filename="/home/nicole/Data Science/Exam_data_analysis/continent_distribution.png",width=650,height=400)
#QUESTO
countries_world <- countries_world %>% 
  mutate(Continent = ifelse(Region=="NORTHERN AMERICA" | Region=="LATIN AMER. & CARIB", "America", ifelse(Region=="NORTHERN AFRICA" | Region=="SUB-SAHARAN AFRICA", "Africa", ifelse(Region=="BALTICS" | Region=="WESTERN EUROPE" | Region=="EASTERN EUROPE" | Region=="C.W. OF IND. STATES", "Europe", ifelse(Region=="NEAR EAST" | Region=="ASIA (EX. NEAR EAST)", "Asia", "Oceania"))))) %>%
  select(Country, Continent, everything())

ggplot(countries_world, aes(x=Continent, y=Population, fill=Region))+
  geom_bar(width=0.7, stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=7))+
  theme(legend.text=element_text(size=6))+
  theme_bw()+
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20), axis.text=element_text(size=13),
        legend.text=element_text(size=13), legend.title=element_text(size=14))

partial <- total_population %>%
  select(Country, Continent, Region, `1960`,`2017`) %>%
  na.omit() #%>%
  #summarize(sum(`1960`))
  
png(filename="/home/nicole/Data Science/exam_big_data/Images/continent_distribution.png",width=650,height=400)
ggplot(partial, aes(x=Continent, y=2017, fill=Region))+
  geom_bar(width=0.7, stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=7))+
  theme(legend.text=element_text(size=6))+
  theme_bw()+ylab("count")+
  ggtitle("2017 Population distribution")+
  scale_fill_brewer(palette="Paired")+
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20), axis.text=element_text(size=13),
        legend.text=element_text(size=13), legend.title=element_text(size=14))
dev.off()

states <- geojsonio::geojson_read("prova.geo.json", what = "sp")
partial <- transform(partial, Regnum = as.numeric(Region))

colnames(partial)[1] <- "name"
population <- partial
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
partial <- population
prova <- merge(states, partial, by="name")
m <- leaflet(prova) %>%
  setView(-1, 42, zoom=1) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

#bins <- partial$Regnum
#bins
colourCount = length(unique(partial$Regnum))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
pal2 <- getPalette(colourCount)
#pal <- colorBin(pal2, domain = states$density, bins = bins)#domain = 0:11, bins=bins)
pal <- colorBin("Paired", domain = states$density, bins = colourCount)

labels <- sprintf(
  "<strong>%s</strong><br/> %s",
  prova$name, prova$Region
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = ~pal(Regnum),
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

saveWidget(m, 'map_Regions.html', selfcontained = TRUE)
#####################################################
####### WORLD PLOT TOTAL POPULATION ANALYSIS ########
####### PERCENTAGE OF FOR EVERY YEAR:
worldpop <- total_population %>%
  filter(Country=="World")

growth <- gather(worldpop, "year", "WorldPopulation", 5:ncol(worldpop)) %>%
  select(-Continent, -Region, -Country.Code) %>%
  filter(year != "X1960") %>%
  mutate(percentage_growth = (WorldPopulation-lag(WorldPopulation))/lag(WorldPopulation)*100) %>%
  select(-Country)

#png(filename="/home/nicole/Data Science/exam_big_data/pop_number2.png",width=550,height=400)
ggplot(growth, aes(x=year, y=WorldPopulation, color=WorldPopulation)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(breaks=seq(1960, 2017, 5))+
  theme_minimal()+
  geom_point()+
  theme(legend.position="none")+
  labs(x = "Year", y="Population", title="Total World Population")+
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20), axis.text=element_text(size=12))
#dev.off()

#png(filename="/home/nicole/Data Science/exam_big_data/pop_growth.png",width=550,height=400)
ggplot(growth, aes(x=year, y=percentage_growth, color=percentage_growth)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(breaks=seq(1960, 2017, 5))+
  theme_minimal()+
  geom_point()+
  theme(legend.position="none")+
  labs(x = "Year", y="Growth Percentage", title="Growth Percentage of the World Population")+
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20), axis.text=element_text(size=12))
#dev.off()



# DEATH RATE AND IMMUNIZATION IN AFRICA
set.seed(12)
n_africa <- levels(immunization$Region)[7]
imm <- immunization %>% 
  filter(Region==n_africa)

d <- gather(imm, "year", "counts", 4:ncol(imm)) 
ggplot(d, aes(x=year, y=counts))+
  geom_line(aes(group = Country, color=Country))

dea <- death %>% 
  filter(Region==n_africa)
d <- gather(dea, "year", "counts", 4:ncol(dea)) 
ggplot(d, aes(x=year, y=counts))+
  geom_line(aes(group = Country, color=Country))

death %>% # hanno piu` morti
  select(c("Region", "Country", "X2016")) %>%
  arrange(desc(X2016))%>%
  head(n=3)

death %>% # hanno meno morti
  select(c("Region", "Country", "X2016")) %>%
  arrange(X2016)%>%
  head(n=3)

countries_world %>%
  select(c("Region", "Country", "Deathrate", "Net.migration")) %>%
  arrange(Deathrate)%>%
  head(n=3)

countries_world %>%
  select(c("Region", "Country", "Deathrate", "Net.migration")) %>%
  arrange(desc(Deathrate))%>%
  head(n=3)

countries_world %>%
  select(max(Net.migration))

#==================


# ================ LINEAR MODEL AND OUTLIERS AREA/POPULATION
# An interesting thing to do is to use the function "influencePlot" 
# of the "car" package to obtain the graph where on the x axis there
# are the estimated values, on the y axis the studentized residuals, 
# and the bubbles represent the Cooks distances.
mod <- lm(log2(Population)~log2(Area), data=countries_world)
rownames(countries_world) <- countries_world$Country
outs <- car::influencePlot(mod)
n <- 3
Cooksdist <- row.names(outs[order(outs$CookD), ])
print(Cooksdist)

pred_val <- predict(mod)
plot(log2(countries_world$Population) - pred_val)

# ================ 


### WORLD PLOT density population
DensDF <- data.frame(country = countries_world$Country.Code, Density = countries_world$StepDensity)
DensMap <- joinCountryData2Map(DensDF, joinCode = "ISO3",nameJoinColumn = "country")

mapCountryData(DensMap, nameColumnToPlot="Density", catMethod = "categorical",
               missingCountryCol = gray(.8))

###########################################
########### OTHER VARIOUS TRIALS ###########

# MORLD MAP IN YEARS 1960, 2017
library(rworldmap)
library(RColorBrewer)

totpopDF <- data.frame(country = total_population$Country.Code, Population1960 = total_population$`1960`)

totpopMap <- joinCountryData2Map(totpopDF, joinCode = "ISO3",nameJoinColumn = "country")

colourPale <- brewer.pal(6, 'YlOrBr')
mapCountryData(totpopMap, nameColumnToPlot="Population1960", catMethod = "logFixedWidth", 
               missingCountryCol = gray(.8), colourPalette=colourPale)

mapCountryData(totpopMap, mapRegion='africa', nameColumnToPlot="Population1960", catMethod = "logFixedWidth",
               missingCountryCol = gray(.8), numCats=10, colourPalette = "terrain")

###### 2017
# totpopDF2017 <- data.frame(country = total_population$Country.Code, Population2017 = total_population$X2017)
# totpopMap2017 <- joinCountryData2Map(totpopDF2017, joinCode = "ISO3",nameJoinColumn = "country")
# 
# mapCountryData(totpopMap2017, nameColumnToPlot="Population2017", catMethod = "logFixedWidth",
#                missingCountryCol = gray(.8), numCats=10, colourPalette = "terrain")
# 
# mapCountryData(totpopMap2017, mapRegion='africa', nameColumnToPlot="Population2017", catMethod = "logFixedWidth",
#                missingCountryCol = gray(.8), numCats=10, colourPalette = "terrain")
# 
