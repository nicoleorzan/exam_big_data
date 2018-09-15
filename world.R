{
  library(dplyr)
  library(ggplot2)
  library(rworldmap)
  library(countrycode)
  library(tidyr)
  library(stringr)
  library(shiny)
  #library(plotly)
}

setwd('/home/nicole/Data Science/exam_big_data')
#### LOAD DATASETS ###
{countries_world <- read.csv("Datasets/countries of the world.csv", na.strings=c("","NA"))
colnames(countries_world)[4]="Area"
colnames(countries_world)[5]="Density"
countries_world[,2] <- str_trim(countries_world[,2], "both") 
#MPI_national <- read.csv("MPI_national.csv")
#MPI_subnational <- read.csv("MPI_subnational.csv")
immunization <- read.csv("Datasets/immunization.csv", skip=4)
death <- read.csv("Datasets/death_rate.csv", skip=4)
Gdp <- read.csv("Datasets/GDP_annual_growth.csv", skip=4)
total_population <- read.csv("Datasets/total_population.csv", skip=4)
}

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
                            Region=countries_world$Region)
country_region$Country.Code <- as.character(country_region$Country.Code)

#######==== Dropping columns and rows with ONLY ======########
#######==== NA's, adding Region and Continent ======########

na_reg <- function(x, country_region){
  
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

{datas <- vector(mode="list", length=4)
names(datas) <- c("immunization", "death", "Gdp", "total_population")
datas[[1]] <- immunization; datas[[2]] <- death
datas[[3]] <- Gdp; datas[[4]] <- total_population

for (i in (1:length(datas))){
  print(i);  print(names(datas)[i])
  tmp_data <- mget(names(datas)[i])
  sapply(tmp_data, na_reg, country_region=country_region)
  datas[[i]] <- x
}
immunization <- datas[[1]]; death <- datas[[2]]
Gdp <- datas[[3]]; total_population <- datas[[4]]}

# cleaning additive data
total_population <-total_population %>%
  mutate(Continent=replace(Continent, Country=="Kosovo", "Europe")) %>%
  mutate(Continent=replace(Continent, Country=="Virgin Islands (U.S.)", "America"))%>%
  mutate(Continent=replace(Continent, Country=="Vanuatu", "Oceania"))%>%
  mutate(Continent=replace(Continent, Country=="South Sudan", "Africa"))%>%
  mutate(Continent=replace(Continent, Country=="Montenegro", "Europe"))%>%
  #mutate(Continent=replace(Continent, Country=="Small states", "Europe"))%>%
  #mutate(Continent=replace(Continent, Country=="South Asia (IDA & IBRD)", "Asia"))%>%
  #mutate(Continent=replace(Continent, Country=="Sub-Saharan Africa (IDA & IBRD countries)", "Africa"))%>%
  #mutate(Continent=replace(Continent, Country=="Latin America & the Caribbean (IDA & IBRD countries)", "America"))%>%
  #mutate(Continent=replace(Continent, Country=="East Asia & Pacific (IDA & IBRD countries)", "Asia"))%>%
  mutate(Continent=replace(Continent, Country=="Sint Maarten (Dutch part)", "America"))%>%
  #mutate(Continent=replace(Continent, Country=="Sub-Saharan Africa (excluding high income)", "Africa"))%>%
  #mutate(Continent=replace(Continent, Country=="South Asia", "Asia"))%>%
  #mutate(Continent=replace(Continent, Country=="South Sudan", "Africa"))%>%
  #mutate(Continent=replace(Continent, Country=="Sub-Saharan Africa", "Africa"))%>%
  mutate(Continent=replace(Continent, Country=="Pacific island small states", "Asia"))%>%
  #mutate(Continent=replace(Continent, Country=="North America", "America"))%>%
  mutate(Continent=replace(Continent, Country=="St. Martin (French part)", "America"))%>%
  #mutate(Continent=replace(Continent, Country=="Latin America & Caribbean", "America")) %>%
  #mutate(Continent=replace(Continent, Country=="Latin America & Caribbean (excluding high income)", "America"))%>%
  #mutate(Continent=replace(Continent, Country=="European Union", "Europe"))%>%
  mutate(Continent=replace(Continent, Country=="Caribbean small states", "America"))%>%
  #mutate(Continent=replace(Continent, Country=="Euro area", "Europe"))%>%
  mutate(Continent=replace(Continent, Country=="Curacao", "America"))%>%
  mutate(Continent=replace(Continent, Country=="Channel Islands", "America"))
#mutate(Continent=replace(Continent, Country=="Central Europe and the Baltics", "Europe"))


total_population %>%
  select(Country, Continent, `1960`) %>%
  na.omit()  %>%
  summarize(sum(`1960`))

na_pop <- total_population %>%
  select(Country, Continent, `1960`) %>%
  filter(is.na(Continent))#%>%
  filter(Country!="World")%>%
  summarize(sum(`1960`, na.rm=TRUE))

#totpopDF <- data.frame(Country = total_population$Country.Code, Population1960 = total_population$`1960`)
#totpopDF$Country <- as.character(totpopDF$Country)

#left_join(countries_world, totpopDF, by=c(countries_world$Country.Code))



countries_world <- countries_world %>% 
  mutate(Continent = ifelse(Region=="NORTHERN AMERICA" | Region=="LATIN AMER. & CARIB", "America", ifelse(Region=="NORTHERN AFRICA" | Region=="SUB-SAHARAN AFRICA", "Africa", ifelse(Region=="BALTICS" | Region=="WESTERN EUROPE" | Region=="EASTERN EUROPE" | Region=="C.W. OF IND. STATES", "Europe", ifelse(Region=="NEAR EAST" | Region=="ASIA (EX. NEAR EAST)", "Asia", "Oceania"))))) %>%
  select(Country, Continent, everything())

countries_world %>%
  group_by(Region) %>%
  count() %>%
  arrange(n)

countries_world %>%
  group_by(Continent) %>%
  count() %>%
  arrange(n)

countries_world %>%
  select(Country, Continent) %>%
  filter(Continent=="Oceania")


# STEPPING DENSITY VALUES OF COUNTRIES WORLD
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



##################  SOME PLOTS #####################
ggplot(data = countries_world) +
  geom_bar(mapping = aes(x = StepDensity))

ggplot(countries_world, mapping = aes(x=Population, y=Area))+
  geom_point(mapping=aes(color=StepDensity))

ggplot(countries_world, mapping=aes(x=log2(Population), y=log2(Area)))+
  geom_point(mapping=aes(color=StepDensity))

ggplot(countries_world, aes(x=Region, y=Population, fill=Region))+
  geom_bar(width=0.7, stat="identity", color="red4")+#fill="sienna2")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  theme(axis.text=element_text(size=7))+
  guides(fill=FALSE)

ggplot(countries_world, aes(x=Region))+
  geom_bar(color="chocolate4", fill="lightblue2")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  theme(axis.text=element_text(size=7))

ggplot(countries_world[1:15,], aes(x=Country, y=Population))+
  geom_bar(width=0.7, stat="identity", color="red4", fill="sienna2")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  theme(axis.text=element_text(size=7))

countries_world %>%
  select(Population) %>%
  summarize(sum(as.numeric(Population)))
  

#png(filename="/home/nicole/Data Science/Exam_data_analysis/continent_distribution.png",width=650,height=400)
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
  summarize(sum(`1960`))
  
ggplot(partial, aes(x=Continent, y=1960))+
  geom_bar(width=0.7, stat="identity")
         
png(filename="/home/nicole/Data Science/exam_big_data/Images/continent_distribution.png",width=650,height=400)
ggplot(partial, aes(x=Continent, y=2017, fill=Region))+
  geom_bar(width=0.7, stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=7))+
  theme(legend.text=element_text(size=6))+
  theme_bw()+
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20), axis.text=element_text(size=13),
        legend.text=element_text(size=13), legend.title=element_text(size=14))
dev.off()

png(filename="/home/nicole/Data Science/exam_big_data/Images/continent_distribution_old.png",width=650,height=400)
ggplot(partial, aes(x=Continent, y=1960, fill=Region))+
  geom_bar(width=0.7, stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=7))+
  theme(legend.text=element_text(size=6))+
  theme_bw()+
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20), axis.text=element_text(size=13),
        legend.text=element_text(size=13), legend.title=element_text(size=14))
dev.off()
# Let's see how much population on m^2
# Total area per continent:
areas <-countries_world %>%
  group_by(Continent) %>%
  mutate(Continent_Area_km2 = sum(Area)*2.589988,Continent_Pop = sum(as.numeric(Population))) %>%
  mutate(Continent_Density_ppl_on_km2 = Continent_Pop/Continent_Area_km2) %>%
  select(Continent_Area_km2, Continent_Pop, Continent_Density_ppl_on_km2 , everything())%>%
  distinct(Continent_Area_km2, Continent_Pop, Continent_Density_ppl_on_km2) %>%
arrange(desc(Continent_Area_km2))

##############################################3

### Immunization over years =======================
set.seed(12)
# sampling taking an obsevation for each region
immsample <- immunization %>% 
  group_by(Region) %>% 
  sample_n(size = 1, replace=FALSE)
View(immsample)

prova <- gather(immsample, "year", "counts", 5:ncol(immsample)) #:D :D :D
ggplot(prova, aes(x=year, y=counts))+
  geom_line(aes(group = Country, color=Country))

#### Per continent ===================
immunization_time_series <- gather(immunization, "year", "counts", 5:ncol(immunization))

immunization_time_series <- immunization_time_series %>%
  group_by(Continent, year) %>%
  arrange(year) %>%
  mutate(mean_per_year_per_continent = mean(counts, na.rm = T))

immunization_prv_sample <- immunization_time_series %>% 
  group_by(Continent, year) %>% 
  sample_n(size = 1, replace=FALSE)

ggplot(immunization_prv_sample, aes(x=year, y=mean_per_year_per_continent))+
  geom_line(aes(group = Continent, color=Continent))
#### End Per continent ===================


### Death rate over years =======================
#death$Country <- as.character(death$Country)
deathsample <- death %>%
  filter(Country %in% immsample$Country)

#proviamo <- filter(death, Country.Name %in% as.character(immsample$Country.Name))
d <- gather(deathsample, "year", "counts", 5:ncol(deathsample)) 
ggplot(d, aes(x=year, y=counts))+
  geom_line(aes(group = Country, color=Country))

#### Per continent ===================
death_time_series <- gather(death, "year", "counts", 5:ncol(death))

death_time_series <- death_time_series %>%
  group_by(Continent, year) %>%
  arrange(year) %>%
  mutate(mean_per_year_per_continent = mean(counts, na.rm = T))

death_prv_sample <- death_time_series %>% 
  group_by(Continent, year) %>% 
  sample_n(size = 1, replace=FALSE)

ggplot(death_prv_sample, aes(x=year, y=mean_per_year_per_continent))+
  geom_line(aes(group = Continent, color=Continent))
#### End Per continent ===================


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


### WORLD PLOT GDP index
GDP_DF <- data.frame(country = countries_world$Country.Code, GDP = countries_world$GDP....per.capita.)
GDPMap <- joinCountryData2Map(GDP_DF, joinCode = "ISO3",nameJoinColumn = "country")

mapCountryData(GDPMap, nameColumnToPlot="GDP", catMethod = "fixedWidth",
               missingCountryCol = gray(.8), numCats=10, colourPalette = "terrain")




countries_world %>%
filter(Birthrate==0)
#group_by(Region) %>%
#summarise(mean_birth_rate = mean(Birthrate))

# poverty mpi index natrional
poverDF <- data.frame(country = MPI_national$ISO, poverty = MPI_national$MPI.Urban)
PoverMap <- joinCountryData2Map(poverDF, joinCode = "ISO3",nameJoinColumn = "country")
View(PoverMap)
mapCountryData(PoverMap, nameColumnToPlot="poverty", catMethod = "categorical",
               missingCountryCol = gray(.8))
library(rgl)
attach(countries_world)
plot3d(Density, Area, Population, col="red", size=3) 
detach(countries_world)

#####################################################
####### WORLD PLOT TOTAL POPULATION ANALYSIS ########
####### PERCENTAGE OF GROWTH FOR EVERY YEAR:
worldpop <- total_population %>%
  filter(Country=="World")

growth <- gather(worldpop, "year", "WorldPopulation", 5:ncol(worldpop)) %>%
  select(-Continent, -Region, -Country.Code) %>%
  filter(year != "X1960") %>%
  mutate(percentage_growth = (WorldPopulation-lag(WorldPopulation))/lag(WorldPopulation)*100) %>%
  select(-Country)



ggplot(growth, aes(x=year, y=percentage_growth))+geom_point()

png(filename="/home/nicole/Data Science/Exam_data_analysis/pop_growth.png",width=550,height=400)
ggplot(growth, aes(x=year, y=percentage_growth, color=percentage_growth)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(breaks=seq(1960, 2017, 5))+
  geom_point()+
  theme(legend.position="none")+
  labs(x = "Year", y="Growth Percentage", title="Growth Percentage of the World Population")+
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20), axis.text=element_text(size=12))
dev.off()

png(filename="/home/nicole/Data Science/Exam_data_analysis/pop_number.png",width=550,height=400)
ggplot(growth, aes(x=year, y=WorldPopulation))+geom_point()#, color=WorldPopulation)) #+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(breaks=seq(1960, 2017, 5))+
  geom_point()+
  theme(legend.position="none")+
  labs(x = "Year", y="Population", title="Total World Population")+
  theme(plot.title = element_text(size=22), axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20), axis.text=element_text(size=12))
dev.off()



#####################################
#####################################

# MORLD MAP IN YEARS 1960, 2017
totpopDF <- data.frame(country = total_population$Country.Code, Population1960 = total_population$`1960`)

totpopMap <- joinCountryData2Map(totpopDF, joinCode = "ISO3",nameJoinColumn = "country")
library(RColorBrewer)
colourPale <- brewer.pal(6, 'YlOrBr')
mapCountryData(totpopMap, nameColumnToPlot="Population1960", catMethod = "logFixedWidth", 
               missingCountryCol = gray(.8), colourPalette=colourPale)

mapCountryData(totpopMap, mapRegion='africa', nameColumnToPlot="Population1960", catMethod = "logFixedWidth",
               missingCountryCol = gray(.8), numCats=10, colourPalette = "terrain")

###### 2017
totpopDF2017 <- data.frame(country = total_population$Country.Code, Population2017 = total_population$X2017)
totpopMap2017 <- joinCountryData2Map(totpopDF2017, joinCode = "ISO3",nameJoinColumn = "country")

mapCountryData(totpopMap2017, nameColumnToPlot="Population2017", catMethod = "logFixedWidth",
               missingCountryCol = gray(.8), numCats=10, colourPalette = "terrain")

mapCountryData(totpopMap2017, mapRegion='africa', nameColumnToPlot="Population2017", catMethod = "logFixedWidth",
               missingCountryCol = gray(.8), numCats=10, colourPalette = "terrain")

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    plot_ly(z = state.area, text = state.name, locations = state.abb,
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(geo = g)
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view event data" else d
  })
  
}

shinyApp(ui, server)
