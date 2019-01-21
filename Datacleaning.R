{
  library(dplyr)
  library(ggplot2)
  library(countrycode)
  library(tidyr)
  library(stringr)
  library(ggraph)
}

setwd('/home/nicole/Data Science/exam_big_data')
#### LOAD DATASETS ###
{countries_world <- read.csv("Datasets/countries of the world.csv", na.strings=c("","NA"))
  colnames(countries_world)[4]="Area"
  colnames(countries_world)[5]="Density"
  countries_world[,2] <- str_trim(countries_world[,2], "both") 
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

{datas <- vector(mode="list", length=4)
  names(datas) <- c("immunization", "death", "Gdp", "total_population")
  datas[[1]] <- immunization; datas[[2]] <- death
  datas[[3]] <- Gdp; datas[[4]] <- total_population
  
  for (i in (1:length(datas))){
    print(i);  print(names(datas)[i])
    tmp_data <- mget(names(datas)[i])
    sapply(tmp_data, na_region_continent, country_region=country_region)
    datas[[i]] <- x
  }
  immunization <- datas[[1]]; death <- datas[[2]]
  Gdp <- datas[[3]]; total_population <- datas[[4]]}


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


#!=======================================================
# computing correlation between countries about pop growth
v = t(total_population)
colnames(v)=v[1,]
v <- v[-c(1,2,3,4, nrow(v)), ]
v1 <- as.data.frame(v)
indx <- sapply(v1, is.factor)
v1[indx] <- lapply(v1[indx], function(x) as.numeric(as.character(x)))
cor(v1)
s <- data.frame(cor(v1))
is.na(s)
rownames(s) <- colnames(s)
e <-s[which(s$Italy>0.95),]
View(e)
#!=======================================================

# after cleeaning the data of world tot pop are not exactly
# the same if we sum all over the regions, maybe beacuse 
# there is some missing data for some countries, 
# which was measured in alternative ways by world bank data; 
# but it is still ok for our purposes
total_population %>%
  select(Country, Continent, `2017`) %>%
  na.omit()  %>%
  summarize(sum(`2017`))


total_population %>%
  select(Country, Continent, `2017`) %>%
  filter(is.na(Continent)) %>%
  filter(`2017`>10000000 & `2017`<20000000)

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

####################
worldpop <- total_population %>%
  filter(Country=="World")

growth <- gather(worldpop, "year", "WorldPopulation", 5:ncol(worldpop)) %>%
  select(-Continent, -Region, -Country.Code) %>%
  filter(year != "X1960") %>%
  mutate(percentage_growth = (WorldPopulation-lag(WorldPopulation))/lag(WorldPopulation)*100) %>%
  select(-Country)

write.csv(file="Clean/growth_clean.csv", x=growth)
nrow(growth)

total_population_withNAs <- total_population %>%
  mutate(logArea = log(Area))

total_population <- total_population %>%
  na.omit() %>% 
  mutate(logArea = log(Area))

population_density_with_NAS <- total_population_withNAs %>%
  mutate(Density1960=`1960`/Area) %>%
  mutate(Density1970=`1970`/Area) %>%
  mutate(Density1980=`1980`/Area) %>%
  mutate(Density1990=`1990`/Area) %>%
  mutate(Density2000=`2000`/Area) %>%
  mutate(Density2010=`2010`/Area) %>%
  mutate(Density2017=`2017`/Area) %>%
  select(Country, Continent, Region, Country.Code,  Density1960:Density2017, Area, logArea)


population_density <- total_population %>%
  mutate(Density1960=`1960`/Area) %>%
  mutate(Density1970=`1970`/Area) %>%
  mutate(Density1980=`1980`/Area) %>%
  mutate(Density1990=`1990`/Area) %>%
  mutate(Density2000=`2000`/Area) %>%
  mutate(Density2010=`2010`/Area) %>%
  mutate(Density2017=`2017`/Area) %>%
  select(Country, Continent, Region, Country.Code,  Density1960:Density2017, Area, logArea)

nrow(total_population)
nrow(population_density)
write.csv(file="Clean/density_clean.csv", x=population_density)
write.csv(file="Clean/density_clean_with_NAs.csv", x=population_density_with_NAS)
write.csv(file="Clean/population_clean.csv", x=total_population)
write.csv(file="Clean/population_clean_with_NAs.csv", x=total_population_withNAs)
 
{
pop_per_continent <- total_population[2:ncol(total_population)]%>%
  select(Continent, everything(), -Region, -Country.Code, -Area, -logArea)

pop_per_continent %>%
  group_by(Continent)%>%
  na.omit()%>%
  summarize(sum(`1960`))

pop_per_continent <- pop_per_continent %>%
  na.omit() %>%
  group_by(Continent) %>%
  mutate_at(vars(`1960`:`2017`), sum) %>%
  unique()
}


write.csv(file="Clean/pop_per_continent.csv", x=pop_per_continent)
write.csv(file="Clean/clean_immuniz.csv", x=immunization)
write.csv(file="Clean/clean_death.csv", x=death)
