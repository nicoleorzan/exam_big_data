{
  library(dplyr)
  library(ggplot2)
  library(rworldmap)
  library(countrycode)
  library(tidyr)
  library(stringr)
  library(ggmap)
  library(maptools)
  library(maps)
}

map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))

setwd('/home/nicole/Data Science/exam_big_data/Clean')
# LOAD CLEANED AND MODIFIED DATASETS
{
density <- read.csv("density_clean.csv")
population <- read.csv("population_clean.csv")

pop_per_continent <- read.csv("pop_per_continent.csv")
growth <- read.csv("growth_clean.csv")
immunization <- read.csv("clean_immuniz.csv")
death <- read.csv("clean_death.csv")
}
colnames(immunization) <- c(colnames(immunization)[1:5], substring(colnames(immunization[,7:length(immunization)-1]), 2), "Area")
colnames(death) <- c(colnames(death)[1:5], substring(colnames(death[,7:length(death)-1]), 2), "Area")

#Per region
death_years <- death %>%
  na.omit() %>%
  select(-X, -Country, -Country.Code, -Area) %>%
  group_by(Region) %>%
  mutate_at(vars(`1960`:`2016`), sum) %>%
  unique()

deaths_gather <- gather(death_years, "year", "deaths", 3:ncol(death_years))
ggplot(deaths_gather, aes(x=year, y=log(deaths), color=Region))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(breaks=seq(1960, 2016, 5))

imm_years <- immunization %>%
 # na.omit() %>%
  select(-X, -Country, -Country.Code, -Area) %>%
  group_by(Region) %>%
  mutate_at(vars(`1980`:`2016`), sum) %>%
  unique()

imm_gather <- gather(imm_years, "year", "deaths", 3:ncol(imm_years))
ggplot(imm_gather, aes(x=year, y=log(deaths), color=Region))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(breaks=seq(1960, 2016, 5))
  

#################
death_years <- death %>%
  filter(Country=="World")%>%
  gather("year", "deaths", 7:ncol(death)-1) %>%
  select(-Continent, -Region, -Country.Code, -Area, -X)

immun_years <- immunization %>%
  filter(Country=="World")%>%
  gather("year", "immunization", 7:ncol(immunization)-1) %>%
  select(-Continent, -Region, -Country.Code, -Area, -X)

immun_years$Country <- as.character(immun_years$Country)
death_years$Country <- as.character(death_years$Country)

immu_and_death <- inner_join(death_years, immun_years, by=c("Country","year"))

immu_and_death %>%
  gather(key, value, deaths, immunization) %>%
  ggplot(aes(x=year, y=value, colour=key)) +
  geom_point()

ggplot(immun_years, aes(x=year, y=immunization))+geom_point()
ggplot(death_years, aes(x=year, y=deaths))+geom_point()

