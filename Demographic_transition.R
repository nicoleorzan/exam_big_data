{
  library(dplyr)
  library(ggplot2)
  library(rworldmap)
  library(countrycode)
  library(tidyr)
  library(stringr)
}

setwd('/home/nicole/Data Science/exam_big_data')
birth_rate <- read.csv("Datasets/birth_rate.csv", skip=4)
death_rate <- read.csv("Datasets/death_rate.csv", skip=4)
tot_pop <- read.csv("Datasets/total_population.csv", skip=4)

dem_trans <- function(place) {
  birth_rate <- birth_rate %>%
    filter(Country.Name==place)
  death_rate <- death_rate %>%
    filter(Country.Name==place)
  tot_pop <- tot_pop %>%
    filter(Country.Name==place)
  
  colnames(birth_rate) <- c("Country", "Country.Code", "Indicator.Name", "Indicator.Code", substring(colnames(birth_rate[,6:length(birth_rate)-1]), 2), "X")
  colnames(death_rate) <- c("Country", "Country.Code", "Indicator.Name", "Indicator.Code", substring(colnames(death_rate[,6:length(death_rate)-1]), 2), "X")
  colnames(tot_pop) <- c("Country", "Country.Code", "Indicator.Name", "Indicator.Code", substring(colnames(tot_pop[,6:length(tot_pop)-1]), 2), "X")
  birth_rate[1:4] <- NULL; death_rate[1:4] <- NULL; tot_pop[1:4] <- NULL

  birth <- gather(birth_rate, key="year", "births", 1:ncol(birth_rate))
  death <- gather(death_rate, key="year", "deaths", 1:ncol(death_rate))
  pop <- gather(tot_pop, key="year", "quantity", 1:ncol(tot_pop))
  birth <- birth[-c(58,59),]
  death <- death[-c(58,59),]
  pop <- pop[-c(58,59),]
  #pop$quantity <- pop$quantity/10000000
  tmp <- left_join(birth, death, by="year")
  return(left_join(tmp, pop, by="year"))
}
place <- "Italy"
full <- dem_trans(place)

p1 <- plot_ly(full, x = ~year, y = ~births, name = 'births', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~deaths, name = 'deaths', mode = 'lines+markers') %>%
  #add_trace(y = ~quantity, name = 'population amount(div 10e7)', mode = 'lines+markers') %>%
  layout(title = "Demoghraphic Transition",
         xaxis = list(title = "Year"),
         yaxis = list (title = paste("Deaths/Births (per 1000 people) in ",place)))

p2 <- plot_ly(full, x = ~year, y = ~quantity, name = 'population', type = 'scatter', mode = 'lines+markers') %>%
  #add_trace(y = ~deaths, name = 'deaths', mode = 'lines+markers') %>%
 # add_trace(y = ~quantity, name = 'population amount(div 10e7)', mode = 'lines+markers') %>%
  layout(title = "Population amount",
         xaxis = list(title = "Year"),
         yaxis = list (title = paste("Total population of ",place)))
p <- subplot(p1, p2)
p
