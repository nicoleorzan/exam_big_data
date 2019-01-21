{
  library(dplyr)
  library(ggplot2)
  library(rworldmap)
  library(countrycode)
  library(tidyr)
  library(stringr)
  library(plotly)
}

setwd('/home/nicole/Data Science/exam_big_data')
birth_rate <- read.csv("Datasets/birth_rate.csv", skip=4)
death_rate <- read.csv("Datasets/death_rate.csv", skip=4)
tot_pop <- read.csv("Datasets/total_population.csv", skip=4)
tot_pop2 <- read.csv("Datasets/pop_italy_earlyages.csv")
#tot_pop2 <- tot_pop2[1:120,]

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

p3 <- plot_ly(tot_pop2, x = ~year, y = ~pop, name = 'population', type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "Population amount",
         xaxis = list(title = "Year"),
         yaxis = list (title = paste("Total population of ",place)))
p3


full$year <- as.numeric(full$year)

full2 <- full %>% 
  select(q =quantity, y=year)%>%
  mutate(p = ((q-lag(q))/(lag(q))))
ggplot(full2, aes(x=y, y=p))+geom_line()
  
p1 <- plot_ly(full2, x = ~y, y = ~p, name = 'PGR', type = 'scatter', mode = 'lines+markers') %>%
layout(title = "Demoghraphic Transition: PGR",
         xaxis = list(title = "Year"),
         yaxis = list (title = paste("PGR of ",place)))
p1
italy <- tot_pop %>%
  filter(Country.Name==place)
colnames(italy) <- c("Country", "Country.Code", "Indicator.Name", "Indicator.Code", substring(colnames(italy[,6:length(italy)-1]), 2), "X")
italy[1:4] <- NULL
it <- gather(italy, key="year", "population", 1:ncol(italy))
it <- it[-c(58,59),]
it <-rbind(it, data.frame(year = c(2017), population=c(60551416)))

# Diverging Barcharts
full2$col = ifelse(full2$p>0, "blue", "green")
png(filename="/home/nicole/Data Science/exam_big_data/Images/pgn.png",width=800,height=800)
ggplot(full2, aes(x=y, y=p, colour=col)) + 
  theme_minimal()+
  geom_bar(stat='identity', width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Divergence from 0 of PGR", 
       title= "Population Growth Rate") + 
  coord_flip()+
  ylab("Population Growth Rate value")+xlab("Year")+
  theme(legend.position="none")+
  theme(plot.title = element_text(size=30), axis.title.x = element_text(size=24),
        plot.subtitle = element_text(size=24),
        axis.title.y = element_text(size=26), axis.text=element_text(size=20),
        legend.text=element_text(size=13), legend.title=element_text(size=14))
dev.off()

