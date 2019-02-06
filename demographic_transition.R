{
  library(dplyr)
  library(ggplot2)
  library(rworldmap)
  library(countrycode)
  library(tidyr)
  library(stringr)
  library(plotly)
}

#setwd('/home/nicole/Data Science/exam_big_data')
birth_rate <- read.csv("Datasets/birth_rate.csv", skip=4)
death_rate <- read.csv("Datasets/death_rate.csv", skip=4)
tot_pop <- read.csv("Datasets/total_population.csv", skip=4)
tot_pop2 <- read.csv("Datasets/pop_italy_earlyages.csv")
tot_pop2$pop = tot_pop2$pop*1000

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

p1 <- plot_ly(full, x = ~year, y = ~births, name = 'Births', type = 'scatter', mode = 'lines+markers',width = 1200, height = 400) %>%
  add_trace(y = ~deaths, name = 'Deaths', mode = 'lines+markers') %>%
  layout(title = "Demoghraphic Transition",
         xaxis = list(title = "Year"),
         yaxis = list (title = paste("Deaths/Births (per 1000 people) in ",place)))

p2 <- plot_ly(full, x = ~year, y = ~quantity, name = 'Population', type = 'scatter', mode = 'lines+markers',width = 1200, height = 400) %>%
  layout(title = paste("Demoghraphic Transition of",place),
         xaxis = list(title = "Year"),
         yaxis = list (title = paste(place, "'s Total population"))#,
         # shapes = list(list(type = "rect",
         #            fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
         #            x0 = "1980-01-01", x1 = "1985-01-01", xref = "x",
         #            y0 = 4, y1 = 12.5, yref = "y"),
         #       list(type = "rect",
         #         fillcolor = "blue", line = list(color = "blue"), opacity = 0.2,
         #         x0 = "2000-01-01", x1 = "2005-01-01", xref = "x",
         #         y0 = 4, y1 = 12.5, yref = "y"))
  )

ptrans <- subplot(p1, p2, shareX = TRUE, titleX = TRUE, titleY = TRUE, margin = c(0.02,0.02,1,1))
ptrans
#chart_link = api_create(ptrans, filename="dem_trans", sharing="public")
#chart_link

p_pop <- plot_ly(tot_pop2, x = ~year, y = ~pop, name = 'population', type = 'scatter', mode = 'lines+markers',width = 900, height = 350) %>%
  layout(title = "Population amount",
         xaxis = list(title = "Year\n <a href='http://www.populstat.info/Europe/italyc.htm'>Source of the Data </a>"),
         yaxis = list (title = paste("Total population of ",place)),
         margin = c(0.02,0.02,0.02,0.02),
         shapes = list(list(type = "rect",
                            fillcolor = "green", line = list(color = "green"), opacity = 0.5,
                            x0 = "1960", x1 = "2005", xref = "x",
                            y0 = 400, y1 = 60000000, yref = "y")) )

p_pop
#chart_link = api_create(p_pop, filename="tot_pop_green", sharing="public")
#chart_link

#=================================================================
# PGR

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
#png(filename="/home/nicole/Data Science/exam_big_data/Images/pgn.png",width=800,height=800)
ggplot(full2, aes(x=y, y=p, colour=col)) + 
  theme_minimal()+
  geom_bar(stat='identity', width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Divergence from 0 of PGR", 
       title= "Population Growth Rate of Italy") + 
  coord_flip()+
  ylab("Population Growth Rate value")+xlab("Year")+
  theme(legend.position="none")+
  theme(plot.title = element_text(size=30), axis.title.x = element_text(size=24),
        plot.subtitle = element_text(size=24),
        axis.title.y = element_text(size=26), axis.text=element_text(size=20),
        legend.text=element_text(size=13), legend.title=element_text(size=14))
#dev.off()

#=======================================================
## FERTILITY - NOT USED IN REPORT
f <- read.csv("Clean/fertility_clean.csv")
i <- read.csv("Clean/clean_immuniz.csv")
cleaning <- function(f, name){
  f <- f %>%
    filter(Country=="World") %>%
    select(-X, -Area, -Country, -Country.Code, -Region, -Continent)
   
   colnames(f) <- c( substring(colnames(f[,1:length(f)]), 2))
   
   ff <- gather(f, key="year", name, 1:ncol(f))
   ff$year <- as.numeric(ff$year)
   return(ff)
}

fert <- cleaning(f, "fertility")
imm <- cleaning(i, "immuniz")
colnames(imm)[2] <- "immunization"
colnames(fert)[2] <- "fertility"

f1 <- list(
  color = "lightgrey"
)
p4 <- plot_ly(fert, x = ~year, y = ~fertility, name = 'World Ferility Rate', 
              type = 'scatter', mode = 'lines+markers',  
              marker = list(
                color = 'rgb(47,27, 255)',
                size = 20,
                opacity=0.7,
                line = list(
                  color = 'rgb(21, 230, 180)',
                  width = 3
                )
              )) %>%
  layout(title = "World Ferility Rate",
         xaxis = list(title = "Year"),
         yaxis = list (title = paste("PGR of ",place)))
p4
#chart_link = api_create(p4, filename="fertility", sharing="public")
#chart_link


merged <- left_join(fert, imm, by="year")

ggplot(data = merged) +
  geom_line(aes(x=year, y=fertility))+
  geom_line(aes(x=year, y=immunization))

