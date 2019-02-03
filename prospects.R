{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(readr)
  library(plotly)
}

setwd('/home/nicole/Data Science/exam_big_data')
pop <- read.csv("Datasets/total_population.csv", skip=4, stringsAsFactors = FALSE)
pop2 <- read_csv("Datasets/total_population.csv", skip=4)
fer <- read.csv("Datasets/fertility.csv", skip=4)

{
  clean <- function(ds){
    a <- ds %>%
      #filter(`Country Name`=="World")%>%
      #select(-`Country Name`, -`Country Code`, -`Indicator Name`, -`Indicator Code`, -X63)
      filter(Country.Name=="World")%>%
      select(-Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code, -X)
    
    colnames(a) <- c(substring(colnames(a[,1:length(a)]), 2))
    
    return(a)
  }

#   w <- pop %>%
#   #filter(`Country Name`=="World")%>%
#   #select(-`Country Name`, -`Country Code`, -`Indicator Name`, -`Indicator Code`, -X63)
#   filter(Country.Name=="World")%>%
#   select(-Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code, -X)
# colnames(w) <- c(substring(colnames(w[,1:length(w)]), 2))
# colnames(w)
  w <- clean(pop)
  f <- clean(fer)
  
  w[2,]<- colnames(w)
  w <- data.frame(t(w))
  colnames(w) <- c("world_pop", "year")
  
  w$world_pop <- as.numeric(as.character(w$world_pop))
  w$year <- as.numeric(as.character(w$year))
  w <- w[-59]
  #w$fert <- as.numeric(as.character(f[1,]))

}

newdata <- seq(2018,2100)
newdata <- data.frame(newdata)
colnames(newdata)[1] <- "year"

# linear model
mod <- lm(world_pop ~ year, w)
lmod <- predict(mod, newdata)
newdata$lm <- data.frame(lmod)
#newdata <- newdata %>%
#  select(lm, year)


## Logistic model!! :)

mod <- nls(world_pop ~ SSlogis(year, phi1, phi2, phi3), data = w)
#summary(mod)
pred <- predict(mod, newdata)
#pred[1:83]
vv <- data.frame(pred)
newdata$world_pop <- data.frame(pred[1:83])
newdata <- newdata %>%
  select(world_pop, year, lm)
newdata$world_pop <- as.numeric(unlist(newdata$world_pop))
newdata$lm <- as.numeric(unlist(newdata$lm))
w$lm <- w$world_pop
tot <- rbind(w, newdata)

p1 <- plot_ly(tot, x = ~year, y = ~world_pop, name = 'Logistic model', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~lm, name = 'Lineaar model', mode = 'lines+markers') %>%
  layout(title = "Predicted trend of world population growth until 2100",
         xaxis = list(title = "Year"),
         yaxis = list (title = paste("Pop quantity")))

p1
#===============================================================
#===============================================================
pop <- read.csv("Datasets/total_population.csv", skip=4)
{
  w <- pop %>%
    filter(Country.Name=="World")%>%
    select(-Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code, -X)
  colnames(w) <- c(substring(colnames(w[,1:length(w)]), 2))
  colnames(w)
  w[2,]<- colnames(w)
  w <- data.frame(t(w))
  colnames(w) <- c("world_pop", "year")
  
  w$world_pop <- as.numeric(as.character(w$world_pop))
  w$year <- as.numeric(as.character(w$year))
}

newdata <- seq(2018,2100)
newdata <- data.frame(newdata)
colnames(newdata)[1] <- "year"

# linear model
mod <- lm(world_pop ~ year, w)
lmod <- predict(mod, newdata)
newdata$lm <- data.frame(lmod)
#newdata <- newdata %>%
#  select(lm, year)


## LOGIT!! :)

mod <- nls(world_pop ~ SSlogis(year, phi1, phi2, phi3), data = w)
pred <- predict(mod, newdata)
#vv <- data.frame(pred)
newdata$world_pop <- data.frame(pred[1:83])
newdata <- newdata %>%
  select(world_pop, year, -lm)
newdata$world_pop <- as.numeric(unlist(newdata$world_pop))
tot <- rbind(w, newdata)

p1 <- plot_ly(tot, x = ~year, y = ~world_pop, name = 'popolation', 
              type = 'scatter', mode = 'lines+markers', 
              marker = list(
                color = 'rgb(17, 157, 255)',
                size = 10,
                opacity=0.5,
                line = list(
                  color = 'rgb(231, 99, 250)',
                  width = 2
                )
              ),width = 900, height = 350) %>%
  layout(title = "Predicted trend of world population growth until 2100",
         xaxis = list(title = "Year"),
         yaxis = list (title = paste("Population (Billions)")))
p1
chart_link = api_create(p1, filename="prosp", sharing="public")
chart_link

api_download_plot()
