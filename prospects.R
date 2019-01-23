{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(plotly)
  library(splines)
}

setwd('/home/nicole/Data Science/exam_big_data')
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
newdata$world_pop <- data.frame(lmod)
newdata <- newdata %>%
  select(world_pop, year)


## LOGIT!! :)

mod <- nls(world_pop ~ SSlogis(year, phi1, phi2, phi3), data = w)
pred <- predict(mod, newdata)
vv <- data.frame(pred)
newdata$pred_logistic <-data.frame(pred)


p1 <- plot_ly(year, x = ~year, y = ~pred_logistic, name = 'births', type = 'scatter', mode = 'lines+markers') %>%
 layout(title = "Prediction",
         xaxis = list(title = "Year"),
         yaxis = list (title = paste("Pop qunatity ")))
p1
years <- seq(1960,2100)
years$pop <- c(w2$worldpop, year$pred_logistic)
prova <- left_join(w2, year, by="year")
## other models

model_pois <- glm(world_pop ~ year, w2, family=gaussian)
summary(model_pois)
out <- predict(model_pois, year)
year$gaus <- data.frame(out)
