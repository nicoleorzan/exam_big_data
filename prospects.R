{
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(splines)
}

setwd('/home/nicole/Data Science/exam_big_data')
pop <- read.csv("Datasets/total_population.csv", skip=4)
w <- pop %>%
  filter(Country.Name=="World")%>%
  select(-Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code, -X)
colnames(w) <- c(substring(colnames(w[,1:length(w)]), 2))
colnames(w)
w[2,]<- colnames(w)
w2 <- data.frame(t(w))
colnames(w2) <- c("world_pop", "year")

w2$world_pop <- as.numeric(as.character(w2$world_pop))
w2$year <- as.numeric(as.character(w2$year))


mod <- lm(world_pop ~ year, w2)
year <- seq(2018,2050)
year <- data.frame(year)
out <- predict(mod, year)
year$newcol <- data.frame(out)


## LOGIT!! :)

#model <- glm(world_pop ~ year, w2, family=binomial(link = "logit"))
#summary(model)

model_pois <- glm(world_pop ~ year, w2, family=gaussian)
summary(model_pois)
out <- predict(model_pois, year)
year$gaus <- data.frame(out)
