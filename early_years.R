{
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(leaflet)
}


early_years <- read.ods("Datasets/before.ods")

early_years <- data.frame(early_years)
early_years <- early_years[2:nrow(early_years),]
early_years[,1] <- as.integer(early_years[,1])
early_years[,2] <- as.integer(early_years[,2])
early_years[,3] <- as.integer(early_years[,3])

ggplot(data=early_years, aes(x=A))+
  geom_line(aes(y = B, colour="Lower Estimate")) + 
  geom_line(aes(y = C, colour="Upper Estimate")) + 
  xlab("Years")+
  ylab("Population")+
  theme_minimal()+
  ggtitle("Population Estimation of Early Ages (200 D.C - 1800 D.C)")


p <- plot_ly(early_years, x = ~A, y = ~B, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~C, name = 'trace 1', mode = 'lines+markers')
chart_link = api_create(p, filename="line-mode1")
chart_link
