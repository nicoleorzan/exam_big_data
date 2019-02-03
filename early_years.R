{
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(leaflet)
  library(readODS)
}


early_years <- read.ods("Datasets/world_early_ages.ods")

early_years <- data.frame(early_years)
early_years <- early_years[2:nrow(early_years),]
early_years[,1] <- as.integer(early_years[,1])
early_years[,2] <- as.integer(early_years[,2])
early_years[,3] <- as.integer(early_years[,3])
colnames(early_years) <- c("year","low","high")
early_years <- early_years %>%
  mutate(average = (low+high)/2)

ggplot(data=early_years, aes(x=year))+
  geom_line(aes(y = low, colour="Lower Estimate")) + 
  geom_line(aes(y = high, colour="Upper Estimate")) + 
  xlab("Years")+
  ylab("Population")+
  theme_minimal()+
  ggtitle("Population Estimation of Early Ages (200 D.C - 1800 D.C)")


p <- plot_ly(early_years, x = ~year, y = ~low, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~high, name = 'trace 1', mode = 'lines+markers')
p

p <- plot_ly(early_years, x = ~year, y = ~high, type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(150, 200, 250)'), 
             showlegend = FALSE, name = 'High 2014') %>%
  add_trace(y = ~low, type = 'scatter', mode = 'lines', 
            fill = 'tonexty', fillcolor='rgb(150, 200, 250)', line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Low 2014') %>%
  add_trace(x = ~year, y = ~average, type = 'scatter', mode = 'linesandmarkers', marker=list( size=5 , opacity=0.5),
            line = list(color='rgb(70,150,80)'),marker=list( size=5 , opacity=0.5, color="black"),
            name = 'Average') %>%
  layout(title = "World Population Estimation of Early Ages\n (1 A.D - 1800 A.D)",
         paper_bgcolor='rgb(255,255,255)',
         xaxis = list(title = "Years",
                      #gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE),
         yaxis = list(title = "World Population (in millions)",
                      #gridcolor = 'rgb(255,255,255)',
                      showgrid = TRUE,
                      showline = FALSE,
                      showticklabels = TRUE,
                      tickcolor = 'rgb(127,127,127)',
                      ticks = 'outside',
                      zeroline = FALSE))
p
chart_link = api_create(p, filename="line-mode1", sharing="public")
chart_link

p <- plot_ly(early_years, x = ~A, y = ~B, name = 'Lower Estimate', type = 'scatter', mode = 'lines+markers',width = 600, height = 400) %>%
  add_trace(y = ~C, name = 'Upper Estimate', mode = 'lines+markers') %>%
  layout(title = "World Population Estimation of Early Ages\n (1 A.D - 1800 A.D)",
         xaxis = list(title = "Year"),
         yaxis = list (title = "World Population (in millions)"))
p 
#<img src="https://plot.ly/~Niki23/132.png">

