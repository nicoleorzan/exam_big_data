{library(shiny)
library(ggplot2)
library(shinydashboard)
library(tools)
library(DT)
library(dplyr)
library(shinydashboard)
library(stringr)}

setwd('/home/nicole/Data Science/exam_big_data')
density <- read.csv("Clean/density_clean.csv")
population <- read.csv("Clean/population_clean.csv")
pop_per_continent <- read.csv("Clean/pop_per_continent.csv")
growth <- read.csv("Clean/growth_clean.csv")
growth <- growth[-nrow(growth),]

###################################################
###################################################


ui <- dashboardPage(
  dashboardHeader(title = "World population"),
  dashboardSidebar(sidebarMenu(
    menuItem("Population Density", tabName = "dens", icon = icon("arrow-right")),
    menuItem("Population Growth", tabName = "grow", icon = icon("arrow-right")),
    menuItem(h5("Built with",
       img(src = "http://www.worldbank.org/content/dam/wbr/logo/logo-wb-header-en.svg", height = "30px") )
  )
    )),
  dashboardBody(
    tabItems(
    
      # Second tab content
      tabItem(tabName = "grow",
              #h2("Population Growth Analysis"),
              fluidRow(
                box(title = "Population Growth", width=6, #status="primary Blue",
                    background = "navy", solidHeader = TRUE,
                    plotOutput("plot1", height=300)),
                box(
                  title = "Controls", width=4,
                  selectInput(inputId = "x", label = "X-axis:",
                              choices = c("year"),
                              selected = "year"),
                  selectInput(inputId = "y", label = "Y-axis:",
                              choices = c("Total World Population" = "WorldPopulation",
                                          "Percentage of annual growth" = "percentage_growth"),
                              selected = "Percentage of annual growth"),
                  selectInput(inputId = "z",label = "Color by:",
                              choices = c("Percentage of annual growth" = "percentage_growth"),
                              selected = "Percentage of annual growth")
                )# close box
              )# closed fluidrow
      ),
      tabItem(tabName = "dens",
              # Boxes need to be put in a row (or column)
              fluidRow(
                
                box(title = "Amount of population as a function of the years", width=6, #status="primary Blue",
                    background = "navy", solidHeader = TRUE,
                    plotOutput("plot2", height=300)),
                
                box(
                  title = "Controls", width=4, #background = "black",
                  selectInput(inputId = "x2", label = "x-axis: Amount of population in Year",
                              choices = c("1960" = "X1960",
                                          "1970" = "X1970",
                                          "1980" = "X1980",
                                          "1990" = "X1990",
                                          "2000" = "X2000",
                                          "2010" = "X2010",
                                          "2017" = "X2017"),
                              selected = "1960"),
                  selectInput(inputId = "y2", label = "Y-axis: Occupied Area",
                              choices = c("Area", "logArea"), 
                              selected = "Area"),
                  selectInput(inputId = "z2",label = "Color by:",
                              choices = c("Continent", "Region"),
                              selected = "Continent"),
                  checkboxGroupInput(inputId = "selected_continent",
                                     label = "Select Continent:",
                                     choices = c("Europe", "Africa", "Asia", "Oceania", "America"),
                                     selected = c("Europe"))
                )# closed box
              ), # closed fluidrow
              fluidRow(
                box(title = "Pop in the world", width=6, #status="primary Blue",
                    background = "navy", solidHeader = TRUE,
                    plotOutput("plotmap", height=300))
              )
      ) # closed tabitem
    )
  )
)

server <- function(input, output) {
  population_subset <- reactive({
    req(input$selected_continent)
    filter(population, Continent %in% input$selected_continent)
  })
  output$plot1 <- renderPlot({
    ggplot(growth, aes_string(x = input$x, y = input$y, color = input$z)) + 
      geom_point()+scale_x_discrete(breaks=seq(1960, 2017, 5))+
      theme(legend.position="none")
  })
  ###
  Map <- joinCountryData2Map(DF, joinCode = "ISO3",nameJoinColumn = "country")
  output$plotmap <- renderPlot({
  mapCountryData(pMap, nameColumnToPlot="Pop", catMethod = "logFixedWidth", 
                 missingCountryCol = gray(.8), colourPalette=colourPale)
  })
  ###
  output$plot2 <- renderPlot({
    ggplot(population_subset(), aes_string(x = input$x2, y = input$y2, color = input$z2)) + 
      geom_point()
  })
  
}

shinyApp(ui, server)
