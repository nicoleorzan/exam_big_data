library(shiny)
library(ggplot2)
library(tools)
library(dplyr)

density <- read.csv("Clean/density_clean.csv")
population <- read.csv("Clean/population_clean.csv")
pop_per_continent <- read.csv("Clean/pop_per_continent.csv")
growth <- read.csv("Clean/growth_clean.csv")


# Define UI for application that plots features of movies
ui <- fluidPage(
  titlePanel("World Population Study"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      # selectInput(inputId = "x", label = "X-axis:",
      #             choices = c("year"),
      #             selected = "year"),
      # selectInput(inputId = "y", label = "Y-axis:",
      #             choices = c("WorldPopulation","percentage_growth"), 
      #             selected = "WorldPopulation"),
      # selectInput(inputId = "z",label = "Color by:",
      #             choices = c("percentage_growth"),
      #             selected = "percentage_growth"),
      # 
                  # Select variable for x-axis
      selectInput(inputId = "x2", label = "x-axis: Population",
                  choices = c("1960" = "X1960",
                              "1970" = "X1970",
                              "1980" = "X1980",
                              "1990" = "X1990",
                              "2000" = "X2000",
                              "2010" = "X2010",
                              "2017" = "X2017"),
                  selected = "1960"),
      selectInput(inputId = "y2", label = "Y-axis: Area",
                  choices = c("Area", "logArea"), 
                  selected = "Area"),
      selectInput(inputId = "z2",label = "Color by:",
                  choices = c("Continent", "Region"),
                  selected = "Continent"),
      textInput(inputId = "plot_title", 
                label = "Plot title", 
                placeholder = "Enter text for plot title"),
      checkboxGroupInput(inputId = "selected_continent",
                         label = "Select Continent:",
                         choices = c("Europe", "Africa", "Asia", "Oceania", "America"),
                         selected = c("Europe", "Africa", "Asia", "Oceania", "America"))
      
    ),
    
    # Outputs
    mainPanel(
      #plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "scatterplot2"),
      textOutput(outputId = "description")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create a subset of data filtering for selected title types
  population_subset <- reactive({
    req(input$selected_continent)
    filter(population, Continent %in% input$selected_continent)
  })
  # Convert plot_title toTitleCase
  pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
  
  # Create the scatterplot object the plotOutput function is expecting
  # output$scatterplot <- renderPlot({
  #   ggplot(data = growth, aes_string(x = input$x, y = input$y, color = input$z)) +  geom_point()
  # })
  output$scatterplot2 <- renderPlot({
    ggplot(population_subset(), aes_string(x = input$x2, y = input$y2, color = input$z2)) + geom_point()
  })
  # Create descriptive text
  output$description <- renderText({
    paste0("The plot above titled '", pretty_plot_title(), "' visualizes the relationship between ", 
           input$x2, " and ", input$y2, ", conditional on ", input$z2, ".")
  })
}


# Create a Shiny app object
shinyApp(ui = ui, server = server)

