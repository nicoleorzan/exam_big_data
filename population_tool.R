library(shiny)
library(dplyr)
library(DT)

population <- read.csv("Clean/population_clean.csv")
colnames(population) <- c("N","Country", "Continent", "Region", "Country.Code", substring(colnames(population[,7:length(population)-1]), 2), "Area")
n_total <- nrow(total_population)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Text instructions - update reference to hardcoded sample size here
      HTML(paste("Enter a value between 1 and", n_total)),
      
      # Numeric input for sample size - define min and max
      numericInput(inputId = "n",
                   label = "Sample size:",
                   value = 30,
                   min = 1, max = n_total,
                   step = 1)
      
    ),
    
    # Output: Show data table
    mainPanel(
      DT::dataTableOutput(outputId = "worldtable")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create data table
  output$worldtable <- DT::renderDataTable({
    population_sample <- population %>%
      sample_n(input$n) %>%
      select(Country:Country.Code)
    DT::datatable(data = population, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
}
shinyApp(ui = ui, server = server)

