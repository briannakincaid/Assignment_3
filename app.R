#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

data2010 <- read.csv("BP Apprehensions 2010.csv")
data2017 <- read.csv("PB Apprehensions 2017.csv")


# Define UI for application that draws a barplot
ui <- fluidPage(
  
  # Application title
  titlePanel("Border Patrol Apprehensions"),
  
  #Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
     selectInput("year",
                  "Year:",
                  choices = list("2010", "2017")),
      conditionalPanel(condition = "input.year == 2010",
                       selectInput("sector", "Sector:", 
                                   choices = data2010$Sector)),
      conditionalPanel(condition = "input.year == 2017",
                      selectInput("sector", "Sector:", 
                                  choices = data2017$Sector))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw barplots and time series graph.
server <- function(input, output) {
  
  output$distPlot <- renderPlot(
    if (input$year == "2010") {
      barplot(height = as.matrix(data2010[data2010$Sector == input$sector, 2:13]), 
              main = input$sector,
              ylab = "Number of Apprehensions",
              xlab = "Month") 
    }
    else {
      barplot(height = as.matrix(data2017[data2017$Sector == input$sector, 2:13]), 
              main = input$sector,
              ylab = "Number of Apprehensions",
              xlab = "Month") 
    }
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
