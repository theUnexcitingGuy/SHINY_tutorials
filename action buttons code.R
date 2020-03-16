
# Import libraries --------------------------------------------------------
library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)


# Create the app page -----------------------------------------------------
ui <- fluidPage(
    
    # App title
    headerPanel("Test dashboard"), #maxi header
    h4('CLick below'), #header of level 4
    actionButton("button", "Click"), #simple button with id = 'button' and label 'Click'
    
    
    # Main panel for displaying outputs
    mainPanel(
      
        # Output: Plot of the requested variable against mpg
        plotlyOutput("plot") #id of the output plot 'plot', , since the plot has to be interactive I write plotlyOutput otherwise I would have put plotOutput
        
    )
)


# Define server logic to plot various variables  --------------------------
server <- function(input, output) {
  
    #output$id is the syntax
    output$plot <- renderPlotly({
      if (input$button) {
        #ggplotly is a wrapper of the function ggplot because I want the output plot to be plotly
        ggplotly(
          ggplot(data = diamonds) +
            geom_bar(mapping = aes(x = diamonds$cut, fill = diamonds$clarity))
        )
      }
    })

}



# run the app command -----------------------------------------------------
shinyApp(ui, server)


