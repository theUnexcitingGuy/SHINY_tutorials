
# Import libraries --------------------------------------------------------
library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)


# Create the app page -----------------------------------------------------
ui <- fluidPage(
    
    # App title
    headerPanel("Test dashboard"),
    h4('CLick below'),
    actionButton("button", "Click"),
    
    
    # Main panel for displaying outputs
    mainPanel(
      
        # Output: Plot of the requested variable against mpg
        plotlyOutput("plot")
        
    )
)


# Define server logic to plot various variables  --------------------------
server <- function(input, output) {
  
    output$plot <- renderPlotly({
      if (input$button) {
        ggplotly(
          ggplot(data = diamonds) +
            geom_bar(mapping = aes(x = diamonds$cut, fill = diamonds$clarity))
        )
      }
    })

}



# run the app command -----------------------------------------------------
shinyApp(ui, server)


