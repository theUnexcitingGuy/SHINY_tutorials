
# import packages ---------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(dplyr)


# UI  -----------------------------------------------------------------
ui <- shinyUI(
    dashboardPage(
        dashboardHeader(title = "DTU dashboard"),
        dashboardSidebar(),
        dashboardBody(
          #first row
          fluidRow(
            box(title = "Price ~ Depth", selectInput("variable", "Variable:",
                            c("Fair cut" = "Fair",
                              "Good cut" = "Good",
                              "Very good cut" = "Very Good",
                              "Premium cut" = "Premium",
                              "Ideal cut" = "Ideal")), height = 220),
            
            box(title = "Create the graph you want", selectInput("variable1", "Select x axis variable", 
                            c("Depth" = "depth", 
                              "Carat" = "carat", 
                              "Price" = "price")), 
                          selectInput("variable2", "select y axis variable", 
                                      c("Depth" = "depth",
                                        "Carat" = "carat",
                                        "Price" = "price")), height = 220),
          ),
          #second row
          fluidRow(
            box(plotlyOutput("testplot")),
            box(plotOutput("testplot2"))
            )
        )
    )
)



# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  #first plot on the left
  output$testplot <- renderPlotly({
    ggplotly(
      ggplot(data = (diamonds %>% filter(cut == input$variable)), aes(x = depth, y = price, color = clarity)) +
        geom_point()
    )
  })
  
  #second plot on the right
  output$testplot2 <- renderPlot({
      #if you dont put aes_string ggplot wont interpret correctly the input variables
      ggplot(data = diamonds, aes_string(x = input$variable1, y = input$variable2, color = diamonds$color)) +
        geom_point()
  })
}


# run server --------------------------------------------------------------
shinyApp(ui, server)

  

