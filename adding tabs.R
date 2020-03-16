# import packages ---------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(dplyr)


# UI  -----------------------------------------------------------------
ui <- shinyUI(
  dashboardPage(title = "Demo app", skin = "red",
    dashboardHeader(title = "DTU dashboard"),
    dashboardSidebar(
      selectInput("variable", "See how the price changes according to:",
                  c("Carat" = "carat", 
                    "Depth" = "depth"))
    ),
    dashboardBody(
      tabsetPanel(
        type = "tab",
        tabPanel("Data", tableOutput("diamonds")),
        tabPanel("Summary", verbatimTextOutput("Summ")),
        tabPanel("Plot",
                 fluidRow(
                   box(plotlyOutput("testPlot")),
                   box(selectInput("variable1", "Highlight points based on:",
                               c("Color of diamond" = "color",
                                 "Clarity of diamond" = "clarity")), selectInput("variable2", "Change the plot type:", 
                                                                                 c("Scatter" = "scatter", 
                                                                                   "Columns" = "column"))))
                 )
      )
    )
  )
)


# Server ------------------------------------------------------------------
server <- function(input, output) {
  #1) table display
  output$diamonds <- renderTable(hover = T, bordered = T,{
    head(diamonds[, c("price", input$variable)], 10)
  })
  
  #2) Displaying the summary of the data
  output$Summ <- renderPrint({
    summary(head(diamonds[, c("price", input$variable)], 10))
  })
  
  #3) displaying the output
  output$testPlot <- renderPlotly({
    
    #first i filter the table otherwise plotly slows down a lot
    filtered <- head(diamonds, 200)
    
    #than i can display the plot with the filtered data using the type of plot the user selects
    if (input$variable2 == "scatter") {
      ggplotly(
      ggplot(data = filtered, aes_string(x = input$variable, y = filtered$price, color = input$variable1)) +
        geom_point()
      )
    } else if (input$variable2 == "column") {
      ggplotly(
        ggplot(data = filtered, aes_string(x = input$variable, y = filtered$price, fill = input$variable1)) +
          geom_col()
      )
    }
  })
  
}


# run server --------------------------------------------------------------
shinyApp(ui, server)
