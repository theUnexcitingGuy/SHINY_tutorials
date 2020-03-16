# import libraries --------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(dplyr)


# Define UI for application that draws a histogram-------------------------
ui <- shinyUI(
    dashboardPage(title = "Demo app", skin = "red", #colour of the app
                  dashboardHeader(title = "Diamonds vs Cars"), #title of the dashboard that appears on top
                  dashboardSidebar(
                    #questo pezzo mi inserisce sezioni nella sidebar
                      sidebarMenu(
                          menuItem("Diamonds analytics",tabName = "diamonds", icon = icon("gem")), #queste sono le sezioni che compaiono nella sidebar a sinistra
                          menuItem("Car analytics", tabName = "mpg", icon = icon("car")), #icon = icon('id dell'icona presa su fontawsome')
                          menuItem("Bhoooo", tabName = 'ciao', icon = icon('hand-spock')) 
                      )
                  ),
                  dashboardBody(
                      #qua inizia la sezione dove richiamo le macrosezioni che trovo nella sidebar della dashboard
                      tabItems(
                          #1) diamonds tabs
                          tabItem(
                              tabName = "diamonds", #diamonds è l'id della sezione 'Diamonds analytics' vedi sopra
                              tabsetPanel(
                                  type = "tab",
                                  tabPanel("Data", #nome del tab che faccio comparire in cima alla sezione Diamonds Analytics
                                           fluidRow( #fluidrow serve per definire il layout, tutto quello che sta in uno stesso fluidrow vuol dire che sta in una solita riga 
                                             box(selectInput("variable", "Filter columns:", 
                                                                                        c("Carat" = "carat", 
                                                                                          "Cut of the diamond" = "cut",
                                                                                          "Color" = "color",
                                                                                          "Clarity" = "clarity",
                                                                                          "Depth" = "depth"), selectize = TRUE, multiple = TRUE), width = 12),
                                             box(tableOutput("diamonds"), width = 12), #width indica quanto è larga la box, width va da 1 a 12
                                             box(h3("Summary of the data"),verbatimTextOutput("Summ"), width = 12)
                                           )
                                    ),
                                  tabPanel("Plots", 
                                           fluidRow(
                                              box(plotlyOutput("plot1")), 
                                              box(selectInput("variable1", "Select X axis variable", 
                                                                                                c("Cut of the diamond" = "cut", 
                                                                                                  "Carat" = "carat",
                                                                                                  "Depth" = "depth",
                                                                                                  "Price" = "price")), selectInput("variable2", "Select Y axis variable",
                                                                                                                                   c("Cut of the diamond" = "cut",
                                                                                                                                     "Carat" = "carat",
                                                                                                                                     "Depth" = "depth",
                                                                                                                                     "Price" = "price"))),
                                              box(selectInput("variable3", "Change the size based on:", 
                                                              c("NULL", "Carat" = "carat",
                                                                "Price" = "price"), selected = NULL), selectInput("variable4", "Change the colour based on:", 
                                                                                                 c("NULL", "Color of diamond" = "color",
                                                                                                   "Clarity" = "clarity"), selected = NULL))
                                           )
                                    )
                              )
                          ),
                          
                          #2) mpg tabs
                          tabItem(
                              tabName = "mpg",
                              tabsetPanel(
                                  type = "tab",
                                  tabPanel("Overview", 
                                           fluidRow(
                                               box(plotlyOutput("carsplot1")),
                                               box(selectInput("variable5", "Select X axis variable", 
                                                               c("Cylinders" = "cyl",
                                                                 "Cty" = "cty",
                                                                 "Hwy" = "hwy")), selectInput("variable6", "Select Y axis variable",
                                                                                              c("Cylinders" = "cyl",
                                                                                                "Cty" = "cty",
                                                                                                "Hwy" = "hwy"))), 
                                               box(selectInput("variable7", "Select type of plot", 
                                                               c("Scatter" = "scatter", "Box plot" = "box", "Bar plot" = "bar")))
                                           )
                                        ),
                                  tabPanel("Detailed analysis", 
                                           fluidRow(
                                             box(selectInput("variable8", "Facet the plot below based on:",
                                                             c("Class of the car" = "class",
                                                               "transition" = "trans",
                                                               "car manufacturer" = "manufacturer")), selectInput("variable9", "Color the facets based on:", 
                                                                                                                    c("NULL", "Manufacturer" = "manufacturer", 
                                                                                                                        "Year of the car" = "year"), selected = NULL)), 
                                             box(plotlyOutput("carsplot2"))
                                           ),
                                           fluidRow(
                                               box(title = "Facet result", plotlyOutput("facetplot"), width = 12)
                                           )
                                    )
                              )
                          ), 
                          #3) Bhoo section
                          tabItem(
                            tabName = 'ciao', 
                            tabsetPanel(
                                type = 'tab', 
                                tabPanel('Primo tab',
                                         box(title = 'Ciao', solidHeader = T, status = 'danger', selectInput('id11', 'Label1', c('Yes', 'No', 'BHO')))
                                         )
                            )
                          )
                      )
                  )
    )
)

# Define server logic --------------------------------------------------
server <- function(input, output) {
    
    #-------DIAMONDS DASHBOARD--------
    #table display
    output$diamonds <- renderTable(hover = T, bordered = T,{
        head(diamonds[, c("price", input$variable)], 10)
    })
    
    #display summary
    output$Summ <- renderPrint({
        summary(head(diamonds[, c("price", input$variable)], 10))
    })
    
    #display the plot in the second tab
    output$plot1 <- renderPlotly({
        #first i filter otherwise plotly goes slow
        filtered <- head(diamonds, 100)
        
        #now i plot
        ggplotly(
            ggplot(data = filtered, aes_string(x = input$variable1, y = input$variable2, color = input$variable4, size = input$variable3)) +
                geom_point()
        )
    })
    
    #-------CARS DASHBOARD-----------
    output$carsplot1 <- renderPlotly({
        
        #first i filter, same reason of before
        filtered_cars <- head(mpg, 200)
        
        #if conditions based on what kind of plot the user selects
        if (input$variable7 == "scatter") {
            ggplotly(
            ggplot(data = filtered_cars, aes_string(x = input$variable5, y = input$variable6)) +
                geom_point()
            )
        } else if (input$variable7 == "box") {
            ggplotly(
                ggplot(data = filtered_cars, aes_string(x = input$variable5, y = input$variable6)) +
                    geom_boxplot()
            )
        } else if (input$variable7 == "bar") {
            ggplotly(
                ggplot(data = filtered_cars, aes_string(x = input$variable5, y = input$variable6)) +
                    geom_col()
            )
        }
    })
    
    output$carsplot2 <- renderPlotly({
        #filter the data
        filtered <- head(mpg, 200)
        
        #now i plot
        ggplotly(
            ggplot(data = filtered, aes(x = displ, y = hwy)) +
                geom_point()
        )
    })
    
    output$facetplot <- renderPlotly({
        #filter as always
        filtered <- head(mpg, 200)
        
        #facet plot
        ggplotly(
            ggplot(data = filtered, aes(x = displ, y = hwy)) +
                geom_point(aes_string(color = input$variable9)) +
                facet_wrap(as.formula(paste("~", input$variable8)))
        )
    })
}

# Run the application--------------------------------------------------- 
shinyApp(ui = ui, server = server)
