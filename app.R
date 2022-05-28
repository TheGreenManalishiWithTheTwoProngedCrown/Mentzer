library(shiny)
library(shinydashboard) 
library(plotly)
library(shinyWidgets)
library(tidyr)
source('testing.R')
source("get_entities.R")

ui <- dashboardPage(
  dashboardHeader(title = "Proyecto Mentzer"),
  dashboardSidebar(sidebarMenu(
    menuItem(tabName = "home", text = "Home", icon = icon("home")),
    menuItem(tabName = "another", text = "Another Tab", icon = icon("heart"))
  )),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("timeseries"), width = 10),
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {

url <- "https://api.ioda.inetintel.cc.gatech.edu/v2/signals/raw/region/4501%2C4503%2C4504?from=1651135804&until=1653727804&datasource=ping-slash24"
test_dataframe <- test_func(fetch_data(url))
  
  
  output$timeseries <- renderPlotly({
    
    plot_ly(test_dataframe,
            x= ~date,
            y= ~values,
            color = ~entityName,
            type = 'scatter',
            mode = "line")
  })
}

shinyApp(ui, server)