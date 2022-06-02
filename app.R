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
      box(plotlyOutput("timeseries"), width = 8),
      box(width = 7,
        title = "Controles",
        pickerInput(
          inputId = "Id008",
          label = "Elegir entidad", 
          choices = entities$name,
          multiple = TRUE,
          selected = "Distrito Capital",
          options =  list("tick-icon" = "glyphicon glyphicon-ok-sign"),
          choicesOpt = list(
            subtext = entities$type

        ))
      ,
       airDatepickerInput(
         inputId = "Id009",
         timepicker = TRUE,
         range = TRUE,
         todayButton = TRUE
       ),
      
      awesomeCheckboxGroup(
        inputId = "Id001",
        label = "Checkboxes with status", 
        choices = c("A", "B", "C"),
        inline = TRUE,
        status = "danger"
      )),
      box(textOutput("text"))
      
      
    )
  )
)

server <- function(input, output) {
  
  
test_dataframe <- reactive({
  
  req(input$Id008,input$Id009)
  
  extract_df(input$Id008,input$Id009,input$Id001)})
  
output$text<- renderPrint(input$Id001)


  output$timeseries <- renderPlotly({
    
    plot_ly(test_dataframe(),
            x= ~date,
            y= ~values,
            color = ~entityName,
            type = 'scatter',
            mode = "line") %>% layout(legend = list(orientation = 'h'))
  })
}

shinyApp(ui, server)