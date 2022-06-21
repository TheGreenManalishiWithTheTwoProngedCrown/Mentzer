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
      box(plotlyOutput("timeseries"), width = 9),
      box(width = 3,
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
        status = "danger"),
      
      materialSwitch(
        inputId = "Id006",
        label = "Normalize", 
        status = "primary",
        right = TRUE
      ),
      materialSwitch(
        inputId = "Id007",
        label = "Moving Average", 
        status = "primary",
        right = TRUE
      )),
      box(textOutput("text"))
      
      
    )
  )
)

server <- function(input, output) {
  
  
test_dataframe <- reactive({
  
  req(input$Id008,input$Id009)
  
  extract_df(input$Id008,input$Id009,input$Id006,input$Id007)})
  

output$text<- renderPrint(input$Id009)



output$text<- renderPrint(input$Id009)
  output$timeseries <- renderPlotly({
    
    plot_ly(test_dataframe(),
            x= ~date,
            y= ~values,
            color = ~entityName,
            type = 'scatter',
            mode = 'lines+markers') %>% 
      layout(legend = list( y = -0.2, orientation = 'h'),
             xaxis = list(title = "Time(UTC)"),
             yaxis = list(title = "#/24s Up (%)")
             )
  })
}

shinyApp(ui, server)
