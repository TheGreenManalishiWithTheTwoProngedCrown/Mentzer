library(shiny)
library(shinydashboard) 
library(plotly)
library(shinyWidgets)
library(tidyr)
source('testing.R')
source("get_entities.R")
source("get_outages.R")

ui <- dashboardPage(
  dashboardHeader(title = "Proyecto Mentzer"),
  dashboardSidebar(width = 300, sidebarMenu(
    
    pickerInput(
      inputId = "Id008",
      label = "Elegir Región", 
      choices = regions$name,
      multiple = TRUE,
      options =  list("tick-icon" = "glyphicon glyphicon-ok-sign",
                      `actions-box` = TRUE,
                      `live-Search` = TRUE,
                      liveSearchStyle = "contains"),
    ),
    pickerInput(
      inputId = "Id010",
      label = "Elegir ISP", 
      choices = isp$name,
      multiple = TRUE,
      options =  list("tick-icon" = "glyphicon glyphicon-ok-sign",
                      `actions-box` = TRUE,
                      `live-Search` = TRUE)
    ),
    
    materialSwitch(
      inputId = "Id011",
      label = "Venezuela (Nacional)", 
      status = "primary",
      right = TRUE
    ),
    
    awesomeCheckboxGroup(
      inputId = "Id001",
      label = "Data Types", 
      choices = c("Active Probing", "BGP", "Telescope"),
      inline = TRUE,
      status = "danger"
    ),
    
    airDatepickerInput(
      inputId = "Id009",
      timepicker = TRUE,
      range = TRUE,
      todayButton = TRUE,
      label = "Fecha",
      clearButton = TRUE
    ),
    materialSwitch(
      inputId = "Id006",
      label = "Moving Average", 
      status = "primary",
      right = TRUE
    ),
    materialSwitch(
      inputId = "Id007",
      label = "Normalize", 
      status = "primary",
      right = TRUE
    )
  )
),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("timeseries"), width = 12, height = 580),
      box(plotlyOutput("mapa"), width = 12, height = 580),
      box(textOutput("text"))
    )
  )
)

server <- function(input, output) {
  
  
test_dataframe <- reactive({
  
  req((isTruthy(input$Id008)|| isTruthy(input$Id010) || isTruthy(input$Id011))
      ,length(input$Id009) == 2)
  
  extract_df(region_input=input$Id008,
             date_list = input$Id009,
             normalize_bool= input$Id007,
             moving_average = input$Id006,
             isp_req = input$Id010,
             vnzla = input$Id011)})


normalize_label <- reactive({
  input$Id007
})

output$text<- renderPrint(input$Id009)


  output$timeseries <- renderPlotly({
    
    p <- plot_ly(test_dataframe(),
            x= ~date,
            y= ~values,
            color = ~entityName,
            type = 'scatter',
            mode = 'lines+markers') %>% 
      layout(height = 535, legend = list(xanchor = "center", x = 0.5, y = -0.15, orientation = 'h'),
             xaxis = list(visible = 'FALSE',title = '<b> Time(UTC) </b>'),
             yaxis = list(rangemode = 'tozero',title = '<b> #/24s Up </b>')
             ) 

    if(normalize_label()){
      p <- p %>% 
        layout(yaxis = list(ticksuffix = "%"))
    }
    
    p
  })
  
  output$mapa <- renderPlotly({


    geojson <- rjson::fromJSON(file = "geojson/venezuela.geojson")
    g<- list(
      #scope = "south america",
      fitbounds = "locations"
    )
    fig <- plot_ly()
    fig <- fig %>% add_trace(
      type="choropleth",
      geojson=geojson,
      locations=outages$location_name,
      z = outages$score,
      featureidkey = "properties.ESTADO",
      colors = "Reds"
    ) %>% 
      layout(geo = g)
    fig
  })

  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
}
shinyApp(ui, server)
