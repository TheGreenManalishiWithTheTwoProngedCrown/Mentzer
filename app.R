library(shiny)
library(shinydashboard) 
library(plotly)
library(shinyWidgets)
library(tidyr)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(reactable)
source('testing.R')
source("get_entities.R")
source("get_outages.R")

options(rsconnect.check.certificate = FALSE)


ui <- dashboardPage(
  dashboardHeader(title = "Proyecto Mentzer"),
  dashboardSidebar(tags$head(
    tags$style(
      HTML('
        #Id009{width: 260px}
    ')
    )),width = 300, sidebarMenu(
    
    pickerInput(
      inputId = "Id008",
      label = "Elegir Región", 
      choices = regions$name,
      multiple = TRUE,
      options =  list("tick-icon" = "glyphicon glyphicon-ok-sign",
                      `actions-box` = TRUE,
                      `live-Search` = TRUE,
                      liveSearchStyle = "contains"),
      selected = outage_regions$location_name
      
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
    
    pickerInput(
      inputId = "Id001",
      label = "Data Types", 
      choices = c("Active Probing", "BGP", "Telescope"),
      multiple = FALSE,
      options =  list("tick-icon" = "glyphicon glyphicon-ok-sign"
                      )
    ),
    
    airDatepickerInput(
      inputId = "Id009",
      timepicker = TRUE,
      range = TRUE,
      todayButton = TRUE,
      label = "Fecha",
      clearButton = TRUE,
      placeholder = "Seleccione un periodo de tiempo",
      value = c(now()-hours(12), now()+hours(8))
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
      box(plotlyOutput("timeseries") %>% 
            withSpinner(size = 1.5, type = 4),
          width = 12,
          height = 580),
      box(leafletOutput("mapa"),
          width = 6,
          title = h3(HTML("<b>Cortes de Internet por region"), align = "center")),
      box(reactableOutput("isp_table"))
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
             vnzla = input$Id011,
             datatype=input$Id001)})


normalize_label <- reactive({
  input$Id007
})


  
  


 
 title_y <- reactive({
    case_when(
      input$Id001 == "Active Probing" ~ '<b> #/24s Up </b>',
      input$Id001 == "BGP" ~ '<b> BGP (# Visible /24s) </b>',
      input$Id001 == "Telescope" ~ '<b> Telescope ( # Unique Source IPs) </b>'
    )
   
 })
 output$text<- renderPrint(title_y())


  output$timeseries <- renderPlotly({
    
    p <- plot_ly(test_dataframe(),
            x= ~date,
            y= ~values,
            color = ~entityName,
            type = 'scatter',
            mode = 'lines+markers') %>% 
      layout(height = 535, legend = list(xanchor = "center", x = 0.5, y = -0.15, orientation = 'h'),
             xaxis = list(visible = 'FALSE',title = '<b> Time(UTC) </b>'),
             yaxis = list(rangemode = 'tozero',title = title_y())
             ) 

    if(normalize_label()){
      p <- p %>% 
        layout(yaxis = list(ticksuffix = "%"))
    }
    
    
    p
  })
  
  bins <- c(0, 10, 20, 50, 100, 200, 500, 1000,Inf)
  pal <- colorBin("Reds", domain = venequia$score, bins = bins)

  labels <- venequia$LABEL %>% lapply(htmltools::HTML)
  output$mapa <- renderLeaflet({
    
    
    if(dim(outages)[1] != 0){  
    leaflet(venequia) %>% 
      addTiles() %>% 
      addPolygons(
        fillColor= ~pal(score),
        weight = 2,
        opacity = 1,
        color = "grey",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      )%>% addLegend(pal = pal, values = ~score, opacity = 0.7, title = NULL,
                     position = "bottomright")}
    
     
    
    
    
  })
  

  output$isp_table<-renderReactable({
    reactable(providers,
              defaultColDef = colDef(show = FALSE),
              columns = list(
                org = colDef(name = "Nombre",
                             cell = function(value,index){
                               codigo<-providers$Codigo[index]
                               div(
                                 div(style = "font-weight: 600", value),
                                 div(style = "font-size: 0.75rem", codigo)
                               )   
                             },
                             show = TRUE),
                ip_count = colDef(name = "# de IP's",
                                  show = TRUE),
                score = colDef(show = TRUE,
                               name = "Nivel",
                               style = function(value,index) {
                                 level1 <- providers$level[index]
                                 if (level1 == "critical"){
                                   color1 <- "rgba(255,0,0,0.5)"
                                 } else if (level1 == 'normal'){
                                   color1 <- "rgba(0,0,0,0)"
                                 } else{
                                   color1 <- "rgba(0,255,0,0.2)"
                                 }
                                 list(background = color1)
                                 
                    
                               })
              ),
              striped = TRUE)
  })
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
}
shinyApp(ui, server)
