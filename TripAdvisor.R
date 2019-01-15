require(shiny)
require(leaflet)
require(htmltools)
require(ggplot2)
require(highcharter)
library(dplyr)
library(plotly)
library(shinydashboard)
library(RColorBrewer)
require(shiny)
require(leaflet)
library(DT)


df <- read.csv("Tripdata1.csv", header=TRUE, sep = ',')
data_pie <- read.csv("data_pie.csv", header=TRUE, sep = ',')
data_bar <- read.csv("data_bar.csv", header=TRUE, sep = ',')
data_lile <- read.csv("plot3.csv", header=TRUE, sep = ',')



ui <- dashboardPage(
  
    dashboardHeader(title = "Restaurants Info", titleWidth = 200),
    
    dashboardSidebar(
      width = 200,
      sidebarMenu(
        menuItem("Map", tabName = "Map", icon=icon("dashboard")),
        menuItem("Plot", tabName = "Plots", icon = icon("bar-chart"),
                 selectInput("Inputregion", label = "Select City", 
                             choices = c(unique(as.character(df$City)))),
                 menuSubItem("Plots", tabName = "PiePlot"))
    )),
    
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "PiePlot",
          fluidRow(
            column(width = 6,box(width = NULL,height = 350, 
                                 highchartOutput("PiePlot", height = 350))),
            column(width = 6,box(width = NULL,height = 350, 
                                 plotlyOutput("BarPlot", height = 350)))),
          fluidRow(width = 11,box(width = NULL,height = 350,
              plotlyOutput("LilePlot",height = 350))
          )),
        
        tabItem(tabName = "Map",
        fluidRow(column(width = 9,
                 box(width = NULL, solidHeader = TRUE, 
                     leafletOutput("map"), height=400), 
                 box(width = NULL, solidHeader = TRUE, 
                     status = "primary", dataTableOutput("myTable"))),
                 
                 column(width = 3,
                 box(width = NULL,status = "success", 
                            selectInput("Inputcustyle", label = "Cuisine Style:",
                                        choices = c("Make your choice:",
                                                    (sort(unique(as.character(df$Style))))),
                                        selected = "All")),
                 
                        box(width = NULL,status = "warning", 
                            checkboxGroupInput("inputrating", "Rating:",
                                               choices = c(3,3.5,4,4.5,5)),
                        actionButton("selectall1","Select All"), 
                        selected = c(3,3.5)),
                        box(width = NULL,status = "primary", 
                            checkboxGroupInput("inputPriceTag","Price Tag:",
                                               choices = c("cheap","medium","expensive")),
                        actionButton("selectall","Select All"))

                                ) ) ))))
                

server <- function(input, output, session) {
  
  #---------- TAB MAP-----------------------------------------------------------------------------------

  data <- reactiveValues(clickedMarker = NULL)
  
  #Chanching the PriceTag checkboxes
  observe({
    if(input$selectall == 0)return(NULL)
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"inputPriceTag","Price Tag:",
                               choices = c("cheap","medium","expensive"))
    }
    else 
    {
      updateCheckboxGroupInput(session,"inputPriceTag","Price Tag:",
                               choices = c("cheap","medium","expensive"), 
                               selected = c("cheap","medium","expensive"))
    }
  })
  
  #Chanching the rating checkboxes
  observe({
    if(input$selectall1 == 0)return(NULL)
    else if (input$selectall1%%2 == 0)
    {
      updateCheckboxGroupInput(session,"inputrating", "Rating:",
                               choices = c(3,3.5,4,4.5,5))
    }
    else 
    {
      updateCheckboxGroupInput(session,"inputrating", "Rating:",
                               choices = c(3,3.5,4,4.5,5), 
                               selected = c(3,3.5,4,4.5,5))
    }
  })

  
  #Filtering the data interactive by the choices we do in the input buttoms
  points <- reactive({ 
    
    poi <- filter(df, 
                      Price_Tag %in% input$inputPriceTag,
                      Rating %in% input$inputrating,
                      Style %in% input$Inputcustyle) 

})
  
  
 #Creating the map
  output$map <- renderLeaflet({
    
    pointIcon <- makeIcon(
      iconUrl = "home-address.png",
      iconWidth = 30,
      iconHeight = 30,
      iconAnchorX = 10,
      iconAnchorY = 10
    )
    
      leaflet( ) %>%  
      setView(1.675063, 47.751569, 4) %>%
        
      addProviderTiles(providers$CartoDB.Positron, 
                       options = providerTileOptions(noWrap = TRUE), 
                       group = "Beautiful Map") %>%  # Add default OpenStreetMap map tiles
        
      addAwesomeMarkers(data=points(),
                        lat = ~lat,lng=~lng,
                        icon=pointIcon,
                        popup= paste(sep="","<b>",
                                     "<font size=2 color=#045FB4>","City: ",
                                     "</font>" ,points()$City))
  }) 
   
  
  #When we click in the map appears one table
  
  observeEvent(input$map_marker_click,{
    data$clickedMarker <- input$map_marker_click
   
    output$myTable <- renderDataTable({
      marta <-select(points(), Name, URL_TA, 
                     Price_Tag, Rating, 
                     Ranking, lat, lng)
      marta2 <- marta[duplicated(marta), ]
      marta4 <- filter(marta2,(lat == data$clickedMarker$lat) & (lng == data$clickedMarker$lng))
      marta5 <- select(marta4, Name, 
                       Price_Tag, Rating, 
                       Ranking,  
                      URL_TA)
      datatable(
        marta5,
        rownames = FALSE,
        options = list(searching = TRUE, lengthChange = FALSE, pageLength = 5)
      )
      
  })
  
    
  })
  
  #---------- TAB PLOT-----------------------------------------------------------------------------------
  
  #PIE-PLOT--------------------------------------------
  
  #Data for pie_plot
  datatInput <- reactive({
    ix <- which(data_pie$City == input$Inputregion) 
    soma <- as.vector(t(data_pie[ix, c("Name")]))
    ola <- as.vector(t(data_pie[ix, c("Style")]))
    Style_count <- as.vector(t(data_pie[ix, c("Name")]))
    tmp.data_pie <-data.frame(type= ola, count=Style_count)
    tmp.data_pie$count <- round(tmp.data_pie$count/sum(soma), 3)*100
    return(tmp.data_pie)
  })
  

  #creating the pie plot
  output$PiePlot <- renderHighchart({
    
    highchart() %>%
      
      hc_title(text = "<strong>Percentage of restaurants per cusine style</strong>",
               style = list(fontSize = "15px")) %>%
      
      hc_chart(data=datatInput(), type = "pie", 
               options3d = list(enabled = TRUE, 
                                alpha = 20, beta = 0)) %>%
      
      hc_plotOptions(pie = list(depth = 50)) %>% 
      
      hc_add_series_labels_values(labels = datatInput()$type, 
                                  values = datatInput()$count, 
                                  name = "Total", showInLegend = FALSE)%>%
      
      hc_tooltip(pointFormat = "{point.y}%")
    
  })
  
  
  #BAR-PLOT--------------------------------------------
  
  #filtering the data for the bar plot
  datatInput2 <- reactive({
    
    ix2 <- which(data_bar$City == input$Inputregion)
    cheap <- as.vector(t(data_bar[ix2, c("cheap")]))
    medium <- as.vector(t(data_bar[ix2, c("medium")]))
    expensive <- as.vector(t(data_bar[ix2, c("expensive")]))
    rating <- as.vector(t(data_bar[ix2, c("Rating")]))
    tmp_bar <- data.frame(rating, cheap, medium, expensive)
    
    
    return(tmp_bar)
  })
  
  
  # creating the bar plot
  output$BarPlot <- renderPlotly({
    
    plot_ly(datatInput2(), x = ~rating, y = ~cheap, type = 'bar', 
            name = 'Cheap Restaurants',
            marker = list(color = 'rgba(50, 171, 96, 0.7)')) %>%
      
      add_trace(y = ~medium, name = 'Medium Restaurants', 
                marker = list(color = 'rgba(55, 128, 191, 0.7)',
                              line = list(color = 'rgba(55, 128, 191, 0.7)',
                                          width = 2))) %>%
      
      add_trace(y = ~expensive, name = 'Expensive Restaurants', 
                marker = list(color = 'rgba(219, 64, 82, 0.7)',
                              line = list(color = 'rgba(219, 64, 82, 1.0)',
                                          width = 2)))  %>%
      
      layout(
        title = "Price-Quality Relationship",
        xaxis = list(title = "Rating",
                     showgrid = FALSE
        ),
        yaxis = list(
          title = 'Number of Restaurants within the Price',
          showgrid = FALSE),
        margin = list(l = 65)
      )
      
  })
  
  #LOLLIPOP-PLOT--------------------------------------------
  
  #filtering the data for the LOLLIPOP
  datatInput3 <- reactive({
    
    ix3 <- which(data_lile$City == input$Inputregion)
    y <- as.vector(t(data_lile[ix3, c("Rating")]))
    x <- as.vector(t(data_lile[ix3, c("Ratio")]))
    tmp_lile <- data.frame(x, y)
    tmp_lile$x <- round(tmp_lile$x, 3)
    return(tmp_lile)
  })
  

  #creating the plot
  output$LilePlot <- renderPlotly({
    
    plot_ly(datatInput3(), color=I("gray80")) %>%
      add_segments(x = 0, xend = ~x, y = ~y, yend = ~y, showlegend = FALSE) %>%
      add_markers(x =~x, y=~y,
                  color = I("blue")) %>%
      layout(
        title = "Average Number of reviews/restaurant per rating",
        xaxis = list(title = "Number of reviews/restaurant",
                     showgrid = FALSE
                     ),
        yaxis = list(
          title = 'Rating',
          showgrid = FALSE),
        margin = list(l = 65)
      )

   
  })
  
  
  }
  
shinyApp(ui, server)
  
  
  
  
  