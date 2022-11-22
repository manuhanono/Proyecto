library(shiny)
library(tidyverse)
library(DT)
library(arrow)
library(hrbrthemes)
library(plotly)
library(leaflet)

crashes = read_parquet("/Users/manu/Downloads/crashes2.parquet")

source("helpers.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    output$Grafico1 <- renderPlotly({
      if(input$Filtrar){ggplotly(g33)}
      else{ggplotly(g1)}
    })
    
    output$Grafico2 <- renderPlotly({
      ggplotly(g2)
    })
    
    #output$t1 <- renderText({})
    
    output$Grafico3 <- renderPlotly({
      ggplotly(g4)
    })
    
    output$Grafico4 <- renderPlotly({
      if(input$Filtrar){ggplotly(g8)}
      else{ggplotly(g7)}
    })
    
    output$Grafico6 <- renderPlotly({
      ggplotly(g6)
    })
    
    data2 <- reactive({
      data <- mapa1
      if(!is.null(input$Año)){
        data<-data %>%
          filter(data$YEAR == input$Año[1]) 
      }
      data
    })
    
    output$Grafico5 <- renderLeaflet({
      data2 <- data2()
      if(input$Muertes){ mapa1 = mapa1 %>% filter(`NUMBER OF PERSONS KILLED` != 0)
        leaflet(mapa1) %>% addTiles() %>% addMarkers(
        clusterOptions = markerClusterOptions()
      ) %>% addProviderTiles(providers$CartoDB.Positron)}
      else{leaflet(mapa1) %>% addTiles() %>% addMarkers(
        clusterOptions = markerClusterOptions()
      ) %>% addProviderTiles(providers$CartoDB.Positron)}
    })
})
