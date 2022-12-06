library(shiny)
library(tidyverse)
library(DT)
library(arrow)
library(hrbrthemes)
library(plotly)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

crashes = read_parquet("https://github.com/manuhanono/Proyecto/blob/main/crashes2.parquet?raw=true")

source("https://raw.githubusercontent.com/manuhanono/Proyecto/main/helpers.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$Grafico1 <- renderPlotly({
    if(input$Filtrar){ggplotly(g33)}
    else{ggplotly(g1)}
  })
  
  output$Grafico2 <- renderPlotly({
    ggplotly(g2)
  })
  
  output$Grafico3 <- renderPlotly({
    ggplotly(g4) 
  })
  
  output$Grafico4 <- renderPlotly({
    if(input$ML4 == "Ver según tipo de día"){ggplotly(g8)}
    else if (input$ML4 == "Ver todos los días"){ggplotly(g7)}
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
    if(input$VerMapa == "Ver accidentes que generaron muertes"){ mapa1 = mapa1 %>% filter(`NUMBER OF PERSONS KILLED` != 0)
    leaflet(mapa1) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions(),
      popup =  ~ paste("Personas que fallecieron: ",`NUMBER OF PERSONS KILLED` , ". Año: ", YEAR, ". Cantidad de autos involucrados:", CANT)
    ) %>% addProviderTiles(providers$CartoDB.Positron)}
    else if(input$VerMapa == "Ver accidentes que involucren lesiones y/o muertes a ciclistas") {
      d = crashes %>% select(lon, lat, `NUMBER OF CYCLIST INJURED`, `NUMBER OF CYCLIST KILLED`) %>% filter(!is.na(lon) & lat > 40 & lat < 42 & lon > -74 & lon < -72 & `NUMBER OF CYCLIST INJURED` !=0)
      leaflet(d) %>% addTiles() %>% addMarkers(
        clusterOptions = markerClusterOptions(),
        popup =  ~ paste("Ciclistas que fallecieron: ",`NUMBER OF CYCLIST KILLED` , ". Ciclistas lesionados:", `NUMBER OF CYCLIST INJURED`)
      ) %>% addProviderTiles(providers$CartoDB.Positron)
    }
    else if(input$VerMapa == "Ver accidentes que involucren lesiones y/o muertes a peatones") {
      d2 = crashes %>% select(lon, lat, `NUMBER OF PEDESTRIANS INJURED`, `NUMBER OF PEDESTRIANS KILLED`) %>% filter(!is.na(lon) & lat > 40 & lat < 42 & lon > -74 & lon < -72 & `NUMBER OF PEDESTRIANS INJURED` !=0)
      leaflet(d2) %>% addTiles() %>% addMarkers(
        clusterOptions = markerClusterOptions(),
        popup =  ~ paste("Peatones que fallecieron: ",`NUMBER OF PEDESTRIANS KILLED` , ". Peatones lesionados:", `NUMBER OF PEDESTRIANS INJURED`)
      ) %>% addProviderTiles(providers$CartoDB.Positron)
    }
    else if (input$VerMapa == "Ver por año"){leaflet(data2) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions()
    ) %>% addProviderTiles(providers$CartoDB.Positron)}
  })
  
  output$TextG5 <- renderText({
    if(input$VerMapa == "Ver accidentes que generaron muertes"){"Con respecto a las muertes a lo largo de los años, no se identifica ninguna tendencia en particular. Sí, en cambio, se puede ver que la mayoría de estos accidentes se generan en intersecciones de avenidas con avenidas o calles."}
    else if(input$VerMapa == "Ver accidentes que involucren lesiones y/o muertes a ciclistas"){"Este tipo de accidentes está distribuido alrededor de toda la ciudad, pero se observan que en las avenidas se acumulan una cantidad considerable de los mismos."}
    else if(input$VerMapa == "Ver accidentes que involucren lesiones y/o muertes a peatones"){"Con respecto a los peatones, se observan más accidentes respecto de los ciclistas. Este tipo de accidentes, si bien la mayoría ocurren en avenidas, hay bastantes otros que ocurren en arterias más pequeñas. "}
    else if(input$VerMapa == "Ver por año") {"Se puede observar cada año en particular desde el 2012 hasta el 2022, ingresándolo en el filtro. No se observa ningún patrón o tendencia a lo largo de los años que permita distinguir el comportamiento de los accidentes."}
  })
  
  data3 <- reactive({
    data1 <- graf15
    data1<-data1 %>% filter(Hora == input$Hora[1]) 
    data1
  })
  
  output$Grafico6 <- renderPlotly({
    data3 <- data3() %>% filter(cantidad > 1000)
    t <- list(size = 10, color = "#242424") 
    if(input$Ver == "Ver todos los horarios"){g6}
    else if(input$Ver == "Ver razones para las muertes"){g16}
    else if(input$Ver == "Ver por hora"){ggplotly(data3  %>% ggplot() + geom_col(aes(x = cantidad, y=reorder(as.factor(Factor),+(cantidad)), fill = cantidad)) +
                                                    ggtitle(paste("Principales razones para los accidentes a las", data3$Hora, "hs")) + 
                                                    scale_x_continuous(breaks = seq(from = 0, to = 50000, by = 5000)) + theme_minimal() +
                                                    theme(legend.position = "none") + theme(axis.title.x=element_blank(),axis.title.y =element_blank()), originalData = FALSE) %>% add_text(textfont = t, x=data3$cantidad, text= paste(" ", data3$cantidad),textposition="right")}
  }) 
  output$TextG6 <- renderText({
    if(input$Ver == "Ver todos los horarios"){"Con respecto a las principales causas que generan los accidentes, se ve un claro predominio de la categoria de distracción o desatención del conductor, siendo esta la razón explicativa para aproximadamente el 25% de los accidentes. Cabe aclarar que, en muchos de los accidentes, la causa no estaba especificada. Es interesante ver como el alcohol influye en poco más del 1% de las causas de los accidentes, unos 21792 a lo largo de 10 años."}
    else if(input$Ver == "Ver razones para las muertes"){"Al ver las razones de los accidentes ordenadas según la cantidad de muertes que generaron, se puede observar como cambia bastante con respecto al gráfico general. Razones como el alcohol, velocidades altas y el evitar controles de tránsito pasan a ser mucho más predominantes."}
    else if(input$Ver == "Ver por hora"){"Cuando vemos las razones según la hora, podemos entender que van cambiando a lo largo de los diferentes rangos horarios. A la madrugada, vuelve a aparecer el alcohol como protagonista, mientras que durante el día tiene casi nula incidencia."}
  })
  
  output$Grafico11 <- renderPlotly({g12})
  output$Grafico12 <- renderPlotly({g13})
  output$Grafico13 <- renderPlot({g14})
  output$Grafico14 <- renderPlotly({g11}
  )
  
  output$Grafico10 <- renderPlotly({
    if(input$ML == "Ver Accidentes"){ggplotly(g27)}
    else if (input$ML == "Ver Muertes"){ggplotly(g28)}
    else if(input$ML == "Ver Lesionados"){ggplotly(g29)}
  })
  
  output$TextG10 <- renderText({
    if(input$ML == "Ver Accidentes"){paste("En primer lugar, se visualiza la evolución de la cantidad de accidentes respecto de los tres períodos. Se puede ver como entre los 2 primeros períodos, es decir, de 2013 a 2018, se ubican las líneas muy por encima del tercer y último período, que nunca supera en promedio los 12500 accidentes por mes.", "Además, se puede notar que los máximos y mínimos para los tres períodos no son los mismos. Para los períodos 2013 a 2015 y 2016 a 2018, el momento del año donde se alcanzan más accidentes es cerca del mes de junio, y los puntos más bajos se registran al comienzo del año. Sin embargo, para el período más reciente, existen dos momentos donde se alcanzan los máximos; en los meses de enero y julio. Los meses con menor cantidad de casos para el período mencionado son marzo y abril." , sep="\n") }
    else if (input$ML == "Ver Muertes"){"Con respecto a la cantidad de muertes a lo largo de los meses, se observa un patrón un poco más parecido en los tres períodos, respecto de la cantidad de accidentes. Se puede ver que hasta el mes de abril, el período 2013-2015 es el que tiene mayor cantidad de casos en promedio. Sin embargo, desde abril hasta el final del año el período 2019-2021 ocupa el primer puesto en muertes, algo que es poco esperable si se toma en cuenta el análisis hecho para la cantidad de casos en conjunto. El período 2016 a 2018, si no se toma en cuenta el mes de agosto, siempre está en último lugar."}
    else if(input$ML == "Ver Lesionados"){"En el análisis para los lesionados, se nota un comportamiento bastante distinto con respecto a las muertes. A diferencia de este, donde el período 2016-2018 se encontraba casi todo el año en tercer lugar, ahora este mismo período se aleja considerablemente de los otros 2 para ocupar el primer lugar. En el mes de junio, en los años 2016 a 2018 se llegaron a registrar más de 4000 lesionados en promedio. Analizando los otros dos períodos, se encuentran bastantes similitudes, como sus máximos o mínimos, sin embargo, se nota un comportamiento más lineal para el período 2013-2015. En el mismo, el número de lesionados sube constantemente desde que arranca el año hasta el mes de junio, donde empieza a decaer."}
  })
  
  output$Grafico15 <- renderPlotly({
    if(input$ML2 == "Ver Muertes"){ggplotly(g18)}
    else if (input$ML2 == "Ver Lesionados"){ggplotly(g19)}
  })
  
  output$Grafico16 <- renderPlotly({
    if(input$ML3 == "Ver Muertes"){ggplotly(g166)}
    else if (input$ML3 == "Ver Lesionados"){ggplotly(g15)}
  })
  
  output$TextG15 <- renderText({
    if(input$ML2 == "Ver Muertes"){"En el gráfico de muertes por hora del día, se puede ver que los números más altos se encuentran entre las 18 y las 00hs. Sin embargo, el pico se encuentra en las 04hs, horario en el cual se produjeron un total de 150 muertes a lo largo de los años. Los números más bajos de muertes se producen cuando hay más accidentes, que es en el rango horario
desde las 07 hasta las 16hs. Además, hay una tendencia a la subida en cantidad de muertes en estos horarios, que se dispara desde las 18hs"
    }
    else if (input$ML2 == "Ver Lesionados"){"Con respecto a la cantidad total de lesionados por hora en la que ocurre el accidente, se puede notar un comportamiento distinto respecto del gráfico de fallecidos. El gráfico alcanza su pico en los horarios donde ocurren más accidentes, entre las 14 y las 18hs. Además, se puede notar que cuando se alcanzan los números más bajos (sobre la noche), hay una tendencia a que el número incremente con el paso de las horas. Una vez que se llega a las 18hs, el número de lesionados comienza a bajar."
    }
  })
  
  output$TextG16 <- renderText({
    if(input$ML3 == "Ver Muertes"){"La primera diferencia que se observa con el presente gráfico a diferencia del anterior, es que las muertes que se ocasionan en los accidentes de tránsito no están únicamente concentradas en tres tipos de autos, ni tampoco los mismos tienen los mismos porcentajes que tenían. Ahora a los autos de tipo sedan, vehículos deportivos y taxis, se suman camiones y motos. Cabe aclarar que los porcentajes, al igual que antes, representan al tipo de auto que causa el accidente fatal, y no aquel que lo sufre. A pesar de que cambia la distribución y los porcentajes con respecto al gráfico de lesionados, en los horarios nocturnos (00 a 07hs y 20 a 23hs) los autos de tipo sedán son los que causan mayores muertes, y en los horarios diurnos (08 a 19 hs) los vehículos deportivos se llevan el mayor porcentaje. Se destaca que los taxis tienen porcentajes menos elevados cuando se analizan las muertes respecto del análisis hecho para los lesionados. Además se puede apreciar como la mayoría de los accidentes que provocan los camiones ocurren durante el día, y que las motos manejan números bastante parecidos a lo largo del día"}
    else if (input$ML3 == "Ver Lesionados"){"En el gráfico de barras se puede visualizar, dependiendo del rango horario, qué tipo de autos son los que causan en mayor porcentaje las lesiones. Se observa como entre los grupos de Sedan, vehículos deportivos y taxis ocasionan más del 90% en la mayoría de los horarios. En los horarios nocturnos (de 00 a 03hs, 04 a 07hs y 20 a 23hs) el auto de tipo Sedan es aquel que causa la mayor cantidad de lesionados. En los horarios diurnos (08 a 11hs, 12 a 15hs y 16 a 19hs), el auto que causa más lesionados es el de tipo deportivo. En todos los casos, los taxis se encuentran en tercera posición, acumulando poco menos del 25% del total de lesionados. El rango horario más 'disparejo' en términos de porcentaje de lesionados es el de 00 a 03hs, donde los autos Sedan son los que concentran el 40% del total de las lesiones. Además, en este horario entre los autos Sedan, vehículos deportivos y taxis, concentran el 94% de la suma total de lesionados"}
  })
  
  output$TextG4 <- renderText({
    if(input$ML4 == "Ver según tipo de día"){"Si se analiza cada día por separado, se puede notar que hasta el año 2015 inclusive el promedio de autos por accidente estaba muy cerca de 2, mientras que a partir del año 2016 este promedio ha ido descendiendo, notando otra fuerte caída en el año 2020. Por lo tanto, no solo se puede concluir que los accidentes con el paso del tiempo han ido bajando (teniendo en cuenta gráficos anteriores), sino que también que menos vehículos chocan entre sí. "}
    else if (input$ML4 == "Ver todos los días"){"Segmentando los días entre días laborales y fines de semana, se puede afirmar que a lo largo de los años, si bien el promedio fue bajando, siempre los días de fin de semana mantienen un promedio de autos involucrados mayor respecto de los días laborales. Esto último puede llegar a deducir que dado que un choque se produzca un sábado o un domingo, la gravedad del mismo podría ser mayor, asumiendo que cuantos más vehículos impactan entre sí, la gravedad del accidente podría ser mayor."}
  })
  
  output$Grafico17 <- renderPlotly({
    if(input$ML5 == "Ver Muertes"){ggplotly(g20)}
    else if (input$ML5 == "Ver Lesionados"){ggplotly(g17)}
  })
  
  output$TextG17 <- renderText({
    if(input$ML5 == "Ver Muertes"){"Para este gráfico, se observa a partir de la cantidad de autos involucrados en un accidente, el porcentaje de accidentes en el que hubo al menos una persona fallecidos. Al igual que en el gráfico anterior, donde se veía el porcentaje de lesionados, en este caso el porcentaje de personas fallecidas tiene un comportamiento muy similar. En aquellos casos donde solo 2 vehículos tienen un accidente, el porcentaje de accidentes que reportan al menos un fallecido es realmente bajo, de un 0,05%. A medida que se suman vehículos al accidente, este porcentaje va aumentando, hasta llegar a un 0,91% de accidentes que reportan al menos un fallecido, cuando el accidente involucra a exactamente 5 autos. Por lo tanto, se puede llegar a una conclusión parecida a la anterior, donde se cree que cuantos más autos formen parte de un accidente, la gravedad del mismo sea mayor, y por lo tanto cause más lesionados y más muertes"}
    else if (input$ML5 == "Ver Lesionados"){"El gráfico muestra cómo varía el porcentaje de lesionados en los accidentes, dependiendo de cuántos autos están involucrados en el mismo. Se tiene en cuenta a la categoría 'Hay lesionados' si en el accidente hubo al menos una persona que resultó herida. Teniendo en cuenta que la mayoría de accidentes es entre dos vehículos, es esperable que esta sea la categoría con menor porcentaje de lesionados, ya que al ser el accidente más común de todas variables, se dejan de lado otras variables que puedan incidir sobre la gravedad y por ende los lesionados que pueda dejar un accidente (como tipos de autos, causas, día, horario, mes, etc.). También se puede observar que a medida que aumenta de 3, 4 y hasta 5 vehículos involucrados por choque, el porcentaje de no lesionados va bajando. Se puede intuir que esto ocurre debido a que cuantos más autos se vean involucrados en un accidente, la gravedad del mismo será mayor. En los registros donde se involucra a 5 vehículos en un mismo accidente, casi la mitad de los mismos trae como consecuencia al menos una persona herida."}
  })
  
  output$Grafico18 <- renderPlotly({
    ggplotly(g11)
  })
  
  output$Grafico19 <- renderPlotly({
    g21
  })
  
  
})
