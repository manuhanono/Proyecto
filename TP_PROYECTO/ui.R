library(shiny)
library(tidyverse)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("yeti"), list(
  tags$head(
    HTML('<link rel="icon" href="nyc.svg" 
                type="image/svg" />'))),
      navbarPage(title = div(div(img(src="nyc.svg")), "....  .   ............."),
      tabPanel("Análisis General", h1("Análisis sobre los accidentes de transito en la ciudad de Nueva York"),
               h4("Hecho por: Manuel Hanono y Bruno Soifer"), h5("Para este trabajo, se utilizó la base de datos de NYC Open Data sobre todos los accidentes de transito ocurridos en la ciudad de Nueva York desde Julio del 2012 hasta Octubre del 2022. Se analizarán 1.937.848 accidentes. La presentación final consiste de cuatro tabs de acuerdo a lo que se quiere explicar. Las mismas son: Análisis General, Análisis Geográfico, Análisis de Causas y Consecuencias y Conclusiones."),
                fluidRow(column(plotlyOutput("Grafico2"), width = 4), column(plotlyOutput("Grafico3"), width = 6), column(h5("Como comienzo del análisis, se puede observar en el primer grafico de la izquierda la cantidad de accidentes de transito ocurridos por año. Se puede ver claramente una tendencia a la baja de los accidentes de transito en los últimos años. Si bien se podría creer que esto es debido al encierro en la pandemia, que podría imposibilitar el uso de vehiculos al mismo nivel que se utilizaba antes, se puede ver en el grafico a su lado que evidentemente, mas allá de la pandemia, en los últimos dos años ha habido una baja considerable en los accidentes."), width = 2) ) 
               ,
               fluidRow(column(wellPanel(checkboxInput("Filtrar", "Agrupar a los días por Tipo de Día (Dia Laboral o Fin de Semana)"), style = "background: white") ,h5("En este gráfico se puede observar cómo la cantidad de accidentes varía dependiendo de la hora y el día en el que se producen los mismos. En primer lugar, se puede notar que para los 7 días hay una tendencia, donde la mayoría de los accidentes se producen entre las 12 y las 19 horas, donde parece lógico que haya más movimiento, y entre las 00 y las 05 alcanzan su punto más bajo. Además, se puede notar que en los horarios nocturnos suele haber más accidentes los fines de semana, mientras que en el rango de 6 de la mañana hasta las 20 horas suele haber mayor prevalencia de los días laborales."), width=4), column(plotlyOutput("Grafico1"), width = 6))
               ,
              fluidRow(column(plotlyOutput("Grafico6"), width = 9), h5("Finalmente, con respecto a las principales causas que generan los accidentes, se ve un claro predominio de la categoria de distracción o desatención del conductor, siendo esta la razón explicativa para aproximadamente el 25% de los accidentes. Cabe aclarar que, en muchos de los accidentes, la causa no estaba especificada. Es interesante ver como el alcohol influye en poco más del 1% de las causas de los accidentes, unos 21792 a lo largo de 10 años. Se va a profundizar más adelante sobre como estas razones impactan en las lesiones o muertes de las personas incolucradas.")))
                ,
      tabPanel("Análisis Geográfico", 
               fluidRow(
                 column(wellPanel(sliderInput("Año", "Seleccionar año:", min = 2012, max = 2022, step = 1, value = 2022), checkboxInput("Muertes", "Ver solo los que accidentes que generen muertes")),width = 4), column(leafletOutput("Grafico5"),width = 8))),
      tabPanel("Análisis de Causas y Consecuencias", titlePanel("Todos los datos"),                fluidRow(column(plotlyOutput("Grafico4"), width=8), column(h5("Siguiendo con el análisis a lo largo del tiempo, se observa en el presente gráfico que no solo la cantidad de accidentes fue disminuyendo a lo largo de los años, sino que también la cantidad promedio de autos involucrados en un accidente viene siguiendo una tendencia a la baja desde el año 2016. Además, se puede notar que en todos los años, los días de fin de semana es donde se producen los accidentes con mayor cantidad de autos involucrados. Es decir, hay una tendencia que se viene m
anteniendo desde el año 2012 donde en aquellos días no laborales un accidente involucra a mayor cantidad de autos que en un día laboral."), width = 4)),"content"),
      tabPanel("Conclusiones", titlePanel("Todos los datos"), "content")
               )))