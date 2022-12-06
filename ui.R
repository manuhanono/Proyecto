library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)

shinyUI(fluidPage(theme = shinytheme("yeti"),useShinydashboard(), list(
  tags$head(
    HTML('<link rel="icon" href="nyc.svg" 
                type="image/svg" />'))),
      navbarPage(title = div(div(img(src="nyc.svg")), "....  .   ............."),
      tabPanel("Análisis General", h1("Análisis sobre los accidentes de tránsito en la ciudad de Nueva York"),
               h4("Hecho por: Manuel Hanono y Bruno Soifer"), h5("Para este trabajo, se utilizó la base de datos de", em("NYC Open Data"), "sobre todos los accidentes de transito ocurridos en la ciudad de Nueva York desde", strong("julio de 2012", style = "color:#27a4f2"), "hasta mediados de", strong("octubre de 2022.", style = "color:#27a4f2"), " La presentación final consiste de cuatro tabs de acuerdo a lo que se quiere explicar. Las mismas son: Análisis General, Análisis Geográfico, Análisis de Causas y Consecuencias y Conclusiones."),
               h5("A lo largo del desarrollo del informe, se va a buscar encontrar diferentes conclusiones sobre una base de",strong("1.937.848", style = "color:#27a4f2"), "accidentes.")
               ,hr(),h3("Comportamiento de los accidentes a lo largo de los años."), h5("Como comienzo del análisis, se puede observar en el primer grafico de la izquierda la cantidad de accidentes de transito ocurridos por año. Se puede ver claramente una", strong("tendencia a la baja", style = "color:#27a4f2"),  "de los accidentes de transito en los últimos años. Si bien se podría creer que esto es debido al encierro en la pandemia, que podría imposibilitar el uso de vehiculos al mismo nivel que se utilizaba antes, se puede ver en el grafico a su lado que evidentemente, mas allá de la pandemia, en los últimos dos años ha habido una baja considerable en los accidentes.",style = "text-align: justify; background-color:white")
              ,fluidRow(column(plotlyOutput("Grafico2"), width = 4, offset = 1), column(plotlyOutput("Grafico3"), width = 6)) 
               ,
              fluidRow(column(h5("Para contextualizar un poco, es posible que la tendencia a la baja vista en los accidentes se deba a una tendencia a la baja en la utilización de vehículos motorizados, impulsada por diversas razones, como pueden ser la",  em("transición del trabajo presencial hacia la modalidad Home Office"), "que impulsó la pandemia desde el 2020. Como se ve en el árticulo de CNBC, donde solo el 10% de los empleados volvieron a la modalidad presencial todos los días y que no más del 50% de los empleados va a la oficina de manera semanal. También puede haber inferencia del" , em("proyecto de ley NYC 25x25"), "propuesto por el alcalde de la ciudad, donde se propone reconvertir un 25% de las calles utilizadas por autos en calles peatonales y bicisendas hacía el 2025.", style = "text-align: justify; background-color:white"), width = 4), column(img(src = "NYC25x25.png", height="100%", width="100%"), width = 4), column(img(src = "NYCHO.png", height="40%", width="100%"), width = 4))
              ,
              hr(), h3("Accidentes según los dias de la semana y las horas del día"), 
              fluidRow( column(h5("A continuación, tras haber observado un pantallazo del panorama general en cuanto al periodo analizado, se va a proceder a observar con más detalle el", em("comportamiento de los accidentes según las horas del día y los días de la semana", style = "color:#27a4f2"), "para poder ver si existen ciertos patrones en estos. A su vez, se buscará ver si al agrupar los días por laborales o fin de semana se puede encontrar alguna tendencia.", style = "text-align: justify; background-color:white"), width = 12))
              ,
               fluidRow(column(h5("En este gráfico se puede observar cómo la cantidad de accidentes varía dependiendo de la hora y el día en el que se producen los mismos. En primer lugar, se puede notar que para los 7 días en conjunto hay una tendencia, que se puede notar viendo que el número de accidentes totales desciende en los horarios nocturnos, y alcanza sus puntos más altos en los horarios diurnos.
Aplicando esta separación, se puede ver que para los horarios nocturnos, los días de fin de semana suelen haber mayor cantidad de accidentes respecto de los días de semana. Ocurre lo contrario en los horarios diurnos, donde en los días laborales (lunes a viernes) hay más accidentes respecto de los sábados y domingos.
",style = "text-align: justify;background-color:white"), hr(),img(src = "LateCrashes.png", height="100%", width="100%"), width=4), column(plotlyOutput("Grafico1"), width = 6), column(wellPanel(checkboxInput("Filtrar", "Agrupar a los días por Tipo de Día (Dia Laboral o Fin de Semana)"), style = "text-align: justify; background: white") ,width = 2))
               ,hr(), h3("Accidentes según las diferentes razones y sus impactos"), h5("En la base de datos, buena parte de los accidentes tienen una razón o causa que haya llevado al mismo. A continuación se observan las razones de los accidentes en sus totalidad, y cómo cambian las", strong("causas",style = "color:#27a4f2"), "de los accidentes a través de las horas, notando que existen causas que se concentran en distintos rangos horarios. Además, se visualizan aquellas causas que generan accidentes que dejan como consecuencia al menos una persona fallecida.", style = "text-align:justify"),
              fluidRow(column(plotlyOutput("Grafico6"), width = 9), column(wellPanel(radioButtons("Ver", "",c("Ver todos los horarios","Ver razones para las muertes", "Ver por hora")) ,sliderInput("Hora", "Seleccione la hora:", min = 00, max = 23, value = 12), style = "text-align: justify; background: white"), h5(textOutput("TextG6"),style = "text-align: justify;background-color:white"), width = 3)),
              hr(), h3("Accidentes según actores involucrados y sus consecuencias"), h5("Es importante notar que en un accidente de tránsito puede haber diferentes partes involucradas. La base distingue entre tres diferentes: Las personas en vehículos, los ciclistas y los peatones.", style = "text-align:justify"),
              fluidRow(column(plotlyOutput("Grafico11"), width = 4), column(plotlyOutput("Grafico12"), width = 4), column(h5("En el primer gráfico de barras se puede notar como la cantidad de", strong("lesionados", style = "color:#27a4f2") ,"es mayor en", strong("gente sobre vehículos
", style = "color:#27a4f2"),"que sobre los peatones o los ciclistas. La cantidad de gente sobre vehículos que resulta lesionada es casi 4 veces
más que los aquellos peatones que sufren lesiones como consecuencia de un accidente, y 8 veces más aproximadamente que los ciclistas.
Por lo tanto, se esperaría que ocurra algo similar con las muertes ocasionadas, más aún teniendo en cuenta que la mayoría de los
choques ocurren entre vehículos de motor.", br(), br(), "Sin embargo, si se analiza la totalidad de", strong("muertes", style = "color:#27a4f2"),"se llega a la conclusión de que los",  strong("peatones", style = "color:#27a4f2"), "son
los que sufren la peor parte de los accidentes. El número de peatones fallecidos como consecuencia de los accidentes 
es el más elevado, ya que tiene casi un 25% más que las personas que circulan con vehículos. En cuanto a los ciclistas,
se observa que solo el 7% de la totalidad de muertes pertenecen a este grupo, que es considerablemente bajo
debido a que los ciclistas no suelen tener tantos accidentes",style = "text-align: justify; background-color:white"), width = 4)),
              hr(), h3("Cantidad de accidentes según cada condado de Nueva York"), h5("La Ciudad de Nueva York se encuentra dividida en 5 diferentes", strong("Boroughs", style = "color:#27a4f2"), "o condados: Brooklyn, Bronx, Manhattan, Queens y Staten Island. La distribución de los accidentes no es la misma en los 5 condados, por lo que a continuación se analizan los condados de Nueva York según la ubicación en la que ocurrieron los accidentes.
", style = "text-allign: justify"),
              fluidRow(column(h5("Analizando los accidentes por barrios, se puede decir que la mayoría ocurren en Brooklyn, con más
de 420.000 casos. En segundo lugar se encuentra el barrio de Queens, seguido por el de Manhattan. De
los 5 barrios identificados en la Ciudad de Nueva York, en los 3 mencionados anteriormente se concentran
el 81% de los accidentes de tránsito. Del 19% restante, el 77% proviene del barrio de Bronx, y solo el 
23% de Staten Island (cerca de un 4% del total de accidentes)", style = "text-align: justify; background-color:white"), hr(),img(src = "NOTAB.png", height="100%", width="100%"), width = 4),column(plotOutput("Grafico13"), width = 8))
              )
                ,
      tabPanel("Análisis Geográfico", h1("Análisis Geográfico"),
               fluidRow(
                 column(h4("Filtros para el mapa:"),radioButtons("VerMapa", "",c("Ver accidentes que generaron muertes","Ver accidentes que involucren lesiones y/o muertes a ciclistas", "Ver accidentes que involucren lesiones y/o muertes a peatones", "Ver por año")) , numericInput("Año", "Insertar un año:", min = 2012, max = 2022, step = 1, value = c(2022))
                       ,width = 4), column(leafletOutput("Grafico5"),width = 8)),
               fluidRow(column(h5(textOutput("TextG5")), width = 8, offset = 4))),
      
      tabPanel("Análisis de Causas y Consecuencias", titlePanel("Análisis de Causas y Consecuencias"), h5("En esta parte se buscará puntualizar sobre un poco de lo que se fue presentando. La intención es llegar a profundizar más acerca de las consecuencias que pueden generar los accidentes, principalmente enfocando en personas", strong("lesionadas", style = "color:#27a4f2"), "y en personas", strong("fallecidas", style = "color:#27a4f2"), "con el fin de poder ayudar a entes gubernamentales a disminuir la cantidad de estos dos grupos encontrando cuáles son las posibles", strong("causas", styile = "color:#27a4f2"), "que las generan."),                
      hr(), h3("Accidentes, muertes y lesionados por período de tiempo"),h5("Para comenzar con esta parte del informe, se busca ver si se mantiene cierta", strong("tendencia temporal") ,"para los accidentes, las muertes y los lesionados. Para ello se dividió a los años en 3 períodos de tiempo de 3 años cada uno. Se excluyó de este análisis al 2012 y al 2022, ya que esos años estan incompletos en la base de datos.", style = "text-align:justify"),
      fluidRow(column(selectInput("ML", "",c("Ver Accidentes","Ver Muertes", "Ver Lesionados")),h5(textOutput("TextG10"), style = "text-align:justify"), width = 4),column(plotlyOutput("Grafico10"), width = 7)),
      hr(), h3("Muertes y lesionados por hora del día"),h5("Siguiendo con el mismo análisis comparativo entre los accidentes que generan muertes y los que generan lesionados, ahora se procede a observar un análisis en cuanto a la hora del día y que incidencias puede tener el mismo, para observar posibles tendencias y ver si estas son las mismas para los accidentes que generan lesionados y los que generan muertes.", style = "text-align:justify"),
      fluidRow(column(plotlyOutput("Grafico15"), width = 6, offset = 1), column(h5(textOutput("TextG15"), style = "text-align:justify"),selectInput("ML2", "",c("Ver Muertes", "Ver Lesionados")),hr(), img(src = "LateCrashes.png", height="100%", width="100%"), width = 4)),
      hr(), h3("Muertes y lesionados por tipo de auto y rango horario"),h5("Teniendo en cuenta que las muertes y los lesionados son causados por distintos tipos de autos, a continuación se analiza cuáles son los tipos de autos que más ocasionan las muertes y los lesionados en promedio, dividiendo el día en distintos rangos horarios.", style = "text-align:justify"),
      fluidRow(column(plotlyOutput("Grafico16"), width = 6),column(h5(textOutput("TextG16"), style = "text-align:justify"),selectInput("ML3","" ,c("Ver Muertes", "Ver Lesionados")), width = 4)),
      hr(), h3("Muertes y lesionados con respecto a la cantidad de vehículos involucrados en el accidente"),h5("Para un accidente, en el dataset se detalla entre otros datos, la cantidad de vehículos involucrados que participaron del mismo, las lesiones y la muertes que se ocasionaron. A continuación, se realiza un análisis sobre cómo la cantidad de autos que impactan en un accidente tiene consecuencia en las lesiones y muertes que generan.", style = "text-align:justify"),
      fluidRow(column(h5(textOutput("TextG17"), style = "text-align:justify"),selectInput("ML5","" ,c("Ver Muertes", "Ver Lesionados")), width = 4), column(plotlyOutput("Grafico17"), width = 6, offset = 1)),
      hr(), h3("Diferencias entre cantidad de accidentes según los feriados"),h5("A continuación se hace un análisis sobre la diferencia en la cantidad de accidentes en un día, dependiendo si el mismo fue considerado feriado nacional en la Ciudad de Nueva York (se toman en cuenta todos los años de la base).", style = "text-align:justify"),
      fluidRow(column(h5("Se visualizan dos diagramas de “caja y bigote”, uno que representa aquellos días considerados como", strong("feriados,", style = "color:#27a4f2") ,"y otro en los que representa aquellos días que no lo son. Se observa que el rango intercuartílico y la mediana cobra valores más altos para aquellos días no feriados. Además, en los días no feriados se observan los valores más altos y más bajos. Ambos grupos presentan outliers, que se encuentran por encima del extremo superior de cada diagrama.", br(), br(), "Por lo tanto, con el análisis de ambos diagramas se puede intuir sí", em("existe una diferencia entre los días feriados y no feriados,"), "donde en promedio se puede esperar que en estos últimos haya mayor cantidad de accidentes por día.
", style = "text-align:justify"), width = 4), column(plotlyOutput("Grafico19"), width = 6, offset = 1)),
      hr(), h3("Cantidad de autos involucrados en los accidentes por día a lo largo de los años"),h5("Para un mismo accidente, en el dataset se encuentran choques que involucran de 1 a 5 vehículos. A continuación se analiza cómo cambia el", strong("promedio de autos involucrados", style = "color:#27a4f2") ,"en un mismo choque, dependiendo del día de la semana.", style = "text-align:justify"),
      fluidRow(column(h5(textOutput("TextG4"), style = "text-align:justify"),selectInput("ML4","" ,c("Ver según tipo de día", "Ver todos los días")), width = 4), column(plotlyOutput("Grafico4"), width = 6, offset = 1))
      ,hr(), h3("¿Entre qué tipos de auto son los accidentes entre 2 autos?"),h5("Como extra, se realizó este diagrama de Sankey con el fin de ver los choques entre los tipos de auto.", style = "text-align:justify"),
      fluidRow(column(plotlyOutput("Grafico18"), width = 8, offset = 2))
      
      )
      ,
      tabPanel("Conclusiones", titlePanel("Conclusiones generales del análisis"), 
               valueBox("110.540", h5("Fueron los accidentes en el año", strong("2021.") ,"Es el registro anual más", strong("bajo") ,"de toda la base.", style = "text-align: justify"), icon = NULL, color = "aqua", width =4, href = NULL),
               valueBox("231.564", h5("Fue la cantidad de accidentes ocurridos en el año", strong("2018."),"Es el registro anual más", strong("alto") ,"de toda la base.", style = "text-align: justify"), icon = NULL, color = "aqua", width = 4, href = NULL),
               valueBox("2692", h5("Fueron las", strong("muertes"), "causadas por los", strong("2585"), "accidentes mortales en Nueva York, en el periodo analizado.", style = "text-align: justify"), icon = NULL, color = "aqua", width = 4, href = NULL),
               br(),fluidRow(column(hr(), width = 12)),
               
               infoBox("LESIONADOS", value = "21,54%", icon = shiny::icon("user-injured", lib = "font-awesome"), subtitle = h5("Es el porcentaje de", em("lesionados"), "con respecto al total de accidentes. Representa a", em("417531"), "accidentes."), color = "aqua", width = 3, href = NULL, fill = FALSE),
               infoBox("BROOKLYN", value = "35%", icon = shiny::icon("car", lib = "font-awesome"), subtitle = h5("De los accidentes que generaron lesionados sucedieron en el condado", em("Brooklyn.")), color = "aqua", width = 3, href = NULL, fill = FALSE),
               infoBox("MUERTES", value = "22%", icon = shiny::icon("arrow-up", lib = "font-awesome"), subtitle = h5("Fue el aumento de la cantidad de muertes desde", em("2018 a 2021."), "El total pasó de", em("231 a 294.")), color = "aqua", width = 3, href = NULL, fill = FALSE),
               infoBox("PEATONES", value = "50,70%", icon = shiny::icon("person-walking", lib = "font-awesome"), subtitle = h5("De los", em("fallecidos"), "son clasificados como", em("peatones."), "El resto se acumula entre ciclistas y gente en vehículos." ), color = "aqua", width = 3, href = NULL, fill = FALSE),
          
               )
      #,inverse = TRUE
      ) ))
runGitHub( "Proyecto", "mhanono")
