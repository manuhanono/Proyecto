library(shiny)
library(tidyverse)
library(DT)
library(arrow)
library(hrbrthemes)
library(plotly)
library(leaflet)
library(treemapify)

crashes = read_parquet("/Users/manu/Downloads/crashes2.parquet")
# Plot
g1 <- crashes %>% group_by(Hora = substr(`CRASH TIME`,start = 1, stop = 2),Dia = WEEKDAY) %>% summarise(Cantidad=n()) %>% ggplot() + geom_point(aes(x=Hora, y=Cantidad, color=Dia)) +
  ggtitle("Cantidad de accidentes de tránsito por día y hora") + theme_minimal() + scale_color_manual(values = c("#08306B","#08519C","#27a4f2", "#3eaef4","#77cff2", "#8ad6f4", "#b9e7f8"))

g2 <- crashes %>% group_by(YEAR) %>% summarise(Cantidad = n()) %>% 
  ggplot(aes(x=YEAR, y = Cantidad))  +
  geom_point(color = "#3eaef4")+
  geom_area(fill = "#58c7f3", alpha=0.1) +
  geom_line(color = "#58c7f3") + 
  scale_y_continuous(breaks = seq(from = 0, to = 250000, by = 50000))+
  scale_x_continuous(breaks= seq(from=2012, to =2022, by=2)) +
  ggtitle("Accidentes de tránsito a lo largo de los años") + 
  theme_minimal() + theme(axis.title.x=element_blank(),
                          axis.title.y =element_blank())

graf1 = crashes %>% group_by(Hora = substr(`CRASH TIME`,start = 1, stop = 2),Dia = WEEKDAY) %>% summarise(Cantidad=n())

g3 = graf1
g3$Tipo =case_when(
  g3$Dia %in% c("Monday","Tuesday", "Wednesday", "Thursday", "Friday") ~ "Dia laboral",
  T ~ "Fin de semana"
)
g33 = ggplot(g3) + geom_point(aes(x=Hora, y=Cantidad, color=Tipo)) +
  ggtitle("Cantidad de choques por tipo de día y hora") + 
  theme_minimal() + scale_color_manual(values = c("#27a4f2", "#b9e7f8"))

g4 = crashes %>% group_by(YEAR, MONTH) %>% summarise(Cantidad = n()) %>% ggplot(aes(YEAR, MONTH)) +  geom_tile(aes(fill = Cantidad), color = "white",lwd = 1.5,linetype = 1)  +
  geom_text(aes(label = Cantidad), color = "#3b444b", size = 3) +
  scale_fill_gradient(low = "white", high = "#58c7f3") + scale_y_discrete(limits = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")) +
  scale_x_continuous(breaks=c(2012:2022)) +
  ggtitle("Cantidad de choques por mes y año")  + theme_minimal() +
  theme(legend.position = "none") + theme(axis.title.x=element_blank(),
                                          axis.title.y =element_blank())

#Grafico de cantidad de autos involucrados por dia de semana, agrupado por años
g7 = crashes %>% filter(CANT!=0) %>% group_by(WEEKDAY, YEAR) %>% 
  summarise(autos_involucrados=mean(CANT)) %>% ggplot() + geom_point(aes(x=YEAR, y=autos_involucrados, color=WEEKDAY)) +
  scale_x_continuous(breaks= seq(from=2012, to =2022, by=2))  + theme_minimal() + scale_color_manual(values = c("#08306B","#08519C","#27a4f2", "#3eaef4","#77cff2", "#8ad6f4", "#b9e7f8"))


#Mismo grafico que antes, dividido por si es o no fin de semana
g8 = crashes %>% filter(CANT!=0) %>% group_by(WEEKDAY, YEAR) %>% 
  summarise(autos_involucrados=mean(CANT)) %>% mutate(Dia=case_when(
    WEEKDAY %in% c("Monday","Tuesday", "Wednesday", "Thursday", "Friday") ~ "Dia laboral",
    T ~ "Fin de semana"
  )) %>% ggplot() + geom_point(aes(x=YEAR, y=autos_involucrados, color=Dia)) +
  scale_x_continuous(breaks= seq(from=2012, to =2022, by=2)) + 
  theme_minimal() + scale_color_manual(values = c("#27a4f2", "#b9e7f8"))

mapa1 = crashes %>% select(lon, lat, YEAR, `NUMBER OF PERSONS KILLED`) %>% filter(!is.na(lon) & lat > 40 & lat < 42 & lon > -74)

g9 = leaflet(mapa1) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
) %>% addProviderTiles(providers$CartoDB.Positron)


crashes6 = crashes %>% stack(select=c(`CONTRIBUTING FACTOR VEHICLE 1`, `CONTRIBUTING FACTOR VEHICLE 2`, `CONTRIBUTING FACTOR VEHICLE 3`, `CONTRIBUTING FACTOR VEHICLE 4`, `CONTRIBUTING FACTOR VEHICLE 5`)) %>% filter(!is.na(values))
#Corrección de columnas mal escritas:
crashes6[crashes6 == 'Cell Phone (hand-held)'] = "Cell Phone (hand-Held)"
crashes6[crashes6 == 'Drugs (Illegal)'] = "Drugs (illegal)"
crashes6[crashes6 == 'Illnes'] = "Illness"
crashes6[crashes6 == 'Reaction to Other Uninvolved Vehicle'] = "Reaction to Uninvolved Vehicle"
crashes6 = crashes6  %>% group_by(values)%>% summarise(Cantidad = n()) %>% filter(Cantidad > 20000 & values != "Unspecified")

g6 = crashes6 %>% ggplot() + geom_col(aes(x = Cantidad, y=reorder(as.factor(values),+(Cantidad)), fill = Cantidad )) +
  ggtitle("Principales razones para los accidentes") + 
  scale_x_continuous(breaks = seq(from = 0, to = 500000, by = 50000)) + theme_minimal() +
  theme(legend.position = "none") + theme(axis.title.x=element_blank(),
                                          axis.title.y =element_blank())
