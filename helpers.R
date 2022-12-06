library(shiny)
library(tidyverse)
library(DT)
library(arrow)
library(hrbrthemes)
library(plotly)
library(leaflet)
library(treemapify)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(RColorBrewer)

crashes = read_parquet("https://github.com/manuhanono/Proyecto/blob/main/crashes2.parquet?raw=true")
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
  theme(legend.position = "none", axis.text = element_text(face="bold")) + theme(axis.title.x=element_blank(),
                                          axis.title.y =element_blank())

#Grafico de cantidad de autos involucrados por dia de semana, agrupado por años
g7 = crashes %>% filter(CANT!=0) %>% group_by(WEEKDAY, YEAR) %>% 
  summarise(Autos=mean(CANT)) %>% ggplot() + geom_point(aes(x=YEAR, y=Autos, color=WEEKDAY)) +
  scale_x_continuous(breaks= seq(from=2012, to =2022, by=2))  + theme_minimal() + scale_color_manual(values = c("#08306B","#08519C","#27a4f2", "#3eaef4","#77cff2", "#8ad6f4", "#b9e7f8"))


#Mismo grafico que antes, dividido por si es o no fin de semana
g8 = crashes %>% filter(CANT!=0) %>% group_by(WEEKDAY, YEAR) %>% 
  summarise(Autos=mean(CANT)) %>% mutate(Dia=case_when(
    WEEKDAY %in% c("Monday","Tuesday", "Wednesday", "Thursday", "Friday") ~ "Dia laboral",
    T ~ "Fin de semana"
  )) %>% ggplot() + geom_point(aes(x=YEAR, y=Autos, color=Dia)) +
  scale_x_continuous(breaks= seq(from=2012, to =2022, by=2)) + 
  theme_minimal() + scale_color_manual(values = c("#27a4f2", "#b9e7f8"))

mapa1 = crashes %>% select(lon, lat, YEAR, `NUMBER OF PERSONS KILLED`, CANT) %>% filter(!is.na(lon) & lat > 40 & lat < 42 & lon > -74 & lon < -72)

g9 = leaflet(mapa1) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
) %>% addProviderTiles(providers$CartoDB.Positron)


crashes6 = crashes %>% stack(select=c(`CONTRIBUTING FACTOR VEHICLE 1`, `CONTRIBUTING FACTOR VEHICLE 2`, `CONTRIBUTING FACTOR VEHICLE 3`, `CONTRIBUTING FACTOR VEHICLE 4`, `CONTRIBUTING FACTOR VEHICLE 5`)) %>% filter(!is.na(values))
#Corrección de columnas mal escritas:
crashes6[crashes6 == 'Cell Phone (hand-held)'] = "Cell Phone (hand-Held)"
crashes6[crashes6 == 'Drugs (Illegal)'] = "Drugs (illegal)"
crashes6[crashes6 == 'Illnes'] = "Illness"
crashes6[crashes6 == 'Reaction to Other Uninvolved Vehicle'] = "Reaction to Uninvolved Vehicle"
crashes7 = crashes6  %>% group_by(values)%>% summarise(Cantidad = n()) %>% filter(Cantidad > 20000 & values != "Unspecified")

crashes8 = crashes %>% filter(`NUMBER OF PERSONS KILLED` == 1) %>% stack(select=c(`CONTRIBUTING FACTOR VEHICLE 1`, `CONTRIBUTING FACTOR VEHICLE 2`, `CONTRIBUTING FACTOR VEHICLE 3`, `CONTRIBUTING FACTOR VEHICLE 4`, `CONTRIBUTING FACTOR VEHICLE 5`)) %>% filter(!is.na(values))
crashes8 = crashes8  %>% group_by(values)%>% summarise(cantidad = n()) %>% filter(values != "Unspecified")
crashes9 = crashes %>% filter(`NUMBER OF PERSONS KILLED` == 2) %>% stack(select=c(`CONTRIBUTING FACTOR VEHICLE 1`, `CONTRIBUTING FACTOR VEHICLE 2`, `CONTRIBUTING FACTOR VEHICLE 3`, `CONTRIBUTING FACTOR VEHICLE 4`, `CONTRIBUTING FACTOR VEHICLE 5`)) %>% filter(!is.na(values))
crashes9 = crashes9  %>% group_by(values)%>% summarise(cantidad = n() * 2) %>% filter(values != "Unspecified")
crashes10 = crashes %>% filter(`NUMBER OF PERSONS KILLED` == 3) %>% stack(select=c(`CONTRIBUTING FACTOR VEHICLE 1`, `CONTRIBUTING FACTOR VEHICLE 2`, `CONTRIBUTING FACTOR VEHICLE 3`, `CONTRIBUTING FACTOR VEHICLE 4`, `CONTRIBUTING FACTOR VEHICLE 5`)) %>% filter(!is.na(values))
crashes10 = crashes10  %>% group_by(values)%>% summarise(cantidad = n() * 3) %>% filter(values != "Unspecified")
crashes11 = crashes %>% filter(`NUMBER OF PERSONS KILLED` == 4) %>% stack(select=c(`CONTRIBUTING FACTOR VEHICLE 1`, `CONTRIBUTING FACTOR VEHICLE 2`, `CONTRIBUTING FACTOR VEHICLE 3`, `CONTRIBUTING FACTOR VEHICLE 4`, `CONTRIBUTING FACTOR VEHICLE 5`)) %>% filter(!is.na(values))
crashes11 = crashes11  %>% group_by(values)%>% summarise(cantidad = n() * 4) %>% filter(values != "Unspecified")
crashes13 = crashes %>% filter(`NUMBER OF PERSONS KILLED` == 5) %>% stack(select=c(`CONTRIBUTING FACTOR VEHICLE 1`, `CONTRIBUTING FACTOR VEHICLE 2`, `CONTRIBUTING FACTOR VEHICLE 3`, `CONTRIBUTING FACTOR VEHICLE 4`, `CONTRIBUTING FACTOR VEHICLE 5`)) %>% filter(!is.na(values))
crashes13 = crashes13  %>% group_by(values)%>% summarise(cantidad = n() * 5) %>% filter(values != "Unspecified")
crashes14 = crashes %>% filter(`NUMBER OF PERSONS KILLED` == 8) %>% stack(select=c(`CONTRIBUTING FACTOR VEHICLE 1`, `CONTRIBUTING FACTOR VEHICLE 2`, `CONTRIBUTING FACTOR VEHICLE 3`, `CONTRIBUTING FACTOR VEHICLE 4`, `CONTRIBUTING FACTOR VEHICLE 5`)) %>% filter(!is.na(values))
crashes14 = crashes14  %>% group_by(values)%>% summarise(cantidad = n() * 8) %>% filter(values != "Unspecified")
cr1 = merge(crashes8, crashes9, all = TRUE)
cr2 = merge(crashes10, crashes11, all = TRUE)
cr3 = merge(crashes13, crashes14, all=TRUE)
cr4 = merge(cr1, cr2, all = TRUE)
cr5 = merge(cr4, cr3, all = TRUE)
#Corrección de columnas mal escritas:
cr5[cr5 == 'Reaction to Other Uninvolved Vehicle'] = "Reaction to Uninvolved Vehicle"
cr5[cr5 == 'Drugs (Illegal)'] = "Drugs (illegal)"
cr5[cr5 == 'Illnes'] = "Illness"
cr5[cr5 == 'Cell Phone (hand-held)'] = "Cell Phone (hand-Held)"

cr5 = cr5 %>% group_by(values) %>% summarise(Cantidad = sum(cantidad)) %>% filter(Cantidad >= 20)

t <- list(size = 10, color = "#242424") 
g6 = ggplotly(crashes7 %>% ggplot(aes(x = Cantidad, y=reorder(as.factor(values),+(Cantidad)), fill = Cantidad )) + geom_col() + 
  ggtitle("Principales razones para los accidentes") + 
  scale_x_continuous(breaks = seq(from = 0, to = 500000, by = 50000)) + theme_minimal() +
  theme(legend.position = "none") + theme(axis.title.x=element_blank(),
                                          axis.title.y =element_blank()), originalData = FALSE) %>% add_text(textfont = t, x=crashes7$Cantidad, text= paste(" ", crashes7$Cantidad),textposition="right")

g16 = ggplotly(cr5 %>% ggplot(aes(x = Cantidad, y=reorder(as.factor(values),+(Cantidad)), fill = Cantidad )) + geom_col() + 
               ggtitle("Principales razones para los accidentes que generan muertes") + 
               scale_x_continuous(breaks = seq(from = 0, to = 400, by = 20)) + theme_minimal() +
               theme(legend.position = "none") + theme(axis.title.x=element_blank(),
                                                       axis.title.y =element_blank()), originalData = FALSE) %>% add_text(textfont = t, x=cr5$Cantidad, text= paste(" ", cr5$Cantidad),textposition="right")

#%>% add_annotations(x = crashes7$Cantidad, text = crashes7$Cantidad, showarrow = FALSE)

graf10 = crashes %>% group_by(Hora = substr(`CRASH TIME`,start = 1, stop = 2),Factor = `CONTRIBUTING FACTOR VEHICLE 1`) %>% summarise(Cantidad=n())
graf11 = crashes %>% group_by(Hora = substr(`CRASH TIME`,start = 1, stop = 2),Factor = `CONTRIBUTING FACTOR VEHICLE 2`) %>% summarise(Cantidad=n())
graf12 = crashes %>% group_by(Hora = substr(`CRASH TIME`,start = 1, stop = 2),Factor = `CONTRIBUTING FACTOR VEHICLE 3`) %>% summarise(Cantidad=n())
graf13 = crashes %>% group_by(Hora = substr(`CRASH TIME`,start = 1, stop = 2),Factor = `CONTRIBUTING FACTOR VEHICLE 4`) %>% summarise(Cantidad=n())
graf14 = crashes %>% group_by(Hora = substr(`CRASH TIME`,start = 1, stop = 2),Factor = `CONTRIBUTING FACTOR VEHICLE 5`) %>% summarise(Cantidad=n())

graf15 = rbind(graf10, graf11, graf12, graf13, graf14) %>% group_by(Factor, Hora) %>% summarise(cantidad = sum(Cantidad)) %>% filter(cantidad > 100 & Factor != "Unspecified" & !is.na(Factor))
graf15$Hora = as.numeric(graf15$Hora)

g10 = graf15 %>% filter(cantidad > 1000)%>% ggplot(aes(x = cantidad, y=reorder(as.factor(Factor),+(cantidad)), fill = cantidad)) + geom_col()+ geom_text(aes(label = cantidad), hjust = 1.5, colour = "black")  +
  ggtitle("Principales razones para los accidentes") + 
  scale_x_continuous(breaks = seq(from = 0, to = 500000, by = 50000))+ theme_minimal() +
  theme(legend.position = "none") + theme(axis.title.x=element_blank(),
                                          axis.title.y =element_blank())

g11 <- plot_ly(type = "sankey",orientation = "h",
               node = list(
                 label = c("Bike", "Bus", "Motorcycle", "Sedan", "Small Vehicle", "Sport Vehicle", "Taxi", "Truck", "Van", "Sedan", "Sport Vehicle", "Taxi", "Truck", "Bus", "Dump", "Flat Bed", "Van", "Small Vehicle"),
                 color = brewer.pal(8, "Blues"),
                 pad = 15, thickness = 10,
                 line = list(color = "black", width = 0.5  )),
               link = list(
    source = c(0,0,0,0,1,1,1,1,1,2,2,2,3,3,3,3,3,3,3,4,4,4,5,5,5,5,5,6,6,6,7,7,8),
    target = c(9,10,11,12,13,9,10,11,12,9,10,11,14,15,9,10,11,12,16,17,10,11,14,10,11,12,16,11,12,16,12,16,16),
    value =  c(18264,17419,13440, 1177,1310,14821,16534,14282,2514, 4693,4419,2290,1855,1119,203311,252899,26167,55287,5945,2455,7297,23469,1011,176414,154219,41112,14489,228392,19785, 22962, 6834,2215, 2268)))
g11 <- g11 %>% layout(title = "Autos involucrados en accidentes de 2 vehículos",font = list(size = 11))

crashes2 = crashes %>% select(`NUMBER OF CYCLIST INJURED`, `NUMBER OF CYCLIST KILLED`, `NUMBER OF PEDESTRIANS INJURED`, `NUMBER OF PEDESTRIANS KILLED`, `NUMBER OF MOTORIST INJURED`, `NUMBER OF MOTORIST KILLED`) %>% filter(!is.na(`NUMBER OF CYCLIST INJURED`) & !is.na(`NUMBER OF CYCLIST KILLED`) & !is.na(`NUMBER OF PEDESTRIANS INJURED`) & !is.na(`NUMBER OF PEDESTRIANS KILLED`)& !is.na(`NUMBER OF MOTORIST INJURED`) & !is.na(`NUMBER OF MOTORIST KILLED`))
Personas <- c("Peatones","Ciclistas", "Gente en vehículos")
lesionados <- c(sum(crashes2$`NUMBER OF PEDESTRIANS INJURED`), sum(crashes2$`NUMBER OF CYCLIST INJURED`), sum(crashes2$`NUMBER OF MOTORIST INJURED`))
fallecidos <- c(sum(crashes2$`NUMBER OF PEDESTRIANS KILLED`), sum(crashes2$`NUMBER OF CYCLIST KILLED`), sum(crashes2$`NUMBER OF MOTORIST KILLED`))
data2 <- data.frame(Personas, lesionados, fallecidos) 

g12 <- plot_ly(data2, x = ~Personas, y = ~lesionados, type = 'bar', name = 'Lesionados',
               texttemplate = '%{y}', textposition = 'outside') %>% layout(yaxis = list(title = 'Lesionados'), barmode = 'stack') 
g13 <- plot_ly(data2, x = ~Personas, y = ~fallecidos, type = 'bar', name = 'Fallecidos' ,
               texttemplate = '%{y}', textposition = 'outside') %>% layout(yaxis = list(title = 'Fallecidos'), barmode = 'stack')

consulta = crashes %>% group_by(BOROUGH) %>% summarise(cantidad=n()) %>% head(5)
vector = consulta$cantidad
geo = st_read("/Users/manu/TP_PROYECTO/Borough Boundaries.geojson")
df = geo
df = df %>% arrange(boro_name)
df = df %>% mutate(Cantidad = vector)
g14 <-ggplot() + geom_sf(data = df, aes(fill=Cantidad)) + geom_sf_label(data=df, aes(label = paste(boro_name, "-", Cantidad))) + theme_void() +
  theme(legend.position="none")
g14

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}
data2 = crashes %>% filter(`NUMBER OF PERSONS KILLED` >0) %>% group_by(dia = as.integer(substr(DATE, start=9, stop=10))) %>%  summarise(cantidad_fallecidos = sum(`NUMBER OF PERSONS KILLED`)) %>% filter(dia!=31)
df <-crashes %>% filter(PINJ >0) %>% group_by(dia = as.integer(substr(DATE, start=9, stop=10))) %>%  summarise(cantidad_lesionados = sum(PINJ)) %>% filter(dia!=31) %>% cbind(cantidad_fallecidos = data2$cantidad_fallecidos) %>% accumulate_by(~dia)
g23 <- ggplotly(ggplot(df,aes(dia, cantidad_fallecidos, frame = frame)) +
                  geom_line()) %>%  layout(
                    title = "Fallecidos por día del mes", yaxis = list(  title = "Cantidad",  zeroline = F, tickprefix = "$" ),
                    xaxis = list(title = "Día", zeroline = F,  showgrid = F   ) ) %>%  animation_opts(  frame = 200,   transition = 0,   redraw = FALSE) %>%  animation_slider(currentvalue = list(  prefix = "Day "))

df = crashes %>% filter(PINJ >0) %>% group_by(dia = as.integer(substr(DATE, start=9, stop=10))) %>%  summarise(cantidad_lesionados = sum(PINJ)) %>% filter(dia!=31) %>% accumulate_by(~dia)
g22 <- ggplotly(ggplot(df,aes(dia, cantidad_lesionados, frame = frame)) +
                  geom_line()) %>%  layout(
                    title = "Lesionados por día del mes", yaxis = list(  title = "Cantidad",  zeroline = F, tickprefix = "$" ),
                    xaxis = list(title = "Día", zeroline = F,  showgrid = F   ) ) %>%  animation_opts(  frame = 200,   transition = 0,   redraw = FALSE) %>%  animation_slider(currentvalue = list(  prefix = "Day "))

graf9 = crashes %>% filter(`NUMBER OF PERSONS KILLED`>0) %>% group_by(Hora = substr(`CRASH TIME`,start = 1, stop = 2)) %>% summarise(Muertes = sum(`NUMBER OF PERSONS KILLED`))
graf9_1 = crashes %>% filter(PINJ>0) %>% group_by(Hora = substr(`CRASH TIME`,start = 1, stop = 2)) %>% summarise(Lesionados = sum(PINJ))

g18 = graf9 %>% ggplot() + geom_point(aes(x = Hora, y = Muertes), color = "#58c7f3") + ggtitle("Muertes según la hora del día")+theme_minimal()
g19 = graf9_1 %>% ggplot() + geom_point(aes(x = Hora, y = Lesionados), color = "#58c7f3")+ ggtitle("Lesionados según la hora del día") + theme_minimal()


crashessankey = crashes %>% mutate(Vehicle = case_when(
  `VEHICLE TYPE CODE 1` == "4 dr sedan" | `VEHICLE TYPE CODE 1` == "Sedan" ~ "Sedan",
  `VEHICLE TYPE CODE 1` == "Bike" ~ "Bike",
  `VEHICLE TYPE CODE 1` == "Box Truck" | `VEHICLE TYPE CODE 1`== "LARGE COM VEH(6 OR MORE TIRES)" | `VEHICLE TYPE CODE 1` == "Pick-up Truck" | `VEHICLE TYPE CODE 1` == "PICK UP TRUCK" | `VEHICLE TYPE CODE 1` == "Tractor Truck Diesel" ~ "Truck",
  `VEHICLE TYPE CODE 1` == "Bus" | `VEHICLE TYPE CODE 1` == "BUS" ~ "Bus",
  `VEHICLE TYPE CODE 1` == "LIVERY VEHICLE" | `VEHICLE TYPE CODE 1` == "SMALL COM VEH(4 TIRES)"~ "Small Vehicle",
  `VEHICLE TYPE CODE 1` == "Motorcycle" ~ "Motorcycle",
  `VEHICLE TYPE CODE 1` == "PASSENGER VEHICLE" | `VEHICLE TYPE CODE 1`== "Taxi" | `VEHICLE TYPE CODE 1` == "TAXI" ~ "Taxi",
  `VEHICLE TYPE CODE 1` == "SPORT UTILITY / STATION WAGON" | `VEHICLE TYPE CODE 1`== "Station Wagon/Sport Utility Vehicle" ~ "Sport Vehicle",
  `VEHICLE TYPE CODE 1` == "Van" | `VEHICLE TYPE CODE 1` == "VAN" ~ "Van",
  T ~ "OTHER"
))

crashessankey = crashessankey %>% mutate(vehicle_2= case_when(
  `VEHICLE TYPE CODE 2` == "4 dr sedan" | `VEHICLE TYPE CODE 2` == "Sedan" ~ "Sedan",
  `VEHICLE TYPE CODE 2` == "Bike" | `VEHICLE TYPE CODE 2`== "BICYCLE" | `VEHICLE TYPE CODE 2` == "E-Bike" | `VEHICLE TYPE CODE 2` == "E-Scooter" ~ "Bike",
  `VEHICLE TYPE CODE 2` == "Box Truck" | `VEHICLE TYPE CODE 2`== "LARGE COM VEH(6 OR MORE TIRES)" | `VEHICLE TYPE CODE 2` == "Pick-up Truck" | `VEHICLE TYPE CODE 2` == "PICK UP TRUCK" | `VEHICLE TYPE CODE 2` == "Tractor Truck Diesel" ~ "Truck",
  `VEHICLE TYPE CODE 2` == "Bus" | `VEHICLE TYPE CODE 2` == "BUS" ~ "Bus",
  `VEHICLE TYPE CODE 2` == "Convertible" ~ "Convertible",
  `VEHICLE TYPE CODE 2` == "Dump" ~ "Dump",
  `VEHICLE TYPE CODE 2` == "Flat Bed" ~ "Flat Bed",
  `VEHICLE TYPE CODE 2` == "LIVERY VEHICLE" | `VEHICLE TYPE CODE 2` == "SMALL COM VEH(4 TIRES)"~ "Small Vehicle",
  `VEHICLE TYPE CODE 2` == "Motorcycle" | `VEHICLE TYPE CODE 2` == "MOTORCYCLE" ~ "Motorcycle",
  `VEHICLE TYPE CODE 2` == "PASSENGER VEHICLE" | `VEHICLE TYPE CODE 2`== "Taxi" | `VEHICLE TYPE CODE 2` == "TAXI" ~ "Taxi",
  `VEHICLE TYPE CODE 2` == "SPORT UTILITY / STATION WAGON" | `VEHICLE TYPE CODE 2`== "Station Wagon/Sport Utility Vehicle" ~ "Sport Vehicle",
  `VEHICLE TYPE CODE 2` == "Van" | `VEHICLE TYPE CODE 2` == "VAN" ~ "Van",
  T ~ "OTHER"
))

crashessankey = crashessankey %>% filter(Vehicle != "OTHER" & vehicle_2 != "OTHER")
g44 = crashessankey %>% filter(PINJ>0) %>% group_by(hora = substr(`CRASH TIME`,start = 1, stop = 2), Vehicle) %>% summarise(cantidad_lesionados = sum(PINJ)) %>% mutate(Porcentaje = round(cantidad_lesionados/sum(cantidad_lesionados), 3) * 100) 
g44$hora = as.integer(g44$hora)
g44 = g44 %>% mutate(Rango = case_when(
  hora <= 3 ~ "00 a 03hs",
  hora <= 7 ~ "04 a 07hs",
  hora <= 11 ~ "08 a 11hs",
  hora <= 15 ~ "12 a 15hs",
  hora <= 19 ~ "16 a 19hs",
  hora <= 23 ~ "20 a 23hs",
  T ~ "ERROR"
))

g44 = g44 %>% group_by(Rango, Vehicle) %>% summarise(total_lesionados = sum(cantidad_lesionados)) %>% mutate(Porcentaje = round(total_lesionados/sum(total_lesionados), 3) * 100)
g44 = g44 %>% mutate(agrupar = case_when(
  Vehicle %in% c("Sedan", "Sport Vehicle", "Taxi") ~ "no",
  T ~ "si"
))
g44_1 = g44 %>% group_by(Rango, agrupar) %>% summarise(Porcentaje_final = sum(Porcentaje))
g44_1 = g44_1 %>% filter(agrupar == "si")

g44 = g44 %>% filter (Vehicle %in% c("Sedan", "Sport Vehicle", "Taxi"))
g44 = data.frame(g44)
g44 = g44 %>% rbind(c("00 a 03hs", "Other", 2028, 5.9, "no"), c("04 a 07hs", "Other", 3088, 9.3, "no"),c("08 a 11hs", "Other", 6984, 10.5, "no"),c("12 a 15hs", "Other", 8536, 9.7, "no"),c("16 a 19hs", "Other", 8348, 8.3, "no"),c("20 a 23hs", "Other", 4091, 6, "no"))
g44$Porcentaje = as.double(g44$Porcentaje)
g44 = g44 %>% arrange(Rango,Porcentaje)


g15 = g44 %>% ggplot() + geom_col(aes(x=Rango, y=Porcentaje, fill=Vehicle))+ theme_minimal() + scale_fill_manual(values = c("#08306B","#08519C","#27a4f2","#77cff2", "#8ad6f4", "#b9e7f8"))

g4_2 = crashessankey %>% filter(`NUMBER OF PERSONS KILLED`>0) %>% group_by(hora = substr(`CRASH TIME`,start = 1, stop = 2), Vehicle) %>% summarise(cantidad_fallecidos = sum(`NUMBER OF PERSONS KILLED`)) %>% mutate(Porcentaje = round(cantidad_fallecidos/sum(cantidad_fallecidos), 3) * 100) 
g4_2$hora = as.integer(g4_2$hora)
g4_2 = g4_2 %>% mutate(Rango = case_when(
  hora <= 3 ~ "00 a 03hs",
  hora <= 7 ~ "04 a 07hs",
  hora <= 11 ~ "08 a 11hs",
  hora <= 15 ~ "12 a 15hs",
  hora <= 19 ~ "16 a 19hs",
  hora <= 23 ~ "20 a 23hs",
  T ~ "ERROR"
))
g4_2 = g4_2 %>% group_by(Rango, Vehicle) %>% summarise(total_fallecidos = sum(cantidad_fallecidos)) %>% mutate(Porcentaje = round(total_fallecidos/sum(total_fallecidos), 3) * 100)
g4_3 = g4_2 %>% mutate(agrupar = case_when(
  Porcentaje >=5 ~ "no",
  T ~ "si"
))

g4_3 = g4_3 %>% filter(agrupar == "si")
g4_3 = g4_3 %>% group_by(Rango) %>% summarise(suma_Porcentaje = sum(Porcentaje))
g4_2 = g4_2 %>% filter(Porcentaje >= 5)
g4_2 = data.frame(g4_2)
g4_2 = g4_2 %>% rbind(c("00 a 03hs", "Other", 6, 4.2), c("04 a 07hs", "Other", 9, 5.4), c("08 a 11hs", "Other", 6, 6.4),c("12 a 15hs", "Other", 7, 5.6), c("16 a 19hs", "Other", 10, 5.7), c("20 a 23hs", "Other", 15, 8.4))
g4_2$Porcentaje = as.double(g4_2$Porcentaje)
g4_2 = g4_2 %>% arrange(Vehicle, Porcentaje)
g166 = g4_2 %>% ggplot() + geom_col(aes(x=Rango, y=Porcentaje, fill=Vehicle)) + scale_fill_manual(values = c("#08306B","#08519C","#27a4f2", "#3eaef4","#77cff2", "#8ad6f4", "#b9e7f8")) + theme_minimal()


cor1 = crashes %>% group_by(PINJ, CANT) %>% summarise(cantidad=n())
cor1$CANT = as.character(cor1$CANT)
cor1 = cor1 %>% filter(PINJ != "NA")
cor1 = cor1 %>% mutate(Lesionados = case_when(
  PINJ == 0 ~ "No hubo lesionados",
  T ~ "Hay lesionados"
) )
cor2 = cor1 %>% group_by(Autos=CANT, Lesionados) %>% summarise(cantidad_casos=sum(cantidad))
cor2 = cor2 %>% filter(Autos>0)
totales = cor2 %>% group_by(Autos) %>% summarise(total_lesionados = sum(cantidad_casos))
cor2 = cor2 %>% mutate(total_lesionados = case_when(
  Autos == 1 ~ 338380,
  Autos == 2 ~ 1456929,
  Autos == 3 ~ 102005,
  Autos == 4 ~ 21306,
  Autos == 5 ~ 7830
)) 
cor2 = cor2 %>% mutate(Porcentaje = (cantidad_casos/total_lesionados) * 100)
g17 <- cor2 %>% ggplot() + geom_col(aes(x=Autos, y=Porcentaje, fill=Lesionados)) + theme_minimal() +scale_fill_manual(values = c("#08519C","#3eaef4"))

cor3 = crashes %>% group_by(`NUMBER OF PERSONS KILLED`, CANT) %>% summarise(cantidad=n())
cor3$CANT = as.character(cor3$CANT)
cor3 = cor3 %>% filter(`NUMBER OF PERSONS KILLED` != "NA")
cor3 = cor3 %>% mutate(personas_fallecidas = case_when(
  `NUMBER OF PERSONS KILLED` == 0 ~ "No hubo fallecidos",
  T ~ "Hubo fallecidos"
) )
cor4 = cor3 %>% group_by(Autos=CANT, personas_fallecidas) %>% summarise(cantidad_casos=sum(cantidad))
cor4 = cor4 %>% filter(Autos>0)
totales = cor4 %>% group_by(Autos) %>% summarise(total_fallecidos = sum(cantidad_casos))
cor4 = cor4 %>% mutate(total_fallecidos_2 = case_when(
  Autos == 1 ~ 338372,
  Autos == 2 ~ 1456927,
  Autos == 3 ~ 102005,
  Autos == 4 ~ 21306,
  Autos == 5 ~ 7830
)) 
cor4 = cor4 %>% mutate(Porcentaje = (cantidad_casos/total_fallecidos_2) * 100)
cor5 = cor4 %>% filter(personas_fallecidas == "Hubo fallecidos")
g20 <- cor5 %>% ggplot() + geom_col(aes(x=Autos, y=Porcentaje), fill = "#3eaef4") + theme_minimal()

feriados = crashes %>% mutate(DAY = substr(DATE,start = 9, stop = 10))
feriados = feriados %>% mutate(fue_feriado = case_when(
  MONTH == 1 & DAY == "01" ~ "Feriado",
  MONTH == 6 & DAY == "19" ~ "Feriado",
  MONTH == 7 & DAY == "04" ~ "Feriado",
  MONTH == 11 & DAY == "11" ~ "Feriado",
  MONTH == 12 & DAY == "25" ~ "Feriado",
  T ~ "No feriado"
))  
feriados = feriados %>% group_by(YEAR, MONTH,DAY,fue_feriado) %>% summarise(Cantidad=n())
feriado_si = feriados %>% filter(fue_feriado == "Feriado") 
Cantidad = feriado_si$Cantidad
feriado_no = feriados %>% filter(fue_feriado == "No feriado")
feriado_no = feriado_no$Cantidad
g21 <- plot_ly(y = ~Cantidad, type = "box", name = "Feriados") %>% add_trace(y = ~feriado_no, name = "No feriados" )


crashes_5 = crashes %>% group_by(YEAR,MONTH) %>% summarise(Accidentes=n())
crashes_5 %>% filter(YEAR!=2012, YEAR!=2022)
crashes_5 = crashes_5 %>% mutate(Periodo = case_when(
  YEAR <= 2015 ~ "2013-2015",
  YEAR <= 2018 ~ "2016-2018",
  T ~ "2019-2021"
))

g27 <- ggplot(crashes_5) + geom_smooth(aes(x=MONTH,y=Accidentes, color=Periodo), se=FALSE) + theme_minimal() +
  scale_x_continuous(breaks= seq(from=1, to =12, by=1)) + ggtitle("Cantidad de accidentes de tránsito por período") + scale_color_manual(values = c("#08519C", "#3eaef4", "#b9e7f8"))
g27

crashes_6 = crashes %>%  filter(`NUMBER OF PERSONS KILLED` != 0) %>% group_by(YEAR,MONTH) %>% summarise(Muertes=n())
crashes_6 %>% filter(YEAR!=2012, YEAR!=2022)
crashes_6 = crashes_6 %>% mutate(Periodo = case_when(
  YEAR <= 2015 ~ "2013-2015",
  YEAR <= 2018 ~ "2016-2018",
  T ~ "2019-2021"
))

g28 <- ggplot(crashes_6) + geom_smooth(aes(x=MONTH,y=Muertes, color=Periodo), se=FALSE) + theme_minimal() +
  scale_x_continuous(breaks= seq(from=1, to =12, by=1)) +ggtitle("Cantidad de muertes por período") + scale_color_manual(values = c("#08519C", "#3eaef4", "#b9e7f8"))
g28

crashes_7 = crashes %>%  filter(PINJ != 0) %>% group_by(YEAR,MONTH) %>% summarise(Lesionados=n())
crashes_7 %>% filter(YEAR!=2012, YEAR!=2022)
crashes_7 = crashes_7 %>% mutate(Periodo = case_when(
  YEAR <= 2015 ~ "2013-2015",
  YEAR <= 2018 ~ "2016-2018",
  T ~ "2019-2021"
))

g29 <- ggplot(crashes_7) + geom_smooth(aes(x=MONTH,y=Lesionados, color=Periodo), se=FALSE) + theme_minimal() +
  scale_x_continuous(breaks= seq(from=1, to =12, by=1)) +ggtitle("Cantidad de lesionados por período") + scale_color_manual(values = c("#08519C", "#3eaef4", "#b9e7f8"))
g29

