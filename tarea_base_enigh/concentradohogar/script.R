library(dplyr)
library(readr)

data<-read.csv("concentradohogar.csv")
#primero filtro mi base de datos normal y selecciono mis dos productos (vivienda y leche)
#asumo que el precio de la leche es 20 para todo el país, entonces puedo inferir la cantidad
#asumo que la cantidad de renta es una cada trimestre, entonces puedo encontrar los precios (imptando la media municipal)
data<-data%>% select(folioviv,foliohog,ubica_geo,upm,factor,vivienda,leche)%>%
  mutate(precio_leche=20, cantidad_leche=leche/20,cantidad_vivienda=1,precio_vivienda=ifelse(vivienda>0,vivienda,NA))

#creo una base de datos auxiliar con información del precio promedio de la vivienda en cada municipio
data_imp<- data%>% 
  select(ubica_geo,upm,factor,precio_vivienda)%>%
  group_by(ubica_geo)%>%summarise(precio_vivienda_promedio = mean(precio_vivienda,na.rm = T))

#imputo los valores
data<-merge(data,data_imp, by = "ubica_geo")
data<-total%>%mutate(precio_vivienda=ifelse(vivienda>0,vivienda,precio_vivienda_promedio))
data$precio_vivienda[is.na(data$precio_vivienda)] <- mean(data$precio_vivienda,na.rm = T)

write.csv("data.csv")
