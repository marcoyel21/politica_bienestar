library(readstata13)
library(readxl)
library(readr)
library(dplyr)

#Bases de datos del Penn
wpt<-read.dta13("https://www.rug.nl/ggdc/docs/pwt100.dta")

#Base de datos del banco mundial
wb_gini<-read.csv("wb_gini.csv")
names(wb_gini)[length(names(wb_gini))]<-"gini" 

# Parte II: Limpieza de datos
# En esta parte voy a crear una base de datos que tenga informacion completa de 1990 a 2015. (los años con información más completa)
# Para eso voy a filtrar por los países de los cuáles al menos cuento con 5 observaciones de gini,
filter<-as.data.frame(summary(factor(wb_gini$Code)))
a<-row.names(filter) #Este es el vector de países con más observaciones
wb_gini_2<-wb_gini%>% filter(Code %in% a)%>% filter(Year > 1989)
wpt_2<-wpt %>% select(country,countrycode,year,rgdpe,rgdpo,pop)%>% filter(countrycode %in% a)%>% filter(year > 1989)%>% filter(year < 2016)
# Junto los datasets
#data es los datos sin la imputación
data <- left_join( wpt_2, wb_gini_2,
                          by = c("countrycode" = "Code", "year"="Year"))

# IMPUTACION
# Realizo la imputación en excel (solamente agrego el promedio del gini entre años para los años faltantes):
write.csv(data,"data.csv")
#Ahora junto la base final, 
clean<-read.csv("clean.csv")
data_2 <- left_join( wpt_2, clean,
                   by = c("countrycode"="countrycode", "year"="year"))  #Esta es la base de datos final

#data_2 es la base final


#Parte III: modelaje


