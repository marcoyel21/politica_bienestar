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

data_2<- data_2%>% 
  group_by(countrycode) %>%
  mutate(growth = c(NA,diff(rgdpe))/lag(rgdpe, 1))

#data_2 es la base final


#Parte III: modelaje

install.packages("plm")
library(plm)

# Modelo 2: Efectos fijos



efectos_fijos_tiempo <- plm( growth  ~ gini, 
                      data = data_2,
                      index = c("countrycode", "year"), 
                      model = "within",
                      effect = "time")

efectos_fijos_pais <- plm( growth  ~ gini, 
                      data = data_2,
                      index = c("countrycode", "year"), 
                      model = "within",
                      effect = "individual")

efectos_fijos_ambos <- plm( growth  ~ gini, 
                           data = data_2,
                           index = c("countrycode", "year"), 
                           model = "within",
                           effect = "twoways")
summary(efectos_fijos_tiempo)
summary(efectos_fijos_pais)
summary(efectos_fijos_ambos)


# De entrada en el modelo más sencillo de efectos fijos ya sabemos que el gini está correlacionado de manera negativa con el crecimiento.
# Aunque la r^2 es demasiado baja, el coeficiente es negativo y es estadisticamente significativo a más del 99%

#En cuanto a efectos fijos, el modelo que contempla efectos fijos para cada país tuvo la R^2 más alta; por el contrario, el modelo que contemplo solo efecto fijo para el tiempo tuvo la más baja

efectos_fijos_region <- plm( growth  ~ gini+region, 
                            data = data_2,
                            index = c("countrycode", "year"), 
                            model = "within",
                            effect = "individual")
summary(efectos_fijos_region)

#Es interesante que si agregamos la variable de region al modelo de efectos fijos que ya contemplaba al país individual, en realidad no cambia NADA del modelo.

efectos_fijos_region_2 <- plm( growth  ~ gini+region, 
                             data = data_2,
                             index = c("countrycode", "year"), 
                             model = "within",
                             effect = "time")
summary(efectos_fijos_region_2)

# Finalmente, si solo dejamos un efecto fijo para el tiempo y usamos region en lugar de un efecto fijo para cada país, obtenemos un modelo con la R^2 más alta (aunque sigue siendo diminuta)



# Modelo 3: Efectos aleatorios

