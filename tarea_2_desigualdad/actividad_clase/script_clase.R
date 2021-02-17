library(readstata13)
library(readxl)
library(readr)
library(dplyr)

#Bases de datos del Penn
data<-read.dta13("cigarretdata.dta")


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

#CONCLUSION MODELOS EFECTOS FIJOS:
# Definitivamente un efecto fijo por el tiempo no es necesario; en cambio podemos optar por un efecto fijo para cada país. Asimismo la variable región otorga mejor r^2 que el efecto fijo de pais
# En resumen, la variable que debe acompañar a gini es region sin efectos fijos para tiempo ni para país-

# Modelo 3: Efectos aleatorios

