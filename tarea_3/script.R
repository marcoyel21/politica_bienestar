library(readstata13)
library(readxl)

#Bases de datos del Penn
wpt<-read.dta13("https://www.rug.nl/ggdc/docs/pwt100.dta")

#Base de datos más completa de ginis, solo que es un desmadre
wiid<-read_excel("wiid_2020.xlsx")

#Hola equipo, aún no se como le haremos, es un reto encontrar datos de desigualdad completos por año. 
# Aun no se como trabajar con estos datos panel, si se les ocurre algo bienvenida la idea


