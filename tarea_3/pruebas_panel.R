library(readstata13)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(foreign)
library(gplots)
library(plm)
library(memisc)
library(stargazer)


#Bases de datos de Penn
wpt<-read.dta13("https://www.rug.nl/ggdc/docs/pwt100.dta")

#Base de datos del banco mundial
wb_gini<-read.csv("wb_gini.csv")

wb_gini<-read.csv("https://raw.githubusercontent.com/marcoyel21/politica_bienestar/master/tarea_3/wb_gini.csv")

names(wb_gini)[length(names(wb_gini))]<-"gini" 

filter<-as.data.frame(summary(factor(wb_gini$Code)))
a<-row.names(filter)
wpt_2<-wpt %>% select(country,countrycode,year,rgdpe,rgdpo,pop,labsh,avh,hc,csh_i,csh_g)%>% filter(countrycode %in% a)%>% filter(year > 1989)%>% filter(year < 2016)
clean<-read.csv("clean_2.csv")
data_2 <- left_join( wpt_2, clean,
                     by = c("countrycode"="countrycode", "year"="year"))
# Creo una variable de creimiento del pib per capita
data_2<- data_2%>% mutate(y=rgdpe/pop)%>% 
  group_by(countrycode) %>%
  mutate(growth_pc = c(NA,diff(y))/lag(y, 1))%>% 
  mutate(growth = c(NA,diff(rgdpe))/lag(rgdpe, 1))
View(data_2)
#data_2 es la base final

#Heterogeneidad

plotmeans(growth_pc~countrycode,main="Heterogeneidad en países",data=data_2)
plotmeans(growth~year,main="Heterogeneidad en el tiempo",data=data_2)

##Regresión OLS: homogeneidad en el tiempo

ols1 <-lm(growth_pc ~ gini, data=data_2)
summary(ols1)

A <- plm( growth_pc  ~ gini, 
          data = data_2,
          index = c("countrycode", "year"), 
          model = "pooling")

#Gini no significativo, R2 muy baja

##EF con Within/Least Squares Dummy Variable Model

ef.lsdv <-lm(growth ~ gini + factor(country) - 1, data=data_2)
summary(ef.lsdv)

#R2 = .288, gini significativo

##
gg <- ggplot(data_22, aes(x=gini, y=growth, col=country)) + 
  geom_smooth(method="lm", size=1, se=FALSE) 
plot(gg)


#Duda. por qué sale el coeficiente igual pero R2 diferente pero con plm no...

B <- plm( growth_pc  ~ gini, 
          data = data_2,
          index = c("countrycode", "year"), 
          model = "within",
          effect="individual")

## OLS vs Efectos fijos--------

mtable(ols1,ef.lsdv)

#Interceptos de cada país

fixef(B)

#Prueba FE vs OLS

pFtest(B,A)

#Donde Ho es que OLS mejor que FE

#ya que el p-value es muy pequeño y rechazamos, efectos fijos es mejor


#Efectos aleatorios/GLS

B.aleat <- plm(growth_pc ~ gini, data=data_2, index=c("country", "year"), model="random")
summary(B.aleat)

#Los coeficientes tienen efectos within y between. 
#La prueba F (Chisq) tiene un valor-p de .21; el coeficiente no es distinto de cero

#Prueba Hausman: EF vs EA-------

phtest (B,B.aleat)

#P-value es muy bajo: mejor usar efectos fijos.

#Tenemos que Efectos fijos es mejor que aleatorios y que pooled.

#Prueba Breusch-Pagan: EA vs pooled-----------

#Ho: varianzas de país a país es cero, o no diferencia significativa entre países -> no efecto panel

plmtest(A, type=c("bp"))


#Prueba Breusch Pagan: EF en el tiempo ------
#Ahora vamos a hacer una prueba para efectos fijos en el tiempo

fixed.time<-plm(growth_pc ~ gini + factor(year), data=data_2, index=c("country",
                                                                         "year"), model="within")

summary(fixed.time)

#Notamos que gini es significativa al 1%, al igual que la prueba, con una R2 de .15
#Hacemos dos pruebas. Ho es que "no time-fixed effects needed"

#Prueba Breushc- Pagan

plmtest(B, c("time"), type=("bp"))

pFtest(fixed.time,B)

#Rechazamos al 1% -> Ho es que no hay efectos de tiempo significativos


## Otras pruebas ----------
##Prueba de dependencia transversal
#Hacemos una prueba Breusch Pagan al modelo EF donde Ho es que residuales entre países no están correlacionados. 

pcdtest(B, test = c("lm"))

#Rechazamos que no hay dependencia, sí la hay. Esto puede sesgar a los estimadores (?)

## Test de correlación serial Breusch-Godfrey/Wooldridge

pbgtest(B)

#Rechazamos al 1%: sí hay correlación serial

## Prueba Breusch Pagan para heteroscedasticidad

library(lmtest)
bptest(growth_pc ~ gini + factor(country), data = data_2, studentize=F)

#Ho: homoscedasticidad
#Rechazamos al 1% -> presencia de heteroscedasticidad

##Controlamos la heteroscedasticidad en efectos fijos. Se sugiere el estimador "Arellano", ya que hay presencia de heteroscedasticidad
#y correlación serial.

coeftest(B, vcovHC(B, method = "arellano"))

#Este es un coeficiente consistente con heteroscedasticidad ?


#################### Replica para two-step within-----------

C <- plm( growth_pc  ~ gini, 
          data = data_2,
          index = c("countrycode", "year"), 
          model = "within",effect="twoways")

#Prueba FE vs OLS

pFtest(C,A)

#Prueba Hausman: EF vs EA-------

phtest (C,B.aleat)

#P-value es muy bajo: mejor usar efectos fijos.

#Tenemos que Efectos fijos es mejor que aleatorios y que pooled.


#Prueba Breusch Pagan: EF en el tiempo ------
#Ahora vamos a hacer una prueba para efectos fijos en el tiempo

fixed.time<-plm(growth_pc ~ gini + factor(year), data=data_2, index=c("country",
                                                                      "year"), model="within",effect="twoways")

#Prueba Breusch- Pagan (Lagrange Multiplier test)

plmtest(C, c("time"), type=("bp"))


#Rechazamos al 1% -> Ho es que no hay efectos de tiempo significativos


## Otras pruebas ----------
##Prueba de dependencia transversal
#Hacemos una prueba Breusch Pagan al modelo EF donde Ho es que residuales entre países no están correlacionados. 

pcdtest(C, test = c("lm"))

#Rechazamos que no hay dependencia, sí la hay. Esto puede sesgar a los estimadores (?)

## Test de correlación serial Breusch-Godfrey/Wooldridge

pbgtest(C)

#Rechazamos al 1%: sí hay correlación serial

## Prueba Breusch Pagan para heteroscedasticidad

library(lmtest)
bptest(growth_pc ~ gini + factor(country), data = data_2, studentize=F)

#Ho: homoscedasticidad
#Rechazamos al 1% -> presencia de heteroscedasticidad

##Controlamos la heteroscedasticidad en efectos fijos. Se sugiere el estimador "Arellano", ya que hay presencia de heteroscedasticidad
#y correlación serial.

coeftest(C, vcovHC(C, method = "arellano"))

#Este es un coeficiente consistente con heteroscedasticidad ?


stargazer(A,B,C, title="Results",type="latex",column.labels = c("Pooled","Efectos Fijos (individuales)", "Efectos fijos (twoways)"), out.header=FALSE,
          column.sep.width = "3pt", 
          font.size = "small")


##### MUNDLAK test: RE vs FE-------

data_2 %>%
  group_by(country, year) %>%
  summarise(mean(gini))

#correr regresión con la variable de la media con RE

mundlak <- plm(growth_pc ~ gini, data=data_2, index=c("country", "year"), model="random")

pFtest(B,mundlak)

#Rechazamos que efectos aleatorios sea mejor que fijos















