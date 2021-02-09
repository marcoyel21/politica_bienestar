library(mice)
library(writexl)
library(sjmisc)
library(tidyverse)
library(dplyr)

data<- read.csv("C:/Users/maira bravo/Documents/gastospersona2.csv")
#df sin categóricas y con info faltante
attach(data)
####Servicios de comida

data.mis1<- subset(data, data[,21] == "A243" | data[,21] =="A244"|data[,21] =="A245"| data[,21] =="A246"| data[,21] =="A247"
                   ,select = -c(folioviv,clave,foliohog,numren,frec_rem,tipo_gasto,mes_dia,forma_pag1,forma_pag2,forma_pag3))

#pattern de NA

#md.pattern(data.mis1)

### imputacion con Predictive mean matching y MICE. m es número de imputaciones, método es PMM 

imputed_Data <- mice(data.mis1, m=2, maxit = 5, method = 'pmm', seed = 500)
#summary(imputed_Data)


#ver info de uno de los sets. fueron 5
completeData <- complete(imputed_Data,1)

#luego hacemos pool para que escoja "the most likely imputed value"
pool.comidas<-merge_imputations(
  data.mis1,
  imputed_Data,
  ori = NULL,
  summary = c("none", "dens", "hist", "sd"),
  filter = NULL
)

##### transporte público

data.mis2<- subset(data, data[,21] == "B001" | data[,21] =="B002"| data[,21] =="B003"| data[,21] =="B004"| data[,21] =="B005"| data[,21] =="B006"| data[,21] =="B007"
                   ,select = -c(folioviv,clave,foliohog,numren,frec_rem,tipo_gasto,mes_dia,forma_pag1,forma_pag2,forma_pag3))

imputed_Data2 <- mice(data.mis2, m=2, maxit = 3, method = 'pmm', seed = 500)

pool.transpp<-merge_imputations(
  data.mis2,
  imputed_Data2,
  ori = NULL,
  summary = c("none", "dens", "hist", "sd"),
  filter = NULL
)

### resto de los bienes

data.mis3<-data %>%  filter(clave!="B001")%>%  filter(clave!="B002")%>%  filter(clave!="B003")%>%  filter(clave!="B004")%>%  filter(clave!="B005")%>%  filter(clave!="B006")%>%  filter(clave!="B007")%>%  filter(clave!="A243")%>%  filter(clave!="A244")%>%  filter(clave!="A245")%>%  filter(clave!="A246")%>%  filter(clave!="A247") %>%
  select(-c(folioviv,clave,foliohog,numren,frec_rem,tipo_gasto,mes_dia,forma_pag1,forma_pag2,forma_pag3))


imputed_Data3 <- mice(data.mis3, m=2, maxit = 5, method = 'pmm', seed = 500)

pool.nresto<-merge_imputations(
  data.mis3,
  imputed_Data3,
  ori = NULL,
  summary = c("none", "dens", "hist", "sd"),
  filter = NULL
)

### Merge
#hacer identificadores

data.mis1<- tibble::rowid_to_column(data.mis1, "ID")
pool.comidas<- tibble::rowid_to_column(pool.comidas, "ID")
data.mis2<- tibble::rowid_to_column(data.mis2, "ID")
pool.transpp<- tibble::rowid_to_column(pool.transpp, "ID")
data.mis3<- tibble::rowid_to_column(data.mis3, "ID")
pool.nresto<- tibble::rowid_to_column(pool.nresto, "ID")


##
df.comida<-merge(data.mis1,pool.comidas,by="ID")
df.transp<-merge(data.mis2,pool.transpp,by="ID")
df.resto<-merge(data.mis3,pool.nresto,by="ID")
View(df.resto)

###agregar columna folioviv. están en orden, ya lo verifiqué
#para comida

data.1<-subset(data, data[,21] == "A243" |data[,21] == "A244" | data[,21] =="A245"| data[,21] =="A246"| data[,21] =="A247"
               ,select = -c(foliohog,frec_rem,tipo_gasto,mes_dia,forma_pag1,forma_pag2,forma_pag3))

df.comida$folioviv=data.1$folioviv
df.comida$clave=data.1$clave
df.comida$numren=data.1$numren

#para transporte publico

data.2<- subset(data, data[,21] == "B001" | data[,21] =="B002"| data[,21] =="B003"| data[,21] =="B004"| data[,21] =="B005"| data[,21] =="B006"| data[,21] =="B007"
                ,select = -c(foliohog,frec_rem,tipo_gasto,mes_dia,forma_pag1,forma_pag2,forma_pag3))

#este sí
df.transp$folioviv=data.2$folioviv
df.transp$clave=data.2$clave
df.transp$numren=data.2$numren

#para el resto de los bienes

data.3<-data %>%  filter(clave!="B001")%>%  filter(clave!="B002")%>%  filter(clave!="B003")%>%  filter(clave!="B004")%>%  filter(clave!="B005")%>%  filter(clave!="B006")%>%  filter(clave!="B007")%>%  filter(clave!="A243")%>%  filter(clave!="A244")%>%  filter(clave!="A245")%>%  filter(clave!="A246")%>%  filter(clave!="A247") %>%
  select(-c(foliohog,frec_rem,tipo_gasto,mes_dia,forma_pag1,forma_pag2,forma_pag3))

df.resto$folioviv=data.3$folioviv
df.resto$clave=data.3$clave
df.resto$numren=data.3$numren
## solo pegar precios
data.1$precios_imp=df.comida$precios.y
data.2$precios_imp=df.transp$precios.y
data.3$precios_imp=df.resto$precios.y

#bind imputacion de comida y transporte pub (bienes 1 y 2)

com_transp<-rbind(data.1,data.2)

#ahora bind de bienes 1 y 2 con 3 (resto)

base_completa_imp<-rbind(com_transp,data.3)
View(base_completa_imp)

write_xlsx(base_completa_imp,"C:/Users/maira bravo/Downloads/enighfinal45.xlsx")


