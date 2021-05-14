#________________________
#AMBIENTE
#________________________

library(readxl)
library(readr)
library(stargazer)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(Jmisc)
library(stringi)

#________________________
#DATOS
#________________________

#CENSO 2020
#Ahora no tienen que descargar nada, todo es automatico :)
temp <- tempfile()
download.file("https://www.inegi.org.mx/contenidos/programas/ccpv/iter/zip/iter2020/iter_nalcsv20.zip",temp)
censo<-read.csv(unz(temp,"ITER_NALCSV20.csv"),encoding= "ASCII")
unlink(temp)

##CENSO 2010
censo_10<-read.csv("http://ri.uaemex.mx/bitstream/handle/20.500.11799/58449/03%20Censo%20de%20poblaci%c3%b3n%20por%20localidades%202010.csv?sequence=2&isAllowed=y")


##PLANEA 
planea_10<-read.csv("ENLACE_sec_2010.csv")
planea_15<-read_xlsx("PLANEA_2015.xlsx")
planea_19<-read_xlsx("PLANEA_2019.xlsx")
#limpieza
planea_15 <- planea_15[-c(1:2),]
planea_19 <- planea_19[-c(1:7),]

#________________________
#PREPROCESAMIENTO CENSO 2010
#________________________


censo_10_localidad<-censo_10 %>% 
  select(ClaveEnt,Entidad,              #EN ESTE SELECT AGREGAMOS LAS VARIABLES QUE NECESITEMOS
         ClaveMun,Municipio,
         Localidad, ClaveLoc,
         pobtot,vivpar_hab,vph_inter,
         vivtot,vph_pc,phog_ind,p15ym_se,
         graproes,pocupada,psinder,tothog,hogjef_f, #COVARIATES primera generacion
         vph_c_elec,p3ym_hli,         #COVARIATES segunda generacion
         p12a14noaf,#total 12-14 no asiste muj
         p_12a14_f,#total 12-14 
         Ambito,
         vph_cel,tam_loc,
         vph_inter,vivtot,vph_pc) %>% 
  mutate(internet_10= vph_inter*100/vivtot,
         pc_10 =vph_pc*100/vivtot,
         pobindig_10=phog_ind*100/pobtot,
         popindig_10_real=p3ym_hli*100/pobtot,
         p15sinesc_10=p15ym_se*100/pobtot,
         promesc_10=graproes*100/pobtot,
         pocup_10=pocupada*100/pobtot,
         psinsalud_10=psinder*100/pobtot,
         jefmujer_10=hogjef_f*100/vivtot,
         cel_10=vph_cel*100/vivtot,
         porc_viv_luz=vph_c_elec*100/vivtot,
         niñas_12_14_esc= (p_12a14_f-p12a14noaf)*100/p_12a14_f)
#OJO: 84,690 MISSING VALUES PROVENIENTES DE LA BASE DE DATOS 

censo_10_localidad <- as.data.frame(t(apply(censo_10_localidad,1,toupper)))



names(censo_10_localidad)[names(censo_10_localidad) == "Entidad"] <- "NOM_ENT"
names(censo_10_localidad)[names(censo_10_localidad) == "Municipio"] <- "NOM_MUN"
names(censo_10_localidad)[names(censo_10_localidad) == "Localidad"] <- "NOM_LOC" 
#________________________
#PREPROCESAMIENTO CENSO 2015
#________________________

#________________________
#PREPROCESAMIENTO CENSO 2020
#________________________


#A) Encontrar acceso a internet (y variables que propuso sam) en censo a nivel localidad
#convierto variables de interes a numéricas
names<-c('VIVTOT','TVIVPAR','VIVPAR_HAB', #localidad
         'VPH_INTER','VPH_PC',#internet y laptop
         'VPH_INTER','VPH_PC','PHOG_IND','P15YM_SE',
         'POBTOT','HOGJEF_F','VPH_CEL','GRAPROES',
         'POCUPADA','PSINDER') #variables sam

for (i in names){
  censo[[i]] <- as.numeric(censo[[i]])
}


#creo la base de datos de interes 
censo_20_localidad<-censo %>% 
  select(ENTIDAD,MUN,LOC,       #claves numéricas de localidad
         NOM_ENT,NOM_MUN,NOM_LOC,
         VPH_INTER,VPH_PC,PHOG_IND,P15YM_SE,
         GRAPROES,POCUPADA,PSINDER,TOTHOG,
         HOGJEF_F,POBTOT,VPH_CEL,
         VIVTOT,TVIVPAR,VIVPAR_HAB,       # vivienda
         VPH_INTER,VPH_PC)   %>%    # internet y laptop       #EN ESTE SELECT AGREGAMOS LAS VARIABLES QUE NECESITEMOS
  mutate(internet_20=VPH_INTER*100/VIVTOT,
         pc_20=VPH_PC*100/VIVTOT,
         pobindig_20=PHOG_IND*100/POBTOT,
         p15sinesc_20=P15YM_SE*100/POBTOT,
         promesc_20=GRAPROES*100/POBTOT,
         pocup_20=POCUPADA*100/POBTOT,
         psinsalud_20=PSINDER*100/POBTOT,
         jefmujer_20=HOGJEF_F*100/VIVTOT,
         cel_20=VPH_CEL*100/VIVTOT
  )
#cambio todo a mayus
censo_20_localidad <- as.data.frame(t(apply(censo_20_localidad,1,toupper)))

# cambio nombres

names(censo_20_localidad)[names(censo_20_localidad) == "Entidad"] <- "NOM_ENT"
names(censo_20_localidad)[names(censo_20_localidad) == "Municipio"] <- "NOM_MUN"
names(censo_20_localidad)[names(censo_20_localidad) == "Localidad"] <- "NOM_LOC"  


#________________________
#PREPOCESAMIENTO: MERGE CENSOS
#________________________

#Preparacion para 2020
censo_20_localidad$ENTIDAD<-as.numeric(censo_20_localidad$ENTIDAD)
censo_20_localidad$MUN<-as.numeric(censo_20_localidad$MUN)
censo_20_localidad$LOC<-as.numeric(censo_20_localidad$LOC)
#Creo indice INEGI
censo_20_localidad<-censo_20_localidad%>%
  mutate(MUN=sprintf("%03d",censo_20_localidad$MUN),
         ENT=sprintf("%02d",censo_20_localidad$ENTIDAD),
         LOC=sprintf("%04d",censo_20_localidad$LOC)) %>% 
  mutate(a=paste(ENT,MUN,sep=""))%>% 
  mutate(INEGI=paste(a,LOC,sep=""))


#Preparacion 2010 (ya viene muy preparado)
censo_10_localidad$ClaveLoc<-as.numeric(censo_10_localidad$ClaveLoc)
#Creo indice INEGI
censo_10_localidad<-censo_10_localidad%>%
  mutate(INEGI=sprintf("%09d",censo_10$ClaveLoc))


#MERGE
censos<-merge(censo_10_localidad,censo_20_localidad, by= "INEGI")

#________________________
#ANALISIS CENSOS
#________________________

names<-c('pc_10','internet_10', #agregar en caso de que no sean numericas
         'internet_20','pc_20') 

for (i in names){
  censos[[i]] <- as.numeric(censos[[i]])
}

summary(censos)

#REALITY CHECK, VIVIMOS EN UN PAIS MUY POBRE O TENEMOS UN PROBLEMA DE MISSING VALUES

#________________________
#PREPROCESAMIENTO PLANTEA 2019
#________________________


#convierto variables de interes a numéricas

names<-c('...23','...24','...25','...26', #calis esp
         '...35','...36','...37','...38') #calis mate
for (i in names){
  planea_19[[i]] <- as.numeric(planea_19[[i]])
}


#cambio nombres
names(planea_19)[names(planea_19) == "P L A N E A    T E R C E R O   D E   S E C U N D A R I A   2 0 1 9"] <- "ENT"

# funcion de moda
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#creo la base de datos de interes

planea_localidad_19<-planea_19 %>% 
  select(...2,...6,...7,ENT,                 
         # son entidad, municipio y loc
         ...23,...24,...25,...26,       # son % de calis de español
         ...35,...36,...37,...38, # son % de calis de mate
         ...10)   %>% #localidad   SE PUEDE MEJORAR UN MONTON CON LA TASA PERO BUENO PONDREMOS LA MODA
  group_by(...2,...6,...7,ENT) %>%
  summarize(esp_1_19 = mean(...23), 
            esp_II_19= mean(...24),
            esp_1II_19 = mean(...25), 
            esp_IV_19= mean(...26),
            mat_1_19 = mean(...35), 
            mat_II_19= mean(...36),
            mat_1II_19 = mean(...37), 
            mat_IV_19= mean(...38),
            marg_10= Mode(...10)) #convierto a mi variable de interes (porcentaje de niños en la mejor mitad)

#cambio todo a mayus
planea_localidad_19 <- as.data.frame(t(apply(planea_localidad_19,1,toupper)))


#cambio nombres
names(planea_localidad_19)[names(planea_localidad_19) == "...2"] <- "NOM_ENT"
names(planea_localidad_19)[names(planea_localidad_19) == "...6"] <- "NOM_MUN"
names(planea_localidad_19)[names(planea_localidad_19) == "...7"] <- "NOM_LOC"


#________________________
#PREPROCESAMIENTO PLANEA 2015 #no la usaremos
#________________________


#convierto variables de interes a numéricas

names<-c('...24','...25','...26','...27', #calis esp
         '...40','...41','...42','...43') #calis mate
for (i in names){
  planea_15[[i]] <- as.numeric(planea_15[[i]])
}


#creo la base de datos de interes

planea_localidad_15<-planea_15 %>% 
  select(ENT,`NOMBRE DEL MUNICIPIO`,`NOMBRE DE LA LOCALIDAD`,                  # son entidad, municipio y loc
         ...24,...25,...26,...27,       # son % de calis de español
         ...40,...41,...42,...43,
         `GRADO DE MARGINACIÓN`)   %>% # son % de calis de mate
  group_by(ENT,`NOMBRE DEL MUNICIPIO`,`NOMBRE DE LA LOCALIDAD`) %>%
  summarize(esp_1_15 = mean(...24), 
            esp_II_15= mean(...25),
            esp_1II_15 = mean(...26), 
            esp_IV_15= mean(...27),
            mat_1_15 = mean(...40), 
            mat_II_15= mean(...41),
            mat_1II_15 = mean(...42), 
            mat_IV_15= mean(...43),
            marg_15=Mode(`GRADO DE MARGINACIÓN`)) 

#cambio todo a mayus
planea_localidad_15 <- as.data.frame(t(apply(planea_localidad_15,1,toupper)))


#cambio nombres

names(planea_localidad_15)[names(planea_localidad_15) == "NOMBRE DEL MUNICIPIO"] <- "NOM_MUN"
names(planea_localidad_15)[names(planea_localidad_15) == "NOMBRE DE LA LOCALIDAD"] <- "NOM_LOC"

#IMPUTACION DE NOMBRE DE ENTIDAD
claves<-planea_10 %>% select(CVE_ENT,NOM_ENT) %>% 
  group_by(NOM_ENT) %>% 
  summarise(CVE_ENT = mean(CVE_ENT))%>% 
  mutate(ENT=sprintf("%02d",CVE_ENT)) %>% 
  select(ENT,NOM_ENT)
planea_localidad_15<-merge(planea_localidad_15,claves, by="ENT")  #este merge tarda un poco, paciencia                  


#________________________
#PREPROCESAMIENTO PLANTEA 2010
#________________________


planea_10<-planea_10%>%
  mutate(MUN=sprintf("%03d",planea_10$CVE_MUN),
         ENT=sprintf("%02d",planea_10$CVE_ENT),
         LOC=sprintf("%04d",planea_10$CVE_LOC)) %>% 
  mutate(a=paste(ENT,MUN,sep=""))%>% 
  mutate(INEGI=paste(a,LOC,sep=""))


planea_localidad_10<-planea_10 %>% 
  select(NOM_ENT,NOM_MUN,NOM_LOC,
         MUN, ENT, LOC, INEGI,                # son entidad, municipio y loc
         POR_N1_ESP_3,POR_N2_ESP_3,POR_N3_ESP_3,POR_N4_ESP_3,       # son % de calis de español
         POR_N1_MAT_3,POR_N2_MAT_3,POR_N3_MAT_3,POR_N4_MAT_3,  # son % de calis de mate
         TIPO)   %>% # tipo de escuela
  group_by(NOM_ENT,NOM_MUN,NOM_LOC,
           MUN, ENT, LOC, INEGI) %>%
  summarize(esp_1_10 = mean(POR_N1_ESP_3), 
            esp_II_10= mean(POR_N2_ESP_3),
            esp_1II_10 = mean(POR_N3_ESP_3), 
            esp_IV_10= mean(POR_N4_ESP_3),
            mat_1_10 = mean(POR_N1_MAT_3), 
            mat_II_10= mean(POR_N2_MAT_3),
            mat_1II_10 = mean(POR_N3_MAT_3), 
            mat_IV_10= mean(POR_N4_MAT_3),
            tipo=Mode(TIPO)) #convierto a mi variable de interes (porcentaje de niños en la mejor mitad)

#cambio todo a mayus
planea_localidad_10 <- as.data.frame(t(apply(planea_localidad_10,1,toupper)))

#________________________
#correct encoding
#________________________

#esto simplemene elimina todos los caracteres especiales
# se intentaron diferentes metodos (suplantacion, recodificacion) y este es el que rescata más observaciones

planea_localidad_10$NOM_ENT<-stri_enc_toascii(planea_localidad_10$NOM_ENT)
planea_localidad_10$NOM_MUN<-stri_enc_toascii(planea_localidad_10$NOM_MUN)
planea_localidad_10$NOM_LOC<-stri_enc_toascii(planea_localidad_10$NOM_LOC)

planea_localidad_15$NOM_ENT<-stri_enc_toascii(planea_localidad_15$NOM_ENT)
planea_localidad_15$NOM_MUN<-stri_enc_toascii(planea_localidad_15$NOM_MUN)
planea_localidad_15$NOM_LOC<-stri_enc_toascii(planea_localidad_15$NOM_LOC)

planea_localidad_19$NOM_ENT<-stri_enc_toascii(planea_localidad_19$NOM_ENT)
planea_localidad_19$NOM_MUN<-stri_enc_toascii(planea_localidad_19$NOM_MUN)
planea_localidad_19$NOM_LOC<-stri_enc_toascii(planea_localidad_19$NOM_LOC)


#9481 #n del primer metodo
#10394  #n del segundo metodo
#12199  #n del tercer metodo
#12234 #lo maximo que he logrado recuperar
planeas_del_10_al_15<-as.data.frame(merge(planea_localidad_10,planea_localidad_15, by= c("ENT","NOM_MUN","NOM_LOC"))) 
#vemos un patron de municipios, son datos diferentes
#esta base solo tiene 10k obs mientras que la de abajo mantiene 14k, quiere decir que planea 15 es una base problematica 


planeas_del_10_al_19<-as.data.frame(merge(planea_localidad_10,planea_localidad_19, by= c("ENT","NOM_MUN","NOM_LOC"))) 
#summary(factor(planeas_del_10_al_19$ENT)) # tenemos 14,522 de 20k localidades que viven a traves de 2010 a 2019

#________________________
#MERGE GLOBAL= FINAL DATA
#________________________

#cambio a numerico para poder hacer mutates
planeas_del_10_al_19$mat_1_10<-as.numeric(planeas_del_10_al_19$mat_1_10)
planeas_del_10_al_19$mat_1_19<-as.numeric(planeas_del_10_al_19$mat_1_19)
planeas_del_10_al_19$mat_IV_10<-as.numeric(planeas_del_10_al_19$mat_IV_10)
planeas_del_10_al_19$mat_IV_19<-as.numeric(planeas_del_10_al_19$mat_IV_19)

planeas_del_10_al_19$esp_1_10<-as.numeric(planeas_del_10_al_19$esp_1_10)
planeas_del_10_al_19$esp_1_19<-as.numeric(planeas_del_10_al_19$esp_1_19)
planeas_del_10_al_19$esp_IV_10<-as.numeric(planeas_del_10_al_19$esp_IV_10)
planeas_del_10_al_19$esp_IV_19<-as.numeric(planeas_del_10_al_19$esp_IV_19)

#creo el data final (por mientras)

data<-as.data.frame(merge(censos,planeas_del_10_al_19, by= c("INEGI"))) %>% 
  na.omit()  %>% mutate(cambio_internet=internet_20-internet_10,
                        cambio_pc=pc_20-pc_10,
                        cambio_esp_bottom=esp_1_19-esp_1_10,
                        cambio_esp_top=esp_IV_19-esp_IV_10,
                        cambio_mat_bottom=mat_1_19-mat_1_10,
                        cambio_mat_top=mat_IV_19-mat_IV_10,
                        inter_pc_internet= cambio_pc*cambio_internet)

summary(data$cambio_internet) #notamos que el cambio de internet estuvo cañon
summary(data$cambio_pc)  #notamos que el cambio de pc no tanto

hist(data$inter_pc_internet)
#________________________
#GUARDO BASE FINAL
#________________________

write_csv(data,"data.csv")
summary(data)

