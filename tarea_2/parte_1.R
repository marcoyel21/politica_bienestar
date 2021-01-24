
#Parte I
library(readr)
library(dplyr)
library(splitstackshape)
library(corrplot)

#importo los datos
data<-read_csv("pobreza_18.csv")
#creo una variable del grupo 3 (mi objetivo)
data<-data%>%mutate(y_3=ifelse(data$cuadrantes==3,1,0))

#Parte II 

#Limpieza

# De todas estas columnas con Nas solo puedo salvar sa_dir
colSums(is.na(data))
data$inas_esc<-NULL
data$pea<-NULL
data$pam<-NULL
data$hli<-NULL
data$tot_iaad<-NULL
data$tot_iamen<-NULL
data$niv_ed<-NULL
data$ent<-NULL  #Entidad no muestra relación con nuestra variable objetivo
summary(factor(data$sa_dir))
data$sa_dir[is.na(data$sa_dir)] <- 0
data<-na.omit(data)

#Analisis de correlaciones (sin expansion)
cor_data<-as.data.frame(cor(data))
cor_data_filtered<-cor_data %>%select(y_3)
#De aqui tomo las variables más correlacionadas positiva y negativamente
# Y paso a la revision de literatura para evaluar cuales son las mejores variables

# Parte III
#Propongo el siguiente modelo
data_filtered<-data%>%select(y_3,factor,
                             plb, s_salud, jef_ss,ss_dir,
                              rururb,ic_sbv,tam_loc ,ic_rezedu, 
                               ic_asalud ,ic_cv,isb_combus,ic_ali)
data_filtered<-expandRows(data_filtered,"factor")

#Como la base de datos es muy grande, propongo una muestra aleatoria de tamaño n para "jugar"
data_filtered_s<-data_filtered[sample(nrow(data_filtered), 50000), ]


logitMod <- glm(y_3 ~ ., data=data_filtered_s, family=binomial(link="logit"))

#Creo una función para generar matriz de confusión
confusion.glm <- function(data, model) {
  prediction <- ifelse(predict(model, data, type='response') > 0.5, TRUE, FALSE)
  confusion  <- table(prediction, as.logical(model$y))
  confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
  confusion  <- as.data.frame(confusion)
  names(confusion) <- c('FALSE', 'TRUE', 'class.error')
  confusion
}

#Genero la matriz de confusión
confusion.glm(data_filtered_s,logitMod)


#EXTRA
#Analisis breve 

#correlograma
corrplot(cor(data_filtered_s), method="circle")


