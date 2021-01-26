library(dplyr)
library(readr)
library(splitstackshape)

#importo los datos
data<-read_csv("pobreza_18.csv")
#creo una variable del grupo 3 (mi objetivo)
data<-data%>%mutate(y_3=ifelse(data$cuadrantes==3,1,0))

#filtro de acuerdo a lo que necesito
data$sa_dir<-NULL
data<-data%>% select(factor,y_3,cuadrantes,tam_loc,rururb,ic_rezedu,
                     ic_asalud,ic_segsoc,jef_ss,
                     s_salud,isb_combus,ins_ali,ictpc,
                     carencias,ic_cv,ic_ali,plb,ss_dir,ic_sbv,ing_mon)
data<-na.omit(data)

data<-expandRows(data,'factor')

sample<-data[sample(nrow(data), 200000), ]

write_csv(sample,"muestra_idd.csv")
