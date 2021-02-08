library(mice)

data<- read.csv("C:/Users/maira bravo/Documents/gastospersona2.csv")

#df sin categóricas ycon info faltante

data.mis <- subset(data, select = -c(frec_rem,tipo_gasto,mes_dia,
                                     forma_pag1,forma_pag2,forma_pag3))

#pattern de NA

md.pattern(data.mis)


### imputacion con Predictive mean matching y MICE. m es número de imputaciones, método es PMM 

imputed_Data <- mice(data.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)


#ver info de uno de los sets. fueron 5
completeData <- complete(imputed_Data,1)



















