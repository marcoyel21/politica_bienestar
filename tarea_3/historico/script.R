library(readstata13)
library(readxl)
library(readr)
library(dplyr)
library(plm)

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
wpt_2<-wpt %>% select(country,countrycode,year,rgdpe,rgdpo,pop,labsh,avh,hc,csh_i,csh_g)%>% filter(countrycode %in% a)%>% filter(year > 1989)%>% filter(year < 2016)
# Junto los datasets
#data es los datos sin la imputación
data <- left_join( wpt_2, wb_gini_2,
                          by = c("countrycode" = "Code", "year"="Year"))

# IMPUTACION
# Realizo la imputación en excel (solamente agrego el promedio del gini entre años para los años faltantes):
write.csv(data,"data.csv")
#Ahora junto la base final, 
clean<-read.csv("clean.csv")
data_2 <- left_join( wpt_2, clean_2,
                   by = c("countrycode"="countrycode", "year"="year"))  #Esta es la base de datos final

data_2<- data_2%>% 
  group_by(countrycode) %>%
  mutate(growth = c(NA,diff(rgdpe))/lag(rgdpe, 1))

#data_2 es la base final


#Parte III: modelaje

# Modelo 2: Sencillo

pooled <- plm( growth  ~ palma_ratio, 
                             data = data_2,
                             index = c("countrycode", "year"), 
                             model = "pooling")

efectos_fijos_individual <- plm( growth  ~ palma_ratio, 
                      data = data_2,
                      index = c("countrycode", "year"), 
                      model = "within",
                      effect = "individual")

efectos_fijos_two_ways <- plm( growth  ~ palma_ratio, 
                           data = data_2,
                           index = c("countrycode", "year"), 
                           model = "within",
                           effect = "twoways")


summary(pooled)
efectos_fijos_individual
efectos_fijos_two_ways


# Modelos más region


pooled <- plm( growth  ~ gini+region, 
               data = data_2,
               index = c("countrycode", "year"), 
               model = "pooling")

efectos_fijos_individual <- plm( growth  ~ gini+region, 
                                 data = data_2,
                                 index = c("countrycode", "year"), 
                                 model = "within",
                                 effect = "individual")

efectos_fijos_two_ways <- plm( growth  ~ gini+region, 
                               data = data_2,
                               index = c("countrycode", "year"), 
                               model = "within",
                               effect = "twoways")

pooled
efectos_fijos_individual
efectos_fijos_two_ways




# Modelos más csh_i


pooled <- plm( growth  ~ gini+csh_i, 
               data = data_2,
               index = c("countrycode", "year"), 
               model = "pooling")

efectos_fijos_individual <- plm( growth  ~ gini+csh_i, 
                                 data = data_2,
                                 index = c("countrycode", "year"), 
                                 model = "within",
                                 effect = "individual")

efectos_fijos_two_ways <- plm( growth  ~ gini+csh_i, 
                               data = data_2,
                               index = c("countrycode", "year"), 
                               model = "within",
                               effect = "twoways")

pooled
efectos_fijos_individual
efectos_fijos_two_ways

# Modelos más labsh


pooled <- plm( growth  ~ gini+labsh, 
               data = data_2,
               index = c("countrycode", "year"), 
               model = "pooling")

efectos_fijos_individual <- plm( growth  ~ gini+labsh, 
                                 data = data_2,
                                 index = c("countrycode", "year"), 
                                 model = "within",
                                 effect = "individual")

efectos_fijos_two_ways <- plm( growth  ~ gini+labsh, 
                               data = data_2,
                               index = c("countrycode", "year"), 
                               model = "within",
                               effect = "twoways")
pooled
efectos_fijos_individual
efectos_fijos_two_ways

# Modelos más csh_g


pooled <- plm( growth  ~ gini+csh_g, 
               data = data_2,
               index = c("countrycode", "year"), 
               model = "pooling")

efectos_fijos_individual <- plm( growth  ~ gini+csh_g, 
                                 data = data_2,
                                 index = c("countrycode", "year"), 
                                 model = "within",
                                 effect = "individual")

efectos_fijos_two_ways <- plm( growth  ~ gini+csh_g, 
                               data = data_2,
                               index = c("countrycode", "year"), 
                               model = "within",
                               effect = "twoways")


a<-read_xlsx("clean_penn_unu.xlsx")
summary(factor(a$income_group))
a<-a%>% mutate(inc_high= ifelse(income_group=="high",1,0),inc_low=ifelse(income_group=="low",1,0),inc_med=ifelse(income_group=="lower_middle"|income_group=="upper_middle",1,0))
        
        
 # NUeva
 
 
data_2 <- left_join( wpt_2, a,
                      by = c("countrycode"="countrycode", "year"="year"))  #Esta es la base de datos final
 
data_2<- data_2%>% 
   group_by(countrycode) %>%
   mutate(growth = c(NA,diff(rgdpe))/lag(rgdpe, 1))
 
 

pooled <- plm( growth  ~ gini+I(gini*inc_high)+I(gini*inc_low), 
               data = data_2,
               index = c("countrycode", "year"), 
               model = "pooling")

efectos_fijos_individual <- plm( growth  ~ gini+I(gini*inc_med), 
                                 data = data_2,
                                 index = c("countrycode", "year"), 
                                 model = "within",
                                 effect = "individual")

efectos_fijos_two_ways <- plm( growth  ~ gini+I(gini*inc_med), 
                               data = data_2,
                               index = c("countrycode", "year"), 
                               model = "within",
                               effect = "twoways")

summary(efectos_fijos_individual)

summary(efectos_fijos_two_ways)
