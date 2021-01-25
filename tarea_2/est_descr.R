
#Parte I
library(readr)
library(dplyr)
library(splitstackshape)
library(corrplot)
library(ggplot2)
library(viridis)
library(viridisLite)
library(hrbrthemes)
library(tidyr)
library(gapminder)
library(janitor)
library(vtree)
library(CGPfunctions)
library(ggridges)
library(arsenal)
library(table1)
library(kableExtra)
library(stargazer)

#importo los datos
d<-read_csv("pobreza_18.csv")
attach(d)

#creo una variable del grupo 3 (mi objetivo)
d.1<-d%>%mutate(y_3=ifelse(d$cuadrantes==3,1,0))

#1. Análisis correlaciones

varscor<- data.frame(y_3,tam_loc,rururb,tamhogesc,ic_rezedu,
                     ic_asalud,ic_segsoc,pea,jef_ss,
                     s_salud,isb_combus,ins_ali,ictpc,
                     carencias,ing_mon,ing_lab,ing_ren,ing_tra,ic_cv,ic_ali)

cor_data<-as.data.frame(cor(varscor,use="complete.obs"))
cor_data_filtered<-cor_data %>%select(y_3)
View(cor_data_filtered,"simple")

#Con modelo Marco
varscor2<-data.frame(y_3,factor,plb, s_salud, jef_ss,ss_dir,rururb,
                     ic_sbv,tam_loc ,ic_rezedu,ic_asalud ,
                     ic_cv,isb_combus,ic_ali)

cor_data2<-as.data.frame(cor(varscor2,use="complete.obs"))
cor_data_filtered<-cor_data2 %>%select(y_3)
kable(cor_data_filtered,"simple")

#Mío
corrplot(cor(varscor,use="complete.obs"), method="circle")

#Marco
corrplot(cor(varscor2,use="complete.obs"), method="circle")

###############################################################################

#1.5 Tabla Descriptivas (media, mediana,tango, sd)

t1<-tableby(as.factor(cuadrantes)~.,data=varscor)
l1<-list(plb="Ingreso < LB",s_salud="Servicios médicos",jef_ss="Acceso SS x jefatura",
         rururb="Loc. Rural",ic_sbv="Carencia serv. básicos viv.", 
         tam_loc="Tamaño loc.", ic_rezedu="Carencia rezago educativo",
         ic_asalud="Carencia x acceso serv. salud",ic_cv="carencia x calidad y esp. vivienda",
         isb_combus="Carencia acceso serv. combustible", ic_ali="Carencia acceso alimentación")
t2<-summary(t1, title = "Estadísticas Descriptivas",labelTranslations = l1)

kable(t2,"simple")

#2. Tablas cruzadas entre Cuadrantes y variables de interés

# s_salud 
tabyl(d, cuadrantes, s_salud) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes, s_salud) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

#plb
tabyl(d, cuadrantes, plb) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes, plb) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()
#,jef_ss,
tabyl(d, cuadrantes, jef_ss) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes, jef_ss) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()
#ss_dir,
tabyl(d, cuadrantes, ss_dir) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes, ss_dir) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()
#rururb,
tabyl(d, cuadrantes, rururb) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes,rururb) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

#ic_sbv,
tabyl(d, cuadrantes, ic_sbv) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes,ic_sbv) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()
#tam_loc ,
tabyl(d, cuadrantes, tam_loc) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes, tam_loc) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()
#ic_rezedu,
tabyl(d, cuadrantes, ic_rezedu) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes, ic_rezedu) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()
#ic_asalud
tabyl(d, cuadrantes, ic_asalud) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes,ic_asalud) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

#ic_cv,

tabyl(d, cuadrantes, ic_cv) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes, ic_cv) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

#isb_combus,
tabyl(d, cuadrantes, isb_combus) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes,isb_combus) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

#ic_ali
tabyl(d, cuadrantes, ic_ali) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

tabyl(d, cuadrantes,ic_ali) %>% adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

##############################################################
#3. Datos cruzados y variables interés, en gráfico


## cAMBIAR GRÁFICOS, CON %

PlotXTabs(d,s_salud,cuadrantes,plottype = "percent")
PlotXTabs(d,jef_ss,cuadrantes,plottype = "percent")
PlotXTabs(d,ss_dir,cuadrantes,plottype = "percent")
PlotXTabs(d,rururb,cuadrantes,plottype = "percent")

#en este tiene 100% en 0
PlotXTabs(d,ic_sbv,cuadrantes,plottype = "percent")

PlotXTabs(d,tam_loc,cuadrantes,plottype = "percent")

#Todos están en 0
PlotXTabs(d,ic_rezedu,cuadrantes,plottype = "percent")
#Todos 0
PlotXTabs(d,ic_asalud,cuadrantes,plottype = "percent")
#Todos 0
PlotXTabs(d,ic_cv,cuadrantes,plottype = "percent")
#Todo 0
PlotXTabs(d,isb_combus,cuadrantes,plottype = "percent")
#Todo 0
PlotXTabs(d,ic_ali,cuadrantes,plottype = "percent")

#################################################################
#4. VTREE

vtree(d,"cuadrantes",palette=2,sortfill=TRUE)

vtree(d,c("cuadrantes","vul_ing"),palette=c(2,3),sortfill=TRUE,horiz=FALSE)

vtree(d,c("cuadrantes","vul_car"),palette=c(2,3),sortfill=TRUE,horiz=FALSE)

##################################################################
#4. Boxplots de ingresos

d.1  %>%ggplot(aes(x=as.factor(cuadrantes), y=ing_mon, fill=as.factor(cuadrantes))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Ingreso por Cuadrante") +
  xlab("")+
  ylim(0,35000)

d  %>%ggplot(aes(x=as.factor(cuadrantes), y=ing_lab, fill=as.factor(cuadrantes))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Ingreso por Cuadrante") +
  xlab("")+
  ylim(0,35000)



#################################################
#5. otros

#Cuadrantes x ing_lab y contar con seguro

d %>% drop_na(ing_lab)%>% ggplot(aes(x=as.factor(cuadrantes),y=ing_lab))+geom_bar(stat='identity')+
  facet_wrap(facets=vars(s_salud),scales="free_y")

#ggridge ingreso vs cuadrantes 
ggplot(d, aes(x = `ing_mon`, y = as.factor(`cuadrantes`), fill = ..x..,group=`cuadrantes`)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Ingreso por hogar", option = "C") +
  labs(title = 'Distr Ingreso por Cuadrante') +
  theme_ipsum() + xlim(0,35000)
theme(
  legend.position="none",
  panel.spacing = unit(0.1, "lines"),
  strip.text.x = element_text(size = 8)
)


#Histograma regresiones ingreso vs tamaño del hogar por cuadrantes

d%>%ggplot(aes(x=tamhogesc, y=ing_mon, col=as.factor(cuadrantes))) + 
  geom_smooth(method="lm", size=1, se=FALSE) + 
  coord_cartesian(xlim=c(0, 19), ylim=c(0, 35000)) + 
  labs( y="Ingreso hogares",
        x="Tamaño hogar", caption="Por Cuadrante")























