
install.packages("ggplot2")
install.packages("stats")                 
install.packages("stargazer")
install.packages("dplyr")
library(stats)                 
library(stargazer) 
library("ggplot2")
library(dplyr)

# LM Kuznet
tabla2 <- read_excel("Maestría/Bienestar/tabla2_2.xlsx")
View(tabla2)

#linea conectora

ggplot(tabla2,aes(x=PIB,y=kuznet)) +
  geom_point()+ theme_minimal() + geom_line()+
  labs(title="PIB vs Kuznet",x="PIB",
       y="Kuznet ratio") +
  theme(plot.title=element_text(hjust=0.5))

#linea regresion

ggplot(tabla2,aes(x=PIB,y=kuznet)) +
  geom_point() + 
  geom_smooth(method='lm',color="blue") + theme_minimal() +
  labs(title="PIB vs Kuznet",x="PIB",
       y="Kuznet ratio") +
  theme(plot.title=element_text(hjust=0.5))









