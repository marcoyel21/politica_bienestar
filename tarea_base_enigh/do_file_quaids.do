
#importo los datos

import delimited "stata.csv"

#Precios centrados en la media


replace p_leche = rnormal(18.72)
replace p_huevo=rnormal(28.5)
replace p_other=1 #Esto es opcional, si quieren la solucion de rafa, quiten esta linea


# corro el quaids
quaids s_leche s_huevo s_other, anot(10) prices(p_leche p_huevo p_other) expenditure(alimentos)
