
#importo los datos


import delimited "stata.csv"


# corro el quaids

quaids gasto_leche gasto_huevo, anot(1) {prices (pleche, phuevo)}{expenditure (gasto_alimentos)}quaids s_leche s_huevo s_other, anot(10) prices(p_leche p_huevo p_other) expenditure(alimentos)
