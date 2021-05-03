
La base de datos de localidades del 2010 no se puede extraer del INEGI. Sale de aquí en el segundo recuadro de la lista (el que dice CSV)

http://ri.uaemex.mx/handle/20.500.11799/58449


En este, las claves se llaman: "ClaveEnt", "ClaveLoc", "ClaveMun"

En el del 2020 son: "ENTIDAD", "MUN", "LOC"

En el script (en el último chunk) cambié el código para incluir a estas variables, lo intenté convertir a numérico, y hacer lo de los dígitos. Cuando hago los select me sale de error:

Column ENTIDAD doesnt exist
Column ClaveEnt doesnt exist

estas son las primeras columnas de cada base.


y cuando los intento hacer numéricos:

Error in `[[<-.data.frame`(`*tmp*`, i, value = numeric(0)) : 
  replacement has 0 rows, data has 195662





