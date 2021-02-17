clear
use "/Users/cynthiavaldivia/Desktop/MAESTRIA_ECONOMIA_APLICADA/BIENESTAR Y POLITICA SOCIAL/TAREAS/CONCENTRADO HOGAR.dta"

merge using "/Users/cynthiavaldivia/Desktop/MAESTRIA_ECONOMIA_APLICADA/BIENESTAR Y POLITICA SOCIAL/TAREAS/TAREA 2/GASTOS HOGAR.dta"

save BASE

keep folioviv foliohog clave est_socio alimentos leche huevo

rename leche gasto_leche
rename huevo gasto_huevo
rename alimentos gasto_alimentos

gen pleche=18.72
gen phuevo=28.5

gen qleche=gasto_leche/pleche
gen qhuevo=gasto_huevo/phuevo

save BASE1

egen gt=rsum(g*)

*Precio de la leche: 18.72 pesos por litro
*Para el precio promedio de la leche, se recabó información en el Sistema Nacional de Información e Integración de Mercados (SNIM) de la Secretaría de Economía; utilizando el precio promedio ponderado por población del litro de la leche pasteurizada vendida en tiendas el día 24 de julio de 2020 -la última actualización.
*Vínculo: http://www.economia-sniim.gob.mx/nuevo/ 

*Precio del huevo: 28.5 pesos por kilo
*Para obtener el precio promedio del huevo, se promedió el precio frecuente del huevo blanco vendido al menudeo del 8 al 12 de febrero de 2021 obtenido del SNIM en el siguiente vínculo: http://www.economia-sniim.gob.mx/Nuevo/Home.aspx?opcion=/SNIIM-Pecuarios-Nacionales/MenAve.asp 

*Como los precios, tanto del huevo, como de la leche no varían significativamente entre los estados y municipios, se determinó utilizar estos precios promedio. 

*Utilizamos como variable demográfica "est_socio" con la que se clasifica el hogar conforme a las características socioeconómicas de quienes las habitan y su equipamiento.  
*1: Bajo
*2: Medio bajo
*3: Medio alto
*4: Alto
