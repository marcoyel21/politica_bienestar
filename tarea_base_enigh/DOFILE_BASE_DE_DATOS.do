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
gen presto=30

replace pleche = rnormal(18.72)
replace phuevo=rnormal(28.5)
replace presto=rnormal(30)

gen gasto_resto=gasto_alimentos-gasto_leche-gasto_huevo

gen qleche=gasto_leche/pleche
gen qhuevo=gasto_huevo/phuevo
gen qresto=gasto_resto/presto

gen shuevo=gasto_huevo/gasto_alimentos
gen sleche=gasto_leche/gasto_alimentos
gen sresto=gasto_resto/gasto_alimentos 

egen stotal=rsum(s*)
tab stotal

save BASETC

quaids shuevo sleche sresto, anot(10) prices(phuevo pleche presto) expenditure(gasto_alimentos) demographics(est_socio) nolog



*Precio de la leche: 18.72 pesos por litro
*Para el precio promedio de la leche, se recabó información en el Sistema Nacional de Información e Integración de Mercados (SNIM) de la Secretaría de Economía; utilizando el precio promedio ponderado por población del litro de la leche pasteurizada vendida en tiendas el día 24 de julio de 2020 -la última actualización.
*Vínculo: http://www.economia-sniim.gob.mx/nuevo/ 

*Precio del huevo: 28.5 pesos por kilo
*Para obtener el precio promedio del huevo, se promedió el precio frecuente del huevo blanco vendido al menudeo del 8 al 12 de febrero de 2021 obtenido del SNIM en el siguiente vínculo: http://www.economia-sniim.gob.mx/Nuevo/Home.aspx?opcion=/SNIIM-Pecuarios-Nacionales/MenAve.asp 

*Como los precios, tanto del huevo, como de la leche no varían significativamente entre los estados y municipios, se determinó utilizar estos precios promedio. 

*Utilizamos como variable demográfica "est_socio" con la que se clasifica el hogar conforme a sus características socioeconómicas de quienes las habitan y su equipamiento.  
*1: Bajo
*2: Medio bajo
*3: Medio alto
*4: Alto

*Para el precio del resto de los bienes se promedió el precio de los demás productos de la canásta básica -exceputando el de la leche y los huevos-. Se tomó como referencia la canasta básica dado que la leche y el huevo son productos de esa canasta. Los precios se obtuvieron de la página de la Secretaría de Desarrollo Económico, se eligió el de las tiendas de autoservicio por estar más estandarizado que el de mercados públicos o mercados sobre ruedas.
*1. Maíz
*2. Frijol
*3. Arroz
*4. Azucar
*5. Harina de maiz enriquecida
*6. Aceite vegetal comestible
*7. Atún
*8. Sardina
*9. Leche fluida, en polvo y derivado de la leche
*10. Chiles envasados
*11. Café soluble
*12. Sal de mesa
*13. Avena
*14. Pasta para sopa
*15. Harina de trigo
*16. Chocolate
*17. Galletas marías, de animales y saladas
*18. Lentejas:17 pesos
*19. Jabon de lavandería
*20. Jabón de tocador
*21. Papel higiénico
*22. Detergente en polvo
*23. Crema dental
*24. Carne de res
*25. Carne de puerco
*26. Carne de pollo
*27. Tostadas
*28. Pan de caja y de dulce
*29. Huevo fresco
*30. Pescado seco 
*31. Agua purificada
*32. Golosina de amaranto, cacahuate, etc
*33. Puré de tomate envasado
*34. Frutas deshidratadas
*35. Jamaica y tamarindo naturales
*36. Concentrados sin azucar para elaboración
*37. Gelatina
*38. Garbanzos, chícaros y soya
*39. Cuadro básico de frutas y verduras
*40. Pilas





