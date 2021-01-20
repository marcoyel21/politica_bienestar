*******************************************************
* RESULTADOS A NIVEL NACIONAL
********************************************************;

tabstat pobreza pobreza_m pobreza_e vul_car vul_ing no_pobv carencias carencias3 ic_rezedu ic_asalud ic_segsoc ic_cv ic_sbv ic_ali plb_m plb [w=factor] if pobreza!=., stats(mean sum) format(%15.8gc) c(s)

********************************************************************************
* PORCENTAJE Y NÚMERO DE PERSONAS POR CONDICIÓN DE POBREZA POR ENTIDAD FEDERATIVA
********************************************************************************
tabstat pobreza pobreza_m pobreza_e vul_car vul_ing no_pobv [w=factor] if pobreza!=.,stats(mean sum) format(%11.6gc) by(ent)



*********************************************************************************
* PORCENTAJE Y NÚMERO DE PERSONAS CON CARENCIAS SOCIALES POR ENTIDAD FEDERATIVA
*********************************************************************************;
tabstat ic_rezedu ic_asalud ic_segsoc ic_cv ic_sbv ic_ali carencias carencias3 plb_m plb [w=factor] if pobreza!=., stats(mean sum) format(%11.6gc) by(ent)

