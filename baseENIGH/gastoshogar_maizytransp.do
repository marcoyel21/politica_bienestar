*************dofile gastos del hogar del maiz, A001-A006 y transporte público (B)*****

**nota como gasto se tomo solo gasto_trmestral

destring gasto_tri, replace
sort folioviv
by folioviv: egen sum_gastotri_hog=total(gasto_tri)
encode clave, gen(Clave)
drop clave
rename Clave clave

*****
egen gasto_maiz_hogar = total(gasto_tri*(clave==1|clave==2|clave==3|clave==4|clave==5)), by(folioviv)
egen gasto_transppub_hogar = total(gasto_tri*(clave==167|clave==168|clave==169|clave==170|clave==171|clave==172|clave==173)), by(folioviv)
destring cantidad, replace
gen precios= (gasto_tri/cantidad)