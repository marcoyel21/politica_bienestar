import delimited C:\Users\cec\Documents\localidad\localidad.csv
save "C:\Users\cec\Documents\localidad\localidad.dta"
save localidad
tab vph_inter
tab vph_inter
tab vph_pc
encode vph_inter, gen(num_internet)
encode tvivparhab, gen(vivparthab)
gen prop_inter=(num_internet/vivparthab)
sum prop_inter
encode vivpar_hab, gen(vph)
gen prop_inter2=(num_internet/vph)
sum prop_inter2
sum prop_inter2 if prop_inter2>1
sum prop_inter if prop_inter>1
gen propinter=(num_internet/tvivhab)
sum propinter if propinter>1
gen interprop=num_internet/vivtot
sum interprop if interprop>1
