library(data.table)
library(officer)
library(flextable)
fmt_num<-function(x,digits=1){out<-formatC(x, format="f", digits=digits, big.mark=" ", decimal.mark="."); out[is.na(x)]<-""; out}
fmt_int<-function(x){out<-formatC(round(x), format="f", digits=0, big.mark=" ", decimal.mark="."); out[is.na(x)]<-""; out}
x<-fread("data/derived/tables/tbl_nat_year_sex_mort.csv")
x<-x[year_id %in% 2018:2024 & cause_level==0 & age_group=="Todas las edades" & sex_label %in% c("Ambos","Hombre","Mujer"), .(year_id, sex_label, metric_abs, metric_rate)]
abs_wide<-dcast(x, year_id ~ sex_label, value.var="metric_abs", fill=0)
rate_wide<-dcast(x, year_id ~ sex_label, value.var="metric_rate", fill=0)
out<-merge(abs_wide, rate_wide, by="year_id", suffixes=c("_abs","_rate"), sort=TRUE)
setnames(out, c("year_id","Ambos_abs","Hombre_abs","Mujer_abs","Ambos_rate","Hombre_rate","Mujer_rate"), c("anio","n_muertes_ambos","n_muertes_hombre","n_muertes_mujer","tasa_ambos_100000","tasa_hombre_100000","tasa_mujer_100000"))
out[, n_muertes_ambos:=fmt_int(n_muertes_ambos)]
out[, n_muertes_hombre:=fmt_int(n_muertes_hombre)]
out[, n_muertes_mujer:=fmt_int(n_muertes_mujer)]
out[, tasa_ambos_100000:=fmt_num(tasa_ambos_100000)]
out[, tasa_hombre_100000:=fmt_num(tasa_hombre_100000)]
out[, tasa_mujer_100000:=fmt_num(tasa_mujer_100000)]
theme_ft<-function(ft){label_map<-c(anio="Anio", n_muertes_ambos="N ambos", n_muertes_hombre="N hombres", n_muertes_mujer="N mujeres", tasa_ambos_100000="Tasa ambos", tasa_hombre_100000="Tasa hombres", tasa_mujer_100000="Tasa mujeres"); ft<-do.call(flextable::set_header_labels, c(list(x=ft), as.list(label_map))); ft<-theme_booktabs(ft); ft<-fontsize(ft, size=7, part="all"); ft<-align(ft, align="center", part="all"); ft<-padding(ft, padding=2, part="all"); ft<-autofit(ft); ft<-fit_to_width(ft, max_width=6.5); ft<-set_table_properties(ft, layout="autofit", width=1); ft}
print(out)
ft<-theme_ft(flextable(out))
print(dim(ft$body$dataset))
doc<-read_docx()
doc<-body_add_par(doc, "Test T1 exacta", style="heading 1")
doc<-body_add_flextable(doc, ft)
print(doc, target="reports/all_causes_word_report/test_t1_exacta.docx")
