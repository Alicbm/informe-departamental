library(data.table)
library(dplyr)

data <- fread("geih_modified.csv")


#CALCULOS POBLACIONALES
poblacion_total <- data[, .(poblacion_total = sum(FEX_C18) / unique(N_MESES)), by = .(DPTO, FECHA)]
poblacion_hombres <- data[P3271 == 1, .(poblacion_hombres = sum(FEX_C18) / unique(N_MESES)), by = .(DPTO, FECHA)]
poblacion_mujeres <- data[P3271 == 2, .(poblacion_mujeres = sum(FEX_C18) / unique(N_MESES)), by = .(DPTO, FECHA)]
poblacion_jovenes <- data[P6040 >= 15 & P6040 <= 28, .(poblacion_jovenes = sum(FEX_C18) / unique(N_MESES)), by = .(DPTO, FECHA)]

poblacion <- poblacion_total[poblacion_hombres, on = c("DPTO", "FECHA")][poblacion_mujeres, on = c("DPTO", "FECHA")][poblacion_jovenes, on = c("DPTO", "FECHA")]


#PIRAMIDE POBLACIONAL

bins <- seq(0, 100, by=10)
labels <- paste0(seq(0, 90, by=10), "-", seq(9, 99, by=10))

#creando variable rango de edad
data[, RANGO_EDAD := cut(P6040, breaks=bins, labels=labels, right=FALSE)]

poblacion_piramide <- data[, .(cantidad = sum(FEX_C18) / unique(N_MESES)), by=.(RANGO_EDAD, P3271, DPTO, FECHA)]

poblacion_piramide[, P3271 := ifelse(P3271 == 1, "Hombre", "Mujer")]



#SITUACION CONYUGAL  
#P6070 => ESTADO CIVIL

poblacion_estado_civil  <- data[, .(total_personas = sum(FEX_C18) / unique(N_MESES)), by = .(DPTO, P6070, FECHA)] 
setnames(poblacion_estado_civil, old = "P6070", new = "estado_civil")

poblacion_estado_civil <- poblacion_estado_civil[!is.na(estado_civil)]
poblacion_estado_civil



#INDICADORES ECONOMICOS

factor_expansion <- data[, .(factor_expansion = sum(FEX_C18) / unique(N_MESES)), by = .(DPTO, FECHA)]

ocupados <- data[, .(ocupados = sum(OCI * FEX_C18, na.rm = TRUE) / unique(N_MESES)), by = .(DPTO, FECHA)]

desocupados <- data[, .(desocupados = sum(DSI * FEX_C18, na.rm = TRUE) / unique(N_MESES)), by = .(DPTO, FECHA)]

poblacion_edad_trabajar <- data[P6040 >= 15, 
                                     .(poblacion_edad_trabajar = sum(FEX_C18, na.rm = TRUE) / length(unique(MES))), by = .(DPTO, FECHA)]

indicadores <- factor_expansion[ocupados, on = c("DPTO", "FECHA")][desocupados, on = c("DPTO", "FECHA")][poblacion_edad_trabajar, on = c("DPTO", "FECHA")]

indicadores[, fuerza_laboral := (ocupados + desocupados) ] #FUERZA LABORAL
indicadores[, tasa_desempleo := (desocupados / fuerza_laboral) * 100] #TASA DE DESEMPLEO
indicadores[, tasa_ocupacion := (ocupados / poblacion_edad_trabajar) * 100] #TASA DE OCUPACIÓN
indicadores[, tasa_global_participacion := (fuerza_laboral / poblacion_edad_trabajar) * 100] #TASA DE GLOBAL DE PARTICIPACION

#TASA DE SUBOCUPACION



#OCUPADOS POR ACTIVIDAD ECONOMICA
#RAMA4D_R4 => RAMAS DE ACTIVIDAD CIIU

pob_oci_sector <- data[OCI == 1, .(factor_expansion = sum(FEX_C18)), by = .(RAMA_ACTIVIDAD, DPTO, FECHA)]

test <- data[AÑO == 2024 & OCI == 1 & RAMA2D_R4 == 9,]
test[, RAMA2D_R4]









#P7090 => desea trabajar mas horas
#P7140S2 => mejorar sus ingresos




factor_expansion <- data[AÑO == 2024, .(factor_expansion = sum(FEX_C18) / unique(N_MESES)), by = DPTO]

ocupados <- data[AÑO == 2024, .(ocupados = sum(OCI * FEX_C18, na.rm = TRUE) / unique(N_MESES)), by = .(DPTO)]

desocupados <- data[AÑO == 2024, .(desocupados = sum(DSI * FEX_C18, na.rm = TRUE) / unique(N_MESES)), by = .(DPTO)]

poblacion_edad_trabajar <- data[AÑO == 2024 & P6040 >= 15, 
                                .(poblacion_edad_trabajar = sum(FEX_C18, na.rm = TRUE) / length(unique(MES))), by = .(DPTO)]

indicadores <- factor_expansion[ocupados, on = c("DPTO")][desocupados, on = c("DPTO")][poblacion_edad_trabajar, on = c("DPTO")]

indicadores[, fuerza_trabajo := (ocupados + desocupados) ] #POBLACIÓN ACTIVA
indicadores[, tasa_desempleo := (desocupados / fuerza_trabajo) * 100] #TASA DE DESEMPLEO
indicadores[, tasa_ocupacion := (ocupados / poblacion_edad_trabajar) * 100] #TASA DE OCUPACIÓN
indicadores[, tasa_global_participacion := (fuerza_trabajo / poblacion_edad_trabajar) * 100]


indicadores[, sum(poblacion_edad_trabajar)]






