library(data.table)

data <- fread("all_geihs.csv")

#REEMPLAZAR DIVIPOLA DE DEPARTAMENTOS POR SU NOMBRE RESPECTIVO
data[, DPTO := fcase(
  DPTO == "5", "Antioquia",
  DPTO == "8", "Atlántico",
  DPTO == "11", "Bogotá D.C.",
  DPTO == "13", "Bolívar",
  DPTO == "15", "Boyacá",
  DPTO == "17", "Caldas",
  DPTO == "18", "Caquetá",
  DPTO == "19", "Cauca",
  DPTO == "20", "Cesar",
  DPTO == "23", "Córdoba",
  DPTO == "25", "Cundinamarca",
  DPTO == "27", "Chocó",
  DPTO == "41", "Huila",
  DPTO == "44", "La Guajira",
  DPTO == "47", "Magdalena",
  DPTO == "50", "Meta",
  DPTO == "52", "Nariño",
  DPTO == "54", "Norte de Santander",
  DPTO == "63", "Quindío",
  DPTO == "66", "Risaralda",
  DPTO == "68", "Santander",
  DPTO == "70", "Sucre",
  DPTO == "73", "Tolima",
  DPTO == "76", "Valle del Cauca",
  DPTO == "81", "Arauca",
  DPTO == "85", "Casanare",
  DPTO == "86", "Putumayo",
  DPTO == "88", "San Andrés y Providencia",
  DPTO == "91", "Amazonas",
  DPTO == "94", "Guainía",
  DPTO == "95", "Guaviare",
  DPTO == "97", "Vaupés",
  DPTO == "99", "Vichada",
  default = NA_character_
)]

#CAMBIAR NOMBRE DE LA COLUMNA DEL AÑO
setnames(data, old = "año_geih", new = "AÑO")

#CREAR UNA COLUMNA PARA LA FECHA (MENSUAL)
data[, FECHA := as.Date(paste0(AÑO, "-", sprintf("%02d", MES), "-01"))]

#CREAR COLUMNA DE NUMERO DE MESES POR CADA AÑO RESPECTIVO (PARA EL CALCULO DE POBLACION Y DEMAS)
n_meses_por_año <- data[, .(N_MESES = uniqueN(MES)), by = .(DPTO, AÑO)]
data <- merge(data, n_meses_por_año, by = c("DPTO", "AÑO")) # UNIR ESA COLUMAN A LA GEIH

#CREAR VARIABLE NUEVA: RAMA DE ACTIVIDAD (CIIU)
#RAMA2D_R4 => RAMAS DE ACTIVIDAD CIIU

data[, PREFIX_RAMA4D_R4 := as.numeric(substr(RAMA2D_R4, 1, 2))] #VARIABLE DE RAMA DE ACTIVIDA A NUMERICA PARA HACER LAS COMPARACIONES

data[OCI == 1, RAMA_ACTIVIDAD := fcase(
  PREFIX_RAMA4D_R4 <= 3, "Agricultura, ganadería, caza, silvicultura y pesca",
  PREFIX_RAMA4D_R4 >= 5 & PREFIX_RAMA4D_R4 <= 9 | PREFIX_RAMA4D_R4 >= 35 & PREFIX_RAMA4D_R4 <= 39, "Suministro de electricidad, gas, agua y gestión de desechos",
  PREFIX_RAMA4D_R4 >= 10 & PREFIX_RAMA4D_R4 <= 33, "Industrias manufactureras",
  PREFIX_RAMA4D_R4 >= 41 & PREFIX_RAMA4D_R4 <= 43, "Construcción",
  PREFIX_RAMA4D_R4 >= 45 & PREFIX_RAMA4D_R4 <= 47, "Comercio y reparación de vehículos",
  PREFIX_RAMA4D_R4 >= 55 & PREFIX_RAMA4D_R4 <= 56, "Alojamiento y servicios de comida",
  PREFIX_RAMA4D_R4 >= 49 & PREFIX_RAMA4D_R4 <= 53, "Transporte y almacenamiento",
  PREFIX_RAMA4D_R4 >= 58 & PREFIX_RAMA4D_R4 <= 63, "Información y comunicaciones",
  PREFIX_RAMA4D_R4 >= 64 & PREFIX_RAMA4D_R4 <= 66, "Actividades financieras y de seguros",
  PREFIX_RAMA4D_R4 == 68, "Actividades inmobiliarias",
  PREFIX_RAMA4D_R4 >= 69 & PREFIX_RAMA4D_R4 <= 82, "Actividades profesionales, científicas, técnicas y servicios administrativos",
  PREFIX_RAMA4D_R4 >= 84 & PREFIX_RAMA4D_R4 <= 88, "Administración pública y defensa, educación y atención de la salud humana",
  PREFIX_RAMA4D_R4 >= 90, "Actividades artísticas, entretenimiento, recreación y otras actividades de servicios",
  default = "No informa"
)]

pob_oci_sector <- data[OCI == 1 & AÑO == 2024 & MES == 6, .(factor_expansion = sum(FEX_C18)), by = .(RAMA_ACTIVIDAD)]
pob_oci_sector[, sum(factor_expansion)]


#CREAR ARCHIVO CON LAS MODIFICACIONES
fwrite(data, "geih_modified.csv")


