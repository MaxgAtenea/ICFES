############################################################
#Calculo valor agregado
#Abril 2025 
#Atenea

#Fuentes de los datos

#1.bd.csv: Base elaborada manualmente con la informacion de data icfes.  

#2. codigos_snies_cine_2023.csv: https://snies.mineducacion.gov.co/portal/ESTADISTICAS/Bases-consolidadas/
#Archivo csv: "Estudiantes matriculados 2023"
#El archivo viene sin duplicados de codigos snies
############################################################


#Cargar las librerias
library(readr) #lectura de archivos
library(dplyr) #manipulacion de dataframes
library(tidyr)
library(ggplot2) #para graficar
library(nlme) #activamos la librería/paquete que nos permite estimar el modelo multinivel


##################################
#Lectura de los datos
##################################

#Leer la base consolidada i,e., Saber Pro cruzado con Saber 11
icfes <- read_delim("ICFES/data/BD/bd.csv", escape_double = FALSE, trim_ws = TRUE)


#Leer base con codigos snies y cine de los programas
cine_snies <- read_delim("ICFES/data/SNIES_CINE/codigos_snies_cine_2023.csv",
                         escape_double = FALSE,
                         trim_ws = TRUE,
                         delim=";",
                         locale = locale(encoding = "Latin1"))

#Verificacion de duplicados para cine_snies
keys <- c(
  "PROGRAMA ACADÉMICO",
  "PROGRAMA ACREDITADO",
  "CÓDIGO DE LA INSTITUCIÓN",
  "CÓDIGO DEL MUNICIPIO (PROGRAMA)",
  "ID MODALIDAD"
  #"ID SEXO",
  #"SEMESTRE",
  #"MATRICULADOS"
  )

#Si bien el codigo SNIES es unico, el nombre del Programa Académico tiene duplicados
#Estos estan en funcion de:
#1. Programa academico
#2. Programa acreditado
#3. Codigo de la institucion
#4. CÓDIGO DEL MUNICIPIO (PROGRAMA)
#5. Id modalidad 
#6. Id sexo #no es relevante para el programa
#7. Semestre #no es relevante para el programa
#8. Matriculados #no es relevante para el programa
#Existen 73 duplicados con las llaves del 1-5 y 14 duplicados del 1-8

# #Mirar cuales son las filas duplicadas
# duplicated_rows <- cine_snies %>%
#   filter(duplicated(select(., all_of(keys))))

#Quedarse con las filas no duplicadas
cine_snies <- cine_snies %>%
  distinct(across(all_of(keys)), .keep_all = TRUE)

#Filtrar por los programas que se ofrecen en Bogota
#Tener en cuenta que existe otro potencial filtro: `CÓDIGO DEL MUNICIPIO IES`
#Discutirlo con el equipo

cine_snies <- cine_snies %>%
  filter(`CÓDIGO DEL MUNICIPIO (PROGRAMA)` == 11001)

############################################
#Mirar los programas cuyos snies no cruzan
############################################

#Miramos los codigos snies de los programas en la BD cine_snies
programas_bdcine_snies <- cine_snies %>%
  select(
    codigo_snies = `CÓDIGO SNIES DEL PROGRAMA`,
    nombre_programa_bdsnies = `PROGRAMA ACADÉMICO`
  ) %>%
  distinct() %>%
  mutate(from_cine_snies = TRUE) #agrega identificador de la BD donde vienen los datos

#Miramos codigos snies de los programas en la BD icfes
programas_bdicfes <- icfes %>%
  select(
    codigo_snies = estu_snies_prgmacademico,
    nombre_programa_bdicfes = estu_prgm_academico
  ) %>%
  distinct() %>%
  mutate(from_icfes = TRUE) #agrega identificador de la BD donde vienen los datos

#Hacer outer join para mirar los programas que coincidieron y no coincidieron en las BDs 
programas <- full_join(programas_bdcine_snies, programas_bdicfes, by = c("codigo_snies"))

#Omitir codigos repetidos
programas <- programas %>%
  distinct(codigo_snies, .keep_all = TRUE)

#Liberar memoria
remove(programas_bdcine_snies)
remove(programas_bdicfes)

#Programas no que cruzaron
programas_non_matching <- programas %>%
  filter(is.na(from_cine_snies) | is.na(from_icfes))

#Programas que si cruzaron
programas_matching <- programas %>%
  filter(!is.na(from_cine_snies) & !is.na(from_icfes))

############################################
#Merge de la data del icfes con la del snies
############################################

#Hacer inner join con la base icfes y cine_snies
data <- inner_join(
  icfes,
  cine_snies,
  by = join_by(estu_snies_prgmacademico==`CÓDIGO SNIES DEL PROGRAMA`)
)

#liberamos memoria
remove(cine_snies)
remove(icfes)

#Resumen de los datos por tipo de dato y Nans
data_summary <- data %>%
  summarise(across(everything(), list(
    class = ~ class(.)[1],
    NA_count = ~ sum(is.na(.)),
    NaN_count = ~ sum(is.nan(.))
  ), .names = "{.col}___{.fn}")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("column", ".value"),
    names_sep = "___"
  )

#Mirar la frecuencia de cada programa
#Nota: recordar que el programa depende de la institucion.
programas_freq <- data %>%
  count(estu_snies_prgmacademico, estu_prgm_academico, name = "frecuencia") %>%
  mutate(estu_prgm_academico = tolower(estu_prgm_academico)) %>%
  mutate(programa_label = paste0(estu_snies_prgmacademico, " - ", estu_prgm_academico))

#Obtener los top 10 programas con mas frecuencia
top_10 <- programas_freq %>% slice_max(frecuencia, n = 10)

# Graficar top 10
ggplot(top_10, aes(x = reorder(programa_label, frecuencia), y = frecuencia)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 10 Programas", x = "Programa", y = "Frecuencia") +
  theme_minimal()

# bottom_10 <- programas_freq %>% slice_min(frecuencia, n = 10)
# # Bottom 10
# ggplot(bottom_10, aes(x = reorder(programa_label, frecuencia), y = frecuencia)) +
#   geom_bar(stat = "identity", fill = "firebrick") +
#   coord_flip() +
#   labs(title = "Bottom 10 Programas", x = "Programa", y = "Frecuencia") +
#   theme_minimal()

#Graficar una densidad de la frecuencia de los programas
#La grafica se utiliza para tener un panorama general de las frecuencias
ggplot(programas_freq, aes(x = frecuencia)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(
    x = "Frecuencia de aparición del programa",
    y = "Densidad estimada",
    title = "Densidad de frecuencias de programas"
  ) +
  theme_minimal()

############################################
#Data para el calcaculo del VA
############################################


#Seleccionar las columnas de interes
keys <- c(
  "estu_consecutivo_icfes",
  "estu_consecutivo_pro",
  "INSTITUCIÓN DE EDUCACIÓN SUPERIOR (IES)",
  "inst_nombre_institucion",
  "estu_nucleo_pregrado",
  "estu_snies_prgmacademico",
  "NÚCLEO BÁSICO DEL CONOCIMIENTO (NBC)",
  "ID CINE CAMPO AMPLIO",
  "ID CINE CAMPO ESPECIFICO",
  "ID CINE CAMPO DETALLADO",
  "punt_global_pro",
  "punt_global_icfes",
  "dif_periodos"
)

keys_limpios <- c(
  "estu_consecutivo_icfes",
  "estu_consecutivo_pro",
  "institucion_de_educacion_superior_ies",
  "inst_nombre_institucion",
  "estu_nucleo_pregrado",
  "estu_snies_prgmacademico",
  "nucleo_basico_del_conocimiento_nbc",
  "id_cine_campo_amplio",
  "id_cine_campo_especifico",
  "id_cine_campo_detallado",
  "punt_global_pro",
  "punt_global_icfes",
  "dif_periodos"
)

#Seleccionar las anteriores variables y quedarnos con las filas que si tienen
#observaciones en punt_global_icfes
data_filtrado <- data %>%
  select(all_of(keys)) %>%
  filter(
    !is.na(punt_global_icfes),
    dif_periodos >= 40,
    dif_periodos <= 80,
    estu_nucleo_pregrado != "MEDICINA"
  )

#renombrar las columnas
colnames(data_filtrado) <- keys_limpios

#Existen 54 NBC unicos
#El problema con el campo NBC es que no es un id sino un string, entonces no es fiable
length(unique(data_filtrado$nucleo_basico_del_conocimiento_nbc))

#Existen 72 CINE detallados unicos
length(unique(data_filtrado$id_cine_campo_detallado))

#Conteo de observaciones por codigo cine_detallado
conteo_campo_detallado <- data_filtrado %>%
  count(id_cine_campo_detallado, name = "frecuencia")

#Grafica de barras para visualizar el conteo por codigo cine detallado
ggplot(conteo_campo_detallado, aes(x = reorder(id_cine_campo_detallado, -frecuencia), y = frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = frecuencia), vjust = -0.2, angle = 90, size = 3) +
  labs(
    title = "Frecuencia por ID CINE Campo Detallado",
    x = "ID CINE Campo Detallado",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



############################################
#Data para el calcaculo del VA
############################################

#Liberar memoria
remove(data,
       data_summary,
       programas,
       programas_freq,
       programas_matching,
       programas_non_matching,
       top_10,
       conteo_campo_detallado
)

#Fijamos la BD con la que vamos a trabajar para no llamarla con cada variable
attach(data_filtrado) 

#estimamos la regresion multinivel con los codigos CINE
multinivel_basico <- lme(
  punt_global_pro ~ punt_global_icfes, random = ~ 1| id_cine_campo_detallado, data = data_filtrado
  )

#Ahora extraemos el listado de efectos aleatorios (valor agregado)
coeff_va <- ranef(multinivel_basico)
View(coeff_va)

multinivel_basico[["coefficients"]][["random"]] # para visualizar el VA más directamente




####################################################
#TBD
###################################################








