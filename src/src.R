############################################################
#Calculo valor agregado
#Abril 2025 
#Autor:TBD

#Fuentes de los datos

#1.bd.csv: Base elaborada manualmente con la informacion de data icfes.  

#2. codigos_snies_cine_2023.csv: https://snies.mineducacion.gov.co/portal/ESTADISTICAS/Bases-consolidadas/
#Archivo csv: "Estudiantes matriculados 2023.csv"
#NOTA: El archivo viene sin duplicados de los codigos SNIES

############################################################

#Cargar las librerias
library(readr) #lectura de archivos
library(stringi) #para ajustar nombres de las columnas de los data frames
library(dplyr) #manipulacion de dataframes
library(tidyr)
library(ggplot2) #para graficar
library(nlme) #activamos la librería/paquete que nos permite estimar el modelo multinivel


##################################
#constantes
##################################

variables_cine_snies <- c(
  "codigo_snies_del_programa",
  "programa_academico",
  "nucleo_basico_del_conocimiento_nbc",
  "id_cine_campo_amplio",
  "id_cine_campo_especifico",
  "id_cine_campo_detallado",
  "codigo_del_municipio_programa",
  "codigo_de_la_institucion",
  "institucion_de_educacion_superior_ies",
  "ies_acreditada",
  "caracter_ies"
)

#CINE campos amplios son 11 los reportados por el DANE
#CINE campos detallados son 121 (82+10 interdisciplinarios + 10 no clasificados en otra parte)

#CINES especificos segun documento DANE
#Son 39 (29 + 10 interdisciplinarios)
#https://www.sen.gov.co/sites/default/files/pagina-migraciones-files/2024-07/documento-de-la-clasificacion-internacional-normalizada-de-la-educacion-campos-de-educacion-y-formacion-adaptada-para-colombia-CINE-F-2013-A.C.pdf
cines_especificos = c(1,2,3,11,18,21,22,23,28,31,32,38,41,42,48,51,
                      52,53,54,58,61,68,71,72,73,78,81,82,83,84,88,
                      91,92,98,101,102,103,104,108)

##################################
#Lectura de los datos
##################################

#Leer la base consolidada del Saber Pro cruzado con Saber 11
icfes <- read_delim("ICFES/data/BD/bd.csv", escape_double = FALSE, trim_ws = TRUE)

#Resumen de los datos por tipo de dato y Nans
icfes_summary <- icfes %>%
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

#Leer base con codigos snies y cine de los programas
cine_snies <- read_delim("ICFES/data/SNIES_CINE/codigos_snies_cine_2023.csv",
                         escape_double = FALSE,
                         trim_ws = TRUE,
                         delim=";",
                         locale = locale(encoding = "Latin1"))

#Resumen de los datos por tipo de dato y Nans
#TODO: eliminar la fila en blanco desde el origen
cine_snies_summary <- cine_snies %>%
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

#Limpiar el nombre de las columnas de cine_sines
names(cine_snies) <- names(cine_snies) %>%
  stri_trans_general("Latin-ASCII") %>%       # elimina tildes
  tolower() %>%                               # convierte a minúsculas
  gsub(" ", "_", .) %>%                       # reemplaza espacios con _
  gsub("\\(|\\)", "", .)                      #Elimina parentesis

cine_snies <- cine_snies %>%
  select(all_of(variables_cine_snies))

#Verificamos que no hayan duplicados de codigos SNIES
#El numero de valores unicos coincide con el numero de observaciones en cine_snies
cine_snies %>%
  summarise(valores_unicos = n_distinct(codigo_snies_del_programa))


#Mirar codigos CINE unicos por nivel
cine_snies %>%
  summarise(
    n_campo_amplio = n_distinct(id_cine_campo_amplio),
    n_campo_especifico = n_distinct(id_cine_campo_especifico),
    n_campo_detallado = n_distinct(id_cine_campo_detallado)
  )

#Filtrar por los programas que se ofrecen en Bogota
#Tener en cuenta que existe otro potencial filtro: `CÓDIGO DEL MUNICIPIO IES`
#Sin embargo, para la estimacion el ICFES distingue las regiones donde se ofrece el programa
#Discutirlo con el equipo
cine_snies <- cine_snies %>%
  filter(codigo_del_municipio_programa == 11001)

#Mirar codigos CINE unicos por nivel a nivel Bogota
cine_snies %>%
  summarise(
    n_campo_amplio = n_distinct(id_cine_campo_amplio),
    n_campo_especifico = n_distinct(id_cine_campo_especifico),
    n_campo_detallado = n_distinct(id_cine_campo_detallado)
  )

#Mirar NBC unicos en el dataframe icfes
#100 valores unicos
icfes %>%
  summarise(valores_unicos = n_distinct(estu_nucleo_pregrado))

#Mirar NBC unicos en el dataframe cine_snies
#100 valores unicos
cine_snies %>%
  summarise(valores_unicos = n_distinct(nucleo_basico_del_conocimiento_nbc))

############################################
#Mirar los programas cuyos snies no cruzan
############################################

#Miramos los codigos SNIES de los programas en el dataframe cine_snies
programas_bdcine_snies <- cine_snies %>%
  select(
    codigo_snies = codigo_snies_del_programa,
    nombre_programa_bdsnies = programa_academico
  ) %>%
  distinct() %>%
  mutate(from_cine_snies = TRUE) #agrega identificador de la BD donde vienen los datos

#Miramos codigos snies de los programas en la el dataframe icfes
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
#TO DO: Analizasr personas duplicadas (sea porque hicieron 2 saber pro, 2 icfes, etc)
data <- left_join(
  icfes,
  cine_snies,
  by = join_by(estu_snies_prgmacademico==codigo_snies_del_programa)
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

#Mirar codigos CINE unicos por nivel y nbc
#11 CINE amplio
#27 CINE especifico
#73 CINE detallado
data %>%
  summarise(
    n_campo_amplio = n_distinct(id_cine_campo_amplio),
    n_campo_especifico = n_distinct(id_cine_campo_especifico),
    n_campo_detallado = n_distinct(id_cine_campo_detallado),
    n_nbc = n_distinct(nucleo_basico_del_conocimiento_nbc)
  )

#Verificar si existen codigos de IES que no coinciden 
sum(data$inst_cod_institucion != data$codigo_de_la_institucion, na.rm = TRUE)

#Crear la variable ICINE que pretende ser el analogo al INBC
data <- data %>%
  mutate(icine = paste(codigo_de_la_institucion, id_cine_campo_especifico, sep = "_"))

#Crear la columna de INBC
#TO DO

############################################
#Estadisticas descriptivas de las variables
############################################

##################################
#Frecuencia SNIES
##################################

#Mirar la frecuencia de cada programa
#Nota: recordar que el programa depende de la institucion.
programas_freq <- data %>%
  count(estu_snies_prgmacademico, estu_prgm_academico, name = "frecuencia") %>%
  mutate(estu_prgm_academico = tolower(estu_prgm_academico)) %>%
  mutate(programa_label = paste0(estu_snies_prgmacademico, " - ", estu_prgm_academico))

#Obtener los top 10 programas con mayor frecuencia
top_10 <- programas_freq %>% slice_max(frecuencia, n = 10)

# Graficar top 10
ggplot(top_10, aes(x = reorder(programa_label, frecuencia), y = frecuencia)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 10 Programas", x = "Programa", y = "Frecuencia") +
  theme_minimal()

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

##################################
#Frecuencia CINE Especifico
##################################

#Mirar la frecuencia de cada cine especifico
cine_freq <- data %>%
  count(id_cine_campo_especifico, name = "frecuencia")

#Mirar los codigos CINE especificos faltantes
cines_especificos_faltantes <- setdiff(cines_especificos, cine_freq$id_cine_campo_especifico)
cines_especificos_faltantes

#Obtener los top 10 cines especificos con mayor frecuencia
top_10_cine <- cine_freq %>% slice_max(frecuencia, n = 10)

ggplot(top_10_cine, aes(x = reorder(id_cine_campo_especifico, frecuencia), y = frecuencia)) +
  geom_col(fill = "#69b3a2") +
  coord_flip() +
  geom_text(aes(label = frecuencia), hjust = -0.1, size = 4) +
  labs(title = "Top 10: Códigos CINE con mayor frecuencia",
       x = "ID CINE Campo Específico",
       y = "Frecuencia") +
  theme_minimal()


#Graficar una densidad de la frecuencia de los cines especificos
#La grafica se utiliza para tener un panorama general de las frecuencias
ggplot(cine_freq, aes(x = frecuencia)) +
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

#Seleccionar columnas de interes
columnas_regresion <- c(
  "estu_consecutivo_bdsaber11",
  "estu_consecutivo_bdsaberpro",
  "icine",
  "institucion_de_educacion_superior_ies",
  "inst_nombre_institucion",
  "estu_nucleo_pregrado",
  "estu_snies_prgmacademico",
  "nucleo_basico_del_conocimiento_nbc",
  "id_cine_campo_amplio",
  "id_cine_campo_especifico",
  "id_cine_campo_detallado",
  "punt_global_bdsaberpro",
  "punt_global_bdsaber11",
  "periodo_bdsaber11",
  "periodo_bdsaberpro",
  "dif_periodos"
)

#Seleccionar las anteriores variables y quedarnos con las filas que si tienen
#observaciones en punt_global_icfes
data_filtrado <- data %>%
  select(all_of(columnas_regresion)) %>%
  filter(
    !is.na(punt_global_bdsaber11),
    !is.na(id_cine_campo_especifico),
    dif_periodos >= 40,
    dif_periodos <= 80,
    estu_nucleo_pregrado != "MEDICINA"
  )

#Existen 54 NBC unicos (recuerdar que se omitio medicina)
#El problema con el campo NBC es que no es un id sino un string, entonces no es fiable
length(unique(data_filtrado$nucleo_basico_del_conocimiento_nbc))
length(unique(data_filtrado$estu_nucleo_pregrado))

#Existen 26 CINE especificos unicos para este ejercicio donde se excluye medicina
length(unique(data_filtrado$id_cine_campo_especifico))

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
       top_10
)

#Fijamos la BD con la que vamos a trabajar para no llamarla con cada variable
attach(data_filtrado) 

#Resumen de los datos por tipo de dato y Nans
data_filtrado %>%
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

#Tabla con frecuencia por ICINE
data_filtrado %>%
  count(icine, name = "n_observaciones") %>%
  arrange(desc(n_observaciones))  #para ver primero los más frecuentes

#Número total de valores únicos de ICINE
n_distinct(data_filtrado$icine)

#estimamos la regresion multinivel con los codigos CINE
multinivel_basico <- lme(
  punt_global_bdsaberpro ~ punt_global_bdsaber11, random = ~ 1| icine, data = data_filtrado
  )


#extraemos el listado de efectos aleatorios (valor agregado)
summary(multinivel_basico)
coeff_va <- ranef(multinivel_basico)

#visualizar el VA más directamente
multinivel_basico[["coefficients"]][["random"]] 


####################################################
#TBD
###################################################








