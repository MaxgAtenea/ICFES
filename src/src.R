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


##################################
#Lectura de los datos
##################################

#Leer la base consolidada del Saber Pro cruzado con Saber 11
icfes <- read_delim("ICFES/data/BD/bd.csv", escape_double = FALSE, trim_ws = TRUE)


#Leer base con codigos snies y cine de los programas
cine_snies <- read_delim("ICFES/data/SNIES_CINE/codigos_snies_cine_2023.csv",
                         escape_double = FALSE,
                         trim_ws = TRUE,
                         delim=";",
                         locale = locale(encoding = "Latin1"))


#Limpiar el nombre de las columnas de cine_sines
names(cine_snies) <- names(cine_snies) %>%
  stri_trans_general("Latin-ASCII") %>%       # elimina tildes
  tolower() %>%                                # convierte a minúsculas
  gsub(" ", "_", .) %>%                        # reemplaza espacios con _
  gsub("\\(|\\)", "", .)   

cine_snies <- cine_snies %>%
  select(all_of(variables_cine_snies))

#De todas maneras verificamos que no hayan duplicados de codigos SNIES
#El numero de valores unicos coincide con el numero de observaciones en cine_snies
cine_snies %>%
  summarise(valores_unicos = n_distinct(codigo_snies_del_programa))

#Mirar cuantos valores unicos hay para los nucleos basicos del conocimiento
#en el dataframe icfes
#100 valores unicos
icfes %>%
  summarise(valores_unicos = n_distinct(estu_nucleo_pregrado))

#Filtrar por los programas que se ofrecen en Bogota
#Tener en cuenta que existe otro potencial filtro: `CÓDIGO DEL MUNICIPIO IES`
#Sin embargo, para la estimacion el ICFES distingue las regiones donde se ofrece el programa
#Discutirlo con el equipo
cine_snies <- cine_snies %>%
  filter(codigo_del_municipio_programa == 11001)


############################################
#Mirar los programas cuyos snies no cruzan
############################################


#Miramos los codigos SNIES de los programas en la el dataframe cine_snies
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
data <- inner_join(
  icfes,
  cine_snies,
  by = join_by(estu_snies_prgmacademico==codigo_snies_del_programa)
)

#Crear la columna de INBC
#TO DO

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

#Obtener los top 10 programas con mas frecuencia
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

#Obtener los top 10 programas con mas frecuencia
top_10_cine <- cine_freq %>% slice_max(frecuencia, n = 10)

ggplot(top_10_cine, aes(x = reorder(id_cine_campo_especifico, frecuencia), y = frecuencia)) +
  geom_col(fill = "#69b3a2") +
  coord_flip() +
  geom_text(aes(label = frecuencia), hjust = -0.1, size = 4) +
  labs(title = "Top 10: Códigos CINE con mayor frecuencia",
       x = "ID CINE Campo Específico",
       y = "Frecuencia") +
  theme_minimal()


#Graficar una densidad de la frecuencia de los programas
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
  "dif_periodos"
)

#Seleccionar las anteriores variables y quedarnos con las filas que si tienen
#observaciones en punt_global_icfes
data_filtrado <- data %>%
  select(all_of(columnas_regresion)) %>%
  filter(
    !is.na(punt_global_bdsaber11),
    dif_periodos >= 40,
    dif_periodos <= 80,
    estu_nucleo_pregrado != "MEDICINA"
  )


#Existen 54 NBC unicos (recuerdar que se omitio medicina)
#El problema con el campo NBC es que no es un id sino un string, entonces no es fiable
length(unique(data_filtrado$nucleo_basico_del_conocimiento_nbc))

#Existen 72 CINE detallados unicos
length(unique(data_filtrado$id_cine_campo_detallado))

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
  punt_global_bdsaberpro ~ punt_global_bdsaber11, random = ~ 1| id_cine_campo_especifico, data = data_filtrado
  )


#extraemos el listado de efectos aleatorios (valor agregado)
summary(multinivel_basico)
coeff_va <- ranef(multinivel_basico)

#visualizar el VA más directamente
multinivel_basico[["coefficients"]][["random"]] 


####################################################
#TBD
###################################################








