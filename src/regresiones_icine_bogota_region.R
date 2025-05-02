############################################################
#Calculo valor agregado
#Abril 2025 
#Autor:TBD

#Fuentes de los datos

#1.bd.csv: Base elaborada manualmente con la informacion de data icfes.  

#2. programas_bogota_nbc_cine.xlsx: https://hecaa.mineducacion.gov.co/consultaspublicas/programas
#NOTA: Es un listado con los codigos CINE F2013, NBC, SNIES de cada programa.

### La fuente 2 es el reemplazo de la fuente 3. 
#3. codigos_snies_cine_2023.csv: https://snies.mineducacion.gov.co/portal/ESTADISTICAS/Bases-consolidadas/
#Archivo csv: "Estudiantes matriculados 2023.csv"
#NOTA: El archivo viene sin duplicados de los codigos SNIES
############################################################


##################################
#Instalar librerias
#################################
# install.packages(c(
#   "readr",
#   "readxl",
#   "stringi",
#   "dplyr",
#   "tidyr",
#   "ggplot2",
#   "nlme",
#   "stringr",
#   "plotly",
#   "lme4"
# ))

##################################
#Librerias
##################################

#Cargar las librerias
library(readr) #lectura de archivos text
library(readxl) #lectura de archivos excel
library(stringi) #para ajustar nombres de las columnas de los data frames
library(dplyr) #manipulacion de dataframes
library(tidyr)
library(ggplot2) #para graficar
library(nlme) #activamos la librería/paquete que nos permite estimar el modelo multinivel
library(stringi) #facilitar la manipulacion de caracteres
library(stringr)#facilitar la manipulacion de caracteres
library(plotly) #para graficas dinamicas
library(lme4)#para la regresion de mixed models
##############################################
#Correr este bloque antes de cargar library(gt)
#install.packages("gt")
#install.packages("webshot2")  # better than webshot, works well
#webshot::install_phantomjs()
#webshot2::install_phantomjs()  # only needs to be run once
#library(gt) #para guardar tablas
##############################################

##########################################
# CONSTANTES
##########################################

columnas_regresion <- c(
  "estu_consecutivo_bdsaber11",
  "estu_consecutivo_bdsaberpro",
  "icine",
  "codigo_institucion",
  "inst_cod_institucion",
  "inst_nombre_institucion",
  "nombre_institucion",
  "estu_nucleo_pregrado",
  "estu_snies_prgmacademico",
  "nucleo_basico_del_conocimiento",
  "id_cine_campo_amplio",
  "cine_f_2013_ac_campo_amplio",
  "id_cine_campo_especifico",
  "cine_f_2013_ac_campo_especific",
  "id_cine_campo_detallado",
  "cine_f_2013_ac_campo_detallado",
  "mod_razona_cuantitat_punt",
  "punt_mate_conciliado",
  "mod_lectura_critica_punt",
  "punt_lectura_critica_conciliado",
  "punt_global_bdsaberpro",
  "punt_global_bdsaber11_conciliado",
  "periodo_bdsaber11",
  "periodo_bdsaberpro",
  "dif_periodos",
  "nombre_del_programa",
  "estado_programa",
  "nivel_de_formacion",
  "nivel_academico",
  "municipio_oferta_programa",
  "codigo_del_municipio_programa"
)

columnas_exportar = c(
  "icine",
  "icine_descrp",
  "codigo_institucion",
  "nombre_institucion",
  "estu_snies_prgmacademico",
  "nombre_del_programa",
  "municipio_oferta_programa",
  "codigo_del_municipio_programa",
  "nucleo_basico_del_conocimiento",
  "id_cine_campo_amplio",
  "cine_f_2013_ac_campo_amplio",
  "id_cine_campo_especifico",
  "cine_f_2013_ac_campo_especific",
  "id_cine_campo_detallado",
  "cine_f_2013_ac_campo_detallado",
  "estado_programa",
  "nivel_de_formacion",
  "nivel_academico",
  "n_estudiantes_icine",
  "n_estudiantes_institucion",
  "n_estudiantes_cinespecifico",
  "coeficiente_PG",
  "coeficiente_LC",
  "coeficiente_RC"
)

##########################################
# FUNCIONES
##########################################

resumen_nans <- function(df) {
  df %>%
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
}

##########################################
# DESARROLLO
##########################################

##########################################
#1. LECTURA DE LA DATA
##########################################

#Lectura de la Data:
#Saber 11
#Saber Pro
#Nombres CINE
#Codigos CINE
#Programas universitarios pregrado activos en Bogota
#ICINE
data <- read_delim("data/BD/icfes_cine.csv", escape_double = FALSE, trim_ws = TRUE)

#Seleccionar las columnas de interes
data <- data %>%
  select(all_of(columnas_regresion))

#FILTRAR POR:
#programa activo
#programa universitario
#programa pregrado
data <- data %>%
   filter(
     estado_programa == "Activo",
     nivel_de_formacion=="Universitario",
     nivel_academico=="Pregrado",
 )


#Resumen de los datos por tipo de dato y Nans
data_summary_raw <- resumen_nans(data)


##########################################
#2. APLICAR FILTROS ICFES
##########################################

#Filtros:
#su puntaje del saber pro es distinto de 0
#si una persona hizo 2 saber pro, quedarse con la observacion con el saber pro mas viejo
#eliminar los icine que tienen menos de 25 estudiantes
#Cada cine_f_2013_ac_campo_especific debe estar en almenos 5 codigo_institucion
#To do: el filtro del 40% de la poblacion total

data_filtrado <- data %>%
  filter(
    punt_global_bdsaberpro != 0,  # El puntaje global del saber pro no puede ser 0
    (
      # Filtro de ventana temporal entre la presentacion del saber pro y saber 11
      (cine_f_2013_ac_campo_detallado == "Medicina" & dif_periodos >= 40 & dif_periodos <= 90) |
        (cine_f_2013_ac_campo_detallado != "Medicina" & dif_periodos >= 40 & dif_periodos <= 80)
    )
  ) %>%
  group_by(estu_consecutivo_bdsaberpro) %>%
  slice_min(order_by = periodo_bdsaberpro, n = 1, with_ties = FALSE) %>% #quedarse con el primer saber pro
  ungroup() %>%
  group_by(icine) %>%
  filter(n() >= 25) %>% #minimo 25 estudiantes por icine
  ungroup() %>%
  group_by(cine_f_2013_ac_campo_especific) %>%
  filter(n_distinct(codigo_institucion) >= 5) %>%  #el icine debe estar en minimo 5 instituciones 
  ungroup()
  

#Resumen de los datos por tipo de dato y Nans
data_filtrado_summary <- resumen_nans(data_filtrado)

##########################################
#3. OBSERVACIONES
##########################################


#numero observaciones por icine
#346 icine distintos
observaciones_icine <- data_filtrado %>%
  group_by(icine) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#Quedan 50 NBC unicos
#Recordar que hay una categoria demas llamada "sin clasificar".
#El problema con el campo NBC es que no es un id sino un string, entonces no es fiable
length(unique(data_filtrado$nucleo_basico_del_conocimiento))
length(unique(data_filtrado$estu_nucleo_pregrado))

#Existen 16 CINE especificos unicos para este ejercicio
length(unique(data_filtrado$cine_f_2013_ac_campo_especific))

#instituciones consideradas: 80
n_distinct(data_filtrado$inst_nombre_institucion)
n_distinct(data_filtrado$codigo_institucion)

#numero programas:
n_distinct(data_filtrado$estu_snies_prgmacademico)

##########################################
#4. REGRESION
##########################################

#Fijamos la BD con la que vamos a trabajar para faciliar la llamada de cada variable
attach(data_filtrado)
# Convertimos a factor la variable icine
data_filtrado$icine <- as.factor(data_filtrado$icine)

#LA REGRESION LA EJECUTAMOS CON LMER


##########################################
#4.1 REGRESION 
#Puntaje global Saber Pro
#Puntaje global conciliado Saber 11
##########################################

# Ajustar el modelo
fit.multinivel_PG <- lmer(
  punt_global_bdsaberpro ~ punt_global_bdsaber11_conciliado + (1 | icine),
  data = data_filtrado
)

#ver resultados
summary(fit.multinivel_PG)
# Capturar el summary del modelo
summary_output_PG <- capture.output(summary(fit.multinivel_PG))
# Guardar como archivo de texto
writeLines(summary_output_PG, "output/fit_multinivel_puntajeglobal_bogotaregion_summary.txt")

#guardar los random effects i.e., el Valor Agregado
coeff_va_pg <- ranef(fit.multinivel_PG)

# Convertir los efectos aleatorios en un data.frame
coefs_pg_df <- as.data.frame(coeff_va_pg$icine)  # 'icine' es el nombre de la variable agrupadora
coefs_pg_df$icine <- rownames(coefs_pg_df)  # Agregar el nombre del grupo (icine) como una columna

#Renombrar la columna (Intercept)
#Ordenar por el valor del efecto aleatorio
coefs_pg_df <- coefs_pg_df %>%
  rename(coeficiente_PG = `(Intercept)`) %>%
  arrange(desc(coeficiente_PG))


##########################################
#4.2 REGRESION 
#Puntaje Razonamiento cuant. Saber Pro
#Puntaje global Saber 11
##########################################

# Ajustar el modelo
fit.multinivel_RC <- lmer(
  mod_razona_cuantitat_punt ~ punt_global_bdsaber11_conciliado + (1 | icine),
  data = data_filtrado
)

#ver resultados
summary(fit.multinivel_RC)
# Capturar el summary del modelo
summary_output_RC <- capture.output(summary(fit.multinivel_RC))
# Guardar como archivo de texto
writeLines(summary_output_RC, "output/fit_multinivel_razcuant_bogotaregion_summary.txt")

#guardar los random effects i.e., el Valor Agregado
coeff_va_rc <- ranef(fit.multinivel_RC)

# Convertir los efectos aleatorios en un data.frame
coefs_rc_df <- as.data.frame(coeff_va_rc$icine)  # 'icine' es el nombre de la variable agrupadora
coefs_rc_df$icine <- rownames(coefs_rc_df)  # Agregar el nombre del grupo (icine) como una columna

#Renombrar la columna (Intercept)
#Ordenar por el valor del efecto aleatorio
coefs_rc_df <- coefs_rc_df %>%
  rename(coeficiente_RC = `(Intercept)`) %>%
  arrange(desc(coeficiente_RC))


##########################################
#4.3 REGRESION 
#Puntaje Lectura critica . Saber Pro
#Puntaje global Saber 11
##########################################

# Ajustar el modelo
fit.multinivel_LC <- lmer(
  mod_lectura_critica_punt ~ punt_global_bdsaber11_conciliado + (1 | icine),
  data = data_filtrado
)

#ver resultados
summary(fit.multinivel_LC)
# Capturar el summary del modelo
summary_output_LC <- capture.output(summary(fit.multinivel_LC))
# Guardar como archivo de texto
writeLines(summary_output_LC, "output/fit_multinivel_lectcrit__bogotaregion_summary.txt")

#guardar los random effects i.e., el Valor Agregado
coeff_va_lc <- ranef(fit.multinivel_LC)

# Convertir los efectos aleatorios en un data.frame
coefs_lc_df <- as.data.frame(coeff_va_lc$icine)  # 'icine' es el nombre de la variable agrupadora
coefs_lc_df$icine <- rownames(coefs_lc_df)  # Agregar el nombre del grupo (icine) como una columna

#Renombrar la columna (Intercept)
#Ordenar por el valor del efecto aleatorio
coefs_lc_df <- coefs_lc_df %>%
  rename(coeficiente_LC = `(Intercept)`) %>%
  arrange(desc(coeficiente_LC))

##########################################
#4. Unir en un unico df cada conjunto de 
#coeficientes
##########################################

resumen_coefs <- coefs_pg_df %>%
  inner_join(coefs_rc_df, by = "icine") %>%
  inner_join(coefs_lc_df, by = "icine")

##########################################
#5. Exportacion del df
##########################################

#adicionamos al dataframe data_filtrado los resultados de la regresion
data_filtrado <- inner_join(data_filtrado,resumen_coefs, by = c("icine"))

data_filtrado <- data_filtrado %>% 
  add_count(icine, name = "n_estudiantes_icine") %>% 
  add_count(codigo_institucion, name = "n_estudiantes_institucion") %>% 
  add_count(cine_f_2013_ac_campo_especific, name = "n_estudiantes_cinespecifico")

#Crear un nuevo dataframe con observaciones unicas de icine
data_icine_unico <- data_filtrado %>%
  distinct(icine, .keep_all = TRUE) %>% arrange(desc(coeficiente_PG))

#Agregar la descripcion del icine
data_icine_unico <- data_icine_unico %>% 
  mutate(
    icine_descrp = paste(inst_nombre_institucion, cine_f_2013_ac_campo_especific, sep = " \n"),
    icine_descrp = str_to_sentence(icine_descrp)
  )

#Seleccionar las columnas de interes
data_icine_unico <- data_icine_unico %>%
  select(all_of(columnas_exportar))

#guardamos el dataframe
write_csv(data_icine_unico, "data/Resultados/va_icine_bogota_region.csv")
