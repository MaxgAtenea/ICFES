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
#Set working directory
##################################
setwd("/home/alejandro/Documentos/ATENEA/Despacho/ICFES")

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
library(data.tree) #visualizar grupos anidados en forma de arbol
library(jsonlite) #guardar en formato json
library(purrr)
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
  "punt_c_naturales_conciliado",
  "punt_sociales_conciliado",
  "punt_ingles_conciliado",
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
  "codigo_del_municipio_programa",
  "ies_acreditada"
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

# Función que calcula ICC y lo une en un dataframe
calcular_ICC <- function(modelos, var_dependiente) {
  # Calcular los valores de ICC
  icc_values <- map(modelos, function(model) {
    # Extraer los componentes de la varianza
    var_comps <- as.data.frame(VarCorr(model))
    
    # Obtener la varianza a nivel de grupo (intercepto aleatorio) y la varianza residual
    var_group <- var_comps$vcov[1]  # Varianza del intercepto aleatorio
    var_residual <- attr(VarCorr(model), "sc")^2  # Varianza residual
    
    # Calcular el ICC
    icc <- var_group / (var_group + var_residual)
    
    return(icc)
  })
  
  # Crear el nombre de la columna ICC dinámicamente
  col_name <- paste0("icc_", var_dependiente)
  
  # Crear el data frame con nombres de columna dinámicos
  icc_df <- data.frame(
    cine = names(icc_values),
    valor = unlist(icc_values)
  )
  names(icc_df)[2] <- col_name
  
  return(icc_df)
}

#' Calcular varianzas condicionales de los efectos aleatorios
#'
#' Esta función extrae la varianza condicional de los interceptos aleatorios (efectos aleatorios) 
#' para cada grupo `icine` en una lista de modelos lineales mixtos ajustados con `lmer()`. 
#' Devuelve un dataframe con una fila por grupo dentro de cada modelo, incluyendo el nombre del grupo y del modelo (cine).
calcular_sd_condicional <- function(modelos, var_dependiente) {
  # Calcular las desviaciones estándar condicionales para cada modelo
  desvios <- map2(modelos, names(modelos), function(modelo, cine_nombre) {
    re <- ranef(modelo, condVar = TRUE)
    post_var <- attr(re$icine, "postVar")
    sd_cond <- sqrt(as.numeric(post_var[1, 1, ]))
    
    data.frame(
      icine = rownames(re$icine),
      cine = cine_nombre,
      valor = sd_cond
    )
  })
  
  # Unir en un solo dataframe
  desvios_df <- bind_rows(desvios)
  
  # Renombrar la columna dinámicamente
  col_name <- paste0("sd_cond_", var_dependiente)
  names(desvios_df)[names(desvios_df) == "valor"] <- col_name
  
  return(desvios_df)
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
#ICINE
#A nivel Bogota-Region
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

#Considerar saber PRO de TODOS LOS AÑOS
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
  filter(n_distinct(codigo_institucion) >= 5) %>%  #el cine debe estar en minimo 5 instituciones 
  ungroup()

#Considerar UNICAMENTE saber pro de 2023
data_filtrado_2023 <- data %>%
  filter(
    punt_global_bdsaberpro != 0,  # El puntaje global del saber pro no puede ser 0
    periodo_bdsaberpro == 20232 | periodo_bdsaberpro == 20231,
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
  filter(n_distinct(codigo_institucion) >= 5) %>%  #el cine debe estar en minimo 5 instituciones 
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

#instituciones consideradas: 79
n_distinct(data_filtrado$inst_nombre_institucion)
n_distinct(data_filtrado$codigo_institucion)

#numero programas: 909
n_distinct(data_filtrado$estu_snies_prgmacademico)


##########################################
#3. OBSERVACIONES POR ANIDACION
##########################################

#CONSIDERANDO SABER PRO TODOS LOS PERIODOS
#Numero de instituciones y personas por CINE especifico
data_filtrado %>%
  group_by(cine_f_2013_ac_campo_especific) %>%
  summarise(
    total_personas = n(),
    icine_distintos = n_distinct(icine)
  ) %>%
  arrange(desc(icine_distintos))

##Ver los icines por cada CINE y el numero de observaciones
### preparar los datos
tabla_cine <- data_filtrado %>%
  group_by(cine_f_2013_ac_campo_especific, icine) %>%
  summarise(
    n_estudiantes = n(),
    promedio_punt_saberpro = mean(punt_global_bdsaberpro),
    snies_programa = first(estu_snies_prgmacademico),
    nombre_del_programa = first(nombre_del_programa),
    .groups = "drop"
  )

#CONSIDERANDO SABER PRO 2023
#Numero de instituciones y personas por CINE especifico
data_filtrado_2023 %>%
  group_by(cine_f_2013_ac_campo_especific) %>%
  summarise(
    total_personas = n(),
    icine_distintos = n_distinct(icine)
  ) %>%
  arrange(desc(icine_distintos))

##Ver los icines por cada CINE y el numero de observaciones
### preparar los datos
tabla_cine_2023 <- data_filtrado_2023 %>%
  group_by(cine_f_2013_ac_campo_especific, icine) %>%
  summarise(
    n_estudiantes = n(),
    promedio_punt_saberpro = mean(punt_global_bdsaberpro),
    snies_programa = first(estu_snies_prgmacademico),
    nombre_del_programa = first(nombre_del_programa),
    .groups = "drop"
  )

##########################################
#4. CORRELACION
##########################################
library(knitr)

var_correlacion = c(
  "punt_global_bdsaber11_conciliado",
  "punt_mate_conciliado",
  "punt_lectura_critica_conciliado",
  "punt_c_naturales_conciliado",
  "punt_sociales_conciliado",
  "punt_ingles_conciliado",
  "mod_razona_cuantitat_punt", 
  "mod_lectura_critica_punt",
  "punt_global_bdsaberpro"
)

# Calcular la matriz de correlación todos los años saber pro
correlation_matrix <- cor(data_filtrado %>% select (all_of(var_correlacion)), use = "complete.obs")  # usa "complete.obs" para ignorar NA
# Ver la matriz de correlación
kable(correlation_matrix, caption = "Correlation Matrix")

# Calcular la matriz de correlación saber pro 2023
correlation_matrix_2023 <- cor(data_filtrado_2023 %>% select (all_of(var_correlacion)), use = "complete.obs")  # usa "complete.obs" para ignorar NA
# Ver la matriz de correlación
kable(correlation_matrix_2023, caption = "Correlation Matrix")

##########################################
#5. REGRESION
##########################################

##########################################
#5.1 REGRESION con el Saber PRO de todos 
#los periodos
##########################################

#Fijamos la BD con la que vamos a trabajar para faciliar la llamada de cada variable
attach(data_filtrado)
# Convertimos a factor la variable icine
data_filtrado$icine <- as.factor(data_filtrado$icine)
data_filtrado$cine_f_2013_ac_campo_especific <- as.factor(data_filtrado$cine_f_2013_ac_campo_especific)

#LA REGRESION LA EJECUTAMOS CON LMER

##########################################
#5.1.1 REGRESION para cada CINE especifico
#Puntaje global Saber Pro
#Puntaje global conciliado Saber 11
##########################################

# Lista de modelos por grupo CINE
modelos_por_cine <- data_filtrado %>%
  split(.$cine_f_2013_ac_campo_especific) %>%
  map(~ lmer(
    punt_global_bdsaberpro ~ 
      punt_mate_conciliado +
      punt_lectura_critica_conciliado +
      punt_c_naturales_conciliado +
      punt_sociales_conciliado + 
      punt_ingles_conciliado +
      (1 | icine)
    , data = .x)
    )

# Resumen de cada modelo
resumenes <- map(modelos_por_cine, summary)

# Efectos aleatorios (valor agregado) de cada modelo
valores_agregados <- map2(
  modelos_por_cine,
  names(modelos_por_cine),
  ~ ranef(.x)$icine %>%
    as.data.frame() %>%
    mutate(icine = rownames(ranef(.x)$icine),
           cine = .y) %>%
    rename(coeficiente_PG = `(Intercept)`)
)

# Unir todos los valores agregados en un solo data.frame ordenado
coefs_pg_df <- bind_rows(valores_agregados) %>%
  arrange(desc(coeficiente_PG))

# Seleccionar icine, nombre_institucion y codigo_institucion únicos desde el dataset original
info_instituciones <- data_filtrado %>%
  select(icine, nombre_institucion, codigo_institucion) %>%
  distinct()

coefs_pg_df <- coefs_pg_df %>%
  left_join(info_instituciones, by = c("icine" = "icine"))

coefs_pg_df <- coefs_pg_df %>%
  left_join(
    tabla_cine %>% select(icine, n_estudiantes,snies_programa,nombre_del_programa, promedio_punt_saberpro),
    by = "icine"
  )

desviaciones_condicionales_df <- calcular_sd_condicional(modelos_por_cine, "pg")

coefs_pg_df <- coefs_pg_df %>%
  left_join(desviaciones_condicionales_df, by = c("icine", "cine"))



##########################################
#5.1.2 REGRESION para cada CINE especifico
#Puntaje Razonamiento cuant. Saber Pro
#Puntaje global Saber 11
##########################################

# Lista de modelos por grupo CINE
modelos_por_cine <- data_filtrado %>%
  split(.$cine_f_2013_ac_campo_especific) %>%
  map(~ lmer(
    mod_razona_cuantitat_punt ~ 
      punt_mate_conciliado +
      punt_lectura_critica_conciliado +
      punt_c_naturales_conciliado +
      punt_sociales_conciliado + 
      punt_ingles_conciliado +
      (1 | icine)
    , data = .x)
  )

# Resumen de cada modelo
resumenes <- map(modelos_por_cine, summary)

# Efectos aleatorios (valor agregado) de cada modelo
valores_agregados <- map2(
  modelos_por_cine,
  names(modelos_por_cine),
  ~ ranef(.x)$icine %>%
    as.data.frame() %>%
    mutate(icine = rownames(ranef(.x)$icine),
           cine = .y) %>%
    rename(coeficiente_RC = `(Intercept)`)
)

# Unir todos los valores agregados en un solo data.frame ordenado
coefs_rc_df <- bind_rows(valores_agregados) %>%
  arrange(desc(coeficiente_RC))

# Seleccionar icine, nombre_institucion y codigo_institucion únicos desde el dataset original
info_instituciones <- data_filtrado %>%
  select(icine, nombre_institucion, codigo_institucion) %>%
  distinct()

coefs_rc_df <- coefs_rc_df %>%
  left_join(info_instituciones, by = c("icine" = "icine"))

coefs_rc_df <- coefs_rc_df %>%
  left_join(
    tabla_cine %>% select(icine, n_estudiantes,snies_programa,nombre_del_programa, promedio_punt_saberpro),
    by = "icine"
  )

desviaciones_condicionales_df <- calcular_sd_condicional(modelos_por_cine, "rc")

coefs_rc_df <- coefs_rc_df %>%
  left_join(desviaciones_condicionales_df, by = c("icine", "cine"))

##########################################
#5.1.3 REGRESION para cada CINE especifico
#Puntaje Lectura Critica Saber Pro
#Puntaje global Saber 11
##########################################

# Lista de modelos por grupo CINE
modelos_por_cine <- data_filtrado %>%
  split(.$cine_f_2013_ac_campo_especific) %>%
  map(~ lmer(
    mod_lectura_critica_punt ~ 
      punt_mate_conciliado +
      punt_lectura_critica_conciliado +
      punt_c_naturales_conciliado +
      punt_sociales_conciliado + 
      punt_ingles_conciliado +
      (1 | icine)
    , data = .x)
  )

# Resumen de cada modelo
resumenes <- map(modelos_por_cine, summary)

# Efectos aleatorios (valor agregado) de cada modelo
valores_agregados <- map2(
  modelos_por_cine,
  names(modelos_por_cine),
  ~ ranef(.x)$icine %>%
    as.data.frame() %>%
    mutate(icine = rownames(ranef(.x)$icine),
           cine = .y) %>%
    rename(coeficiente_LC = `(Intercept)`)
)

# Unir todos los valores agregados en un solo data.frame ordenado
coefs_lc_df <- bind_rows(valores_agregados) %>%
  arrange(desc(coeficiente_LC))

# Seleccionar icine, nombre_institucion y codigo_institucion únicos desde el dataset original
info_instituciones <- data_filtrado %>%
  select(icine, nombre_institucion, codigo_institucion) %>%
  distinct()

coefs_lc_df <- coefs_lc_df %>%
  left_join(info_instituciones, by = c("icine" = "icine"))

coefs_lc_df <- coefs_lc_df %>%
  left_join(
    tabla_cine %>% select(icine, n_estudiantes,snies_programa,nombre_del_programa, promedio_punt_saberpro),
    by = "icine"
  )

desviaciones_condicionales_df <- calcular_sd_condicional(modelos_por_cine, "lc")

coefs_lc_df <- coefs_lc_df %>%
  left_join(desviaciones_condicionales_df, by = c("icine", "cine"))

###############################################################################
##########################################
#5.2 REGRESION con el Saber PRO del 2023
##########################################
#Desfijamos la BD de la seccion 5.1
detach(data_filtrado)

#Fijamos la BD con la que vamos a trabajar para faciliar la llamada de cada variable
attach(data_filtrado_2023)
# Convertimos a factor la variable icine
data_filtrado_2023$icine <- as.factor(data_filtrado_2023$icine)
data_filtrado_2023$cine_f_2013_ac_campo_especific <- as.factor(data_filtrado_2023$cine_f_2013_ac_campo_especific)

#LA REGRESION LA EJECUTAMOS CON LMER

##########################################
#5.2.1 REGRESION para cada CINE especifico
#SABER PRO 2023
#Puntaje global Saber Pro
#Puntaje global conciliado Saber 11
##########################################

# Lista de modelos por grupo CINE
modelos_por_cine <- data_filtrado_2023 %>%
  split(.$cine_f_2013_ac_campo_especific) %>%
  map(~ lmer(
    punt_global_bdsaberpro ~ 
      punt_mate_conciliado +
      punt_lectura_critica_conciliado +
      punt_c_naturales_conciliado +
      punt_sociales_conciliado + 
      punt_ingles_conciliado +
      (1 | icine)
    , data = .x)
  )

# Resumen de cada modelo
resumenes <- map(modelos_por_cine, summary)

# Efectos aleatorios (valor agregado) de cada modelo
valores_agregados <- map2(
  modelos_por_cine,
  names(modelos_por_cine),
  ~ ranef(.x)$icine %>%
    as.data.frame() %>%
    mutate(icine = rownames(ranef(.x)$icine),
           cine = .y) %>%
    rename(coeficiente_PG = `(Intercept)`)
)

# Unir todos los valores agregados en un solo data.frame ordenado
coefs_pg_df_2023 <- bind_rows(valores_agregados) %>%
  arrange(desc(coeficiente_PG))

# Seleccionar icine, nombre_institucion y codigo_institucion únicos desde el dataset original
info_instituciones <- data_filtrado_2023 %>%
  select(icine, nombre_institucion, codigo_institucion) %>%
  distinct()

coefs_pg_df_2023 <- coefs_pg_df_2023 %>%
  left_join(info_instituciones, by = c("icine" = "icine"))

coefs_pg_df_2023 <- coefs_pg_df_2023 %>%
  left_join(
    tabla_cine_2023 %>% select(icine, n_estudiantes,snies_programa,nombre_del_programa, promedio_punt_saberpro),
    by = "icine"
  )

desviaciones_condicionales_df <- calcular_sd_condicional(modelos_por_cine, "pg")

coefs_pg_df_2023 <- coefs_pg_df_2023 %>%
  left_join(desviaciones_condicionales_df, by = c("icine", "cine"))

##########################################
#5.2.2 REGRESION para cada CINE especifico
#SABER PRO 2023
#Puntaje Razonamiento cuant. Saber Pro
#Puntaje global Saber 11
##########################################

# Lista de modelos por grupo CINE
modelos_por_cine <- data_filtrado_2023 %>%
  split(.$cine_f_2013_ac_campo_especific) %>%
  map(~ lmer(
    mod_razona_cuantitat_punt ~ 
      punt_mate_conciliado +
      punt_lectura_critica_conciliado +
      punt_c_naturales_conciliado +
      punt_sociales_conciliado + 
      punt_ingles_conciliado +
      (1 | icine)
    , data = .x)
  )

# Resumen de cada modelo
resumenes <- map(modelos_por_cine, summary)

# Efectos aleatorios (valor agregado) de cada modelo
valores_agregados <- map2(
  modelos_por_cine,
  names(modelos_por_cine),
  ~ ranef(.x)$icine %>%
    as.data.frame() %>%
    mutate(icine = rownames(ranef(.x)$icine),
           cine = .y) %>%
    rename(coeficiente_RC = `(Intercept)`)
)

# Unir todos los valores agregados en un solo data.frame ordenado
coefs_rc_df_2023 <- bind_rows(valores_agregados) %>%
  arrange(desc(coeficiente_RC))

# Seleccionar icine, nombre_institucion y codigo_institucion únicos desde el dataset original
info_instituciones <- data_filtrado_2023 %>%
  select(icine, nombre_institucion, codigo_institucion) %>%
  distinct()

coefs_rc_df_2023 <- coefs_rc_df_2023 %>%
  left_join(info_instituciones, by = c("icine" = "icine"))

coefs_rc_df_2023 <- coefs_rc_df_2023 %>%
  left_join(
    tabla_cine_2023 %>% select(icine, n_estudiantes,snies_programa,nombre_del_programa, promedio_punt_saberpro),
    by = "icine"
  )

desviaciones_condicionales_df <- calcular_sd_condicional(modelos_por_cine, "rc")

coefs_rc_df_2023 <- coefs_rc_df_2023 %>%
  left_join(desviaciones_condicionales_df, by = c("icine", "cine"))

##########################################
#5.2.3 REGRESION para cada CINE especifico
# SABER PRO 2023
#Puntaje Lectura Critica Saber Pro
#Puntaje global Saber 11
##########################################

# Lista de modelos por grupo CINE
modelos_por_cine <- data_filtrado_2023 %>%
  split(.$cine_f_2013_ac_campo_especific) %>%
  map(~ lmer(
    mod_lectura_critica_punt ~ 
      punt_mate_conciliado +
      punt_lectura_critica_conciliado +
      punt_c_naturales_conciliado +
      punt_sociales_conciliado + 
      punt_ingles_conciliado +
      (1 | icine)
    , data = .x)
  )

# Resumen de cada modelo
resumenes <- map(modelos_por_cine, summary)

# Efectos aleatorios (valor agregado) de cada modelo
valores_agregados <- map2(
  modelos_por_cine,
  names(modelos_por_cine),
  ~ ranef(.x)$icine %>%
    as.data.frame() %>%
    mutate(icine = rownames(ranef(.x)$icine),
           cine = .y) %>%
    rename(coeficiente_LC = `(Intercept)`)
)

# Unir todos los valores agregados en un solo data.frame ordenado
coefs_lc_df_2023 <- bind_rows(valores_agregados) %>%
  arrange(desc(coeficiente_LC))

# Seleccionar icine, nombre_institucion y codigo_institucion únicos desde el dataset original
info_instituciones <- data_filtrado_2023 %>%
  select(icine, nombre_institucion, codigo_institucion) %>%
  distinct()

coefs_lc_df_2023 <- coefs_lc_df_2023 %>%
  left_join(info_instituciones, by = c("icine" = "icine"))

coefs_lc_df_2023 <- coefs_lc_df_2023 %>%
  left_join(
    tabla_cine_2023 %>% select(icine, n_estudiantes,snies_programa,nombre_del_programa, promedio_punt_saberpro),
    by = "icine"
  )

desviaciones_condicionales_df <- calcular_sd_condicional(modelos_por_cine, "lc")

coefs_lc_df_2023 <- coefs_lc_df_2023 %>%
  left_join(desviaciones_condicionales_df, by = c("icine", "cine"))


##########################################
#6. Unir los coeficientes de va en un
#unico df
##########################################

#Valores agregados con base en todos los periodos del Saber PRO
resumen_coefs <- coefs_pg_df %>%
  inner_join(coefs_rc_df %>% select(icine, coeficiente_RC, sd_cond_rc), by = "icine") %>%
  inner_join(coefs_lc_df %>% select(icine, coeficiente_LC, sd_cond_lc), by = "icine") %>% 
  relocate(coeficiente_PG, coeficiente_LC, coeficiente_RC, .after = last_col())

#Valores agregados con base en 2023 del Saber PRO
resumen_coefs_2023 <- coefs_pg_df_2023 %>%
  inner_join(coefs_rc_df_2023 %>% select(icine, coeficiente_RC, sd_cond_rc), by = "icine") %>%
  inner_join(coefs_lc_df_2023 %>% select(icine, coeficiente_LC, sd_cond_lc), by = "icine") %>%
  relocate(coeficiente_PG, coeficiente_LC, coeficiente_RC, .after = last_col())


##########################################
#7. Exportacion de los resultados
##########################################
#Exportar la info
write_csv(resumen_coefs, "data/Resultados/va_icine_bogota_region.csv")
write_csv(resumen_coefs_2023, "data/Resultados/va_icine_bogota_region_2023.csv")


##########################################
#8. Exportacion de la data empleada en 
# la estimacion
##########################################
write_csv(data_filtrado, "data/BD/data_estimacion_todos_periodos.csv")
write_csv(data_filtrado_2023, "data/BD/data_estimacion_2023.csv")

###############################################################################




