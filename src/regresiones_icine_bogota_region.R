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

# Usando lapply base R
lapply(c("readr",
         "readxl",
         "stringi",
         "dplyr", "tidyr",
         "ggplot2",
         "nlme",
         "stringr",
         "plotly",
         "lme4",
         "data.tree",
         "jsonlite",
         "purrr"),
       library,
       character.only = TRUE
       )

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
  'inbc',
  'icine_amplio',
  "icine",
  'icine_detall',
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
  "anio_presentacion_sb11",
  "periodo_bdsaberpro",
  "anio_presentacion_sbpro",
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


nivel_agregacion <- list(
  cine_amplio = list(cine_col = "cine_f_2013_ac_campo_amplio", icine_col = "icine_amplio"),
  cine_especifico = list(cine_col = "cine_f_2013_ac_campo_especific", icine_col = "icine"),
  cine_detallado = list(cine_col = "cine_f_2013_ac_campo_detallado", icine_col = "icine_detall"),
  nbc = list(cine_col = "nucleo_basico_del_conocimiento", icine_col = "inbc")
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


#' Filtrar y depurar datos según criterios del ICFES
#'
#' Esta función toma la base consolidada del icfes junto con la informacion de los programas
#' y realiza los filtros que el icfes hace en su metodología del VA:
#' 
#' - El puntaje global del Saber Pro debe ser distinto de cero.
#' - Se selecciona solo el primer resultado del Saber Pro por estudiante.
#' - Se considera una ventana temporal válida entre la presentación del Saber 11 y Saber Pro:
#'     - Para Medicina: entre 40 y 90 periodos de diferencia.
#'     - Para otros programas: entre 40 y 80 periodos.
#' - Se requiere un mínimo de 25 estudiantes por categoría `icine`.
#' - Se conservan solo los campos específicos (`cine_f_2013_ac_campo_especific`) 
#'   con presencia en al menos 5 instituciones distintas.
#'
#' @param data Un data frame con los datos completos.
#' @param tipo_cine_col Nivel CINE que se está considerando
#' @param año (Opcional) Año específico de presentación del Saber Pro a filtrar. Si es NULL, no se aplica este filtro.
#' @return Un data frame filtrado según los criterios establecidos.
filtros_icfes <- function(data, nivel_cine, anios = NULL) {
  temp_icine <-  nivel_agregacion[[nivel_cine]]$icine_col
  temp_cine <-  nivel_agregacion[[nivel_cine]]$cine_col
  data_filtrada <- data %>%
    # Filtrar por uno o varios años, si se especifican
    { if (!is.null(anios)) filter(., anio_presentacion_sbpro %in% anios) else . } %>%
    
    # Filtros fijos
    filter(
      punt_global_bdsaberpro != 0,
      (
        (cine_f_2013_ac_campo_detallado == "Medicina" & dif_periodos >= 40 & dif_periodos <= 90) |
          (cine_f_2013_ac_campo_detallado != "Medicina" & dif_periodos >= 40 & dif_periodos <= 80)
      )
    ) %>%
    
    # Quedarse con el primer Saber Pro por estudiante
    group_by(estu_consecutivo_bdsaberpro) %>%
    slice_min(order_by = periodo_bdsaberpro, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    
    # Filtro por mínimo 25 estudiantes por ICINE
    group_by(!!sym(temp_icine)) %>%
    filter(n() >= 25) %>%
    ungroup() %>%
    
    # Filtro por mínimo 5 instituciones por nivel_cine
    group_by(!!sym(temp_cine)) %>%
    filter(n_distinct(codigo_institucion) >= 5) %>%
    ungroup()
}



# ------------------------------------------------------------
# Función: resumir_por_cine
# Descripción: 
# Resume el dataframe agrupando por dos columnas dadas, calculando
# el número de estudiantes, el promedio de 'punt_global' y 
# valores representativos de columnas clave.
# ------------------------------------------------------------
promedio_sbpro_por_cine <- function(df, cine, icine) {
  df %>%
    group_by(across(all_of(c(cine, icine)))) %>%
    summarise(
      n_estudiantes = n(),
      promedio_punt_saberpro = mean(punt_global_bdsaberpro),
      nombre_institucion = first(nombre_institucion),
      snies_programas = paste(unique(estu_snies_prgmacademico), collapse = "; "),
      nombres_programas = paste(unique(nombre_del_programa), collapse = "; "),
      .groups = "drop"
    )
}

#' Resumen de icines y numero estudiantes por campo CINE
#'
#' Esta función agrupa los datos por un campo CINE (por defecto, el campo específico)
#' y resume el número total de personas y la cantidad de icine distintos.
#'
#' @param data Dataframe a analizar
#' @param columna_cine Nombre de la columna de agrupación CINE
#' @return Un dataframe con los totales de personas y de instituciones distintas por campo CINE
observaciones_por_cine <- function(data, columna_cine) {
  data %>%
    group_by(.data[[columna_cine]]) %>%
    summarise(
      total_personas = n(),
      icine_distintos = n_distinct(icine),
      .groups = "drop"
    ) %>%
    arrange(desc(icine_distintos))
}


#' Ajusta modelos lineales mixtos por grupo CINE y extrae efectos aleatorios por ICINE
#'
#' Esta función ajusta modelos lineales mixtos usando `lmer()` para cada grupo definido 
#' por una columna CINE (por ejemplo, campo específico, amplio o detallado). Cada modelo 
#' predice una variable dependiente a partir de puntajes de áreas del Saber 11 como 
#' predictores fijos y un intercepto aleatorio por icine. 
#'
#' A partir de los modelos ajustados, la función extrae los efectos aleatorios institucionales 
#' (valor agregado), los une con información institucional básica y, opcionalmente, con una tabla 
#' externa de resumen por icine. Además, calcula la desviación estándar condicional para 
#' cada modelo.
#'
#' @param data Data frame con la información completa.
#' @param cine_col String con el nombre de la columna que define los grupos CINE (por ejemplo, "cine_f_2013_ac_campo_especific").
#' @param dep_var String con el nombre de la variable dependiente de la regresión (por ejemplo, "punt_global_bdsaberpro").
#' @param icine_col String con el nombre de la columna que representa la institución (por ejemplo, "icine", "icine_amplio").
#' @param nombre_intercepto Nombre que se asignará a la columna del intercepto en el output final (por defecto, "coeficiente_PG").
#' @param sufijo Sufijo usado para nombrar columnas en el cálculo de desviación condicional (por ejemplo, "pg").
#' @param tabla_cine (Opcional) Data frame con resumen por institución para unir al resultado final.
#'
#' @return Un data frame que contiene: efectos aleatorios por institución, nombre y código de institución,
#'         información complementaria (si se proporciona `tabla_cine`) y desviaciones estándar condicionales.
ajustar_efectos_mixtos_por_cine <- function(
    data,
    cine_col,               # Nombre de la columna que representa el tipo de CINE (amplio, específico, detallado)
    dep_var,                # Variable dependiente de la regresión (por ejemplo, punt_global_bdsaberpro)
    icine_col,              # Columna que representa el agrupador aleatorio (icine, icine_amplio, etc.)
    nombre_intercepto = "coeficiente_PG", # Nombre que se le dará a la columna del intercepto en el output
    sufijo = "pg"        # Sufijo usado en calcular_sd_condicional para identificar el tipo de variable
) {
  
  # Convertir las columnas de agrupación a factores
  data[[icine_col]] <- as.factor(data[[icine_col]])
  data[[cine_col]] <- as.factor(data[[cine_col]])
  
  # 1. Ajustar modelos mixtos lineales por cada grupo definido en cine_col
  modelos_por_cine <- data %>%
    split(.[[cine_col]]) %>%  # Dividir los datos por el tipo de cine
    map(~ lmer(
      formula = as.formula( # Armar la fórmula de manera dinámica usando la variable dependiente
        paste0(dep_var, " ~ punt_mate_conciliado + punt_lectura_critica_conciliado + ",
               "punt_c_naturales_conciliado + punt_sociales_conciliado + ",
               "punt_ingles_conciliado + (1 | ", icine_col, ")")
      ),
      data = .x
    ))
  
  # 2. Extraer los efectos aleatorios del agrupador (icine) para cada modelo (recuerde que es un modelo por CINE)
  valores_agregados <- map2(
    modelos_por_cine,
    names(modelos_por_cine),
    ~ ranef(.x)[[icine_col]] %>%              # Extrae los efectos aleatorios de cada modelo
      as.data.frame() %>%
      mutate(
        !!icine_col := rownames(ranef(.x)[[icine_col]]), # Añadir columna icine con nombre de la fila
        cine = .y                                        # Añadir el valor del grupo cine actual
      ) %>%
      rename(!!nombre_intercepto := `(Intercept)`)       # Renombrar el intercepto
  )
  
  # 3. Combinar todos los efectos aleatorios en un único data frame
  coefs_df <- bind_rows(valores_agregados) %>%
    arrange(desc(!!sym(nombre_intercepto)))  # Ordenar por el valor del efecto aleatorio
  
  # 4. Unir con información institucional básica (nombre y código)
  info_instituciones <- data %>%
    select(all_of(c(icine_col, "nombre_institucion", "codigo_institucion"))) %>%
    distinct()
  
  coefs_df <- coefs_df %>%
    left_join(info_instituciones, by = icine_col)

  #promedio puntaje saber pro, n_estudiantes, programas y nombre de programas por icine
  tabla_cine <- promedio_sbpro_por_cine(data, cine_col, icine_col)
  
  columnas_unicas <- setdiff(names(tabla_cine), names(coefs_df))
  columnas_a_unir <- unique(c(icine_col, columnas_unicas))
  
  coefs_df <- coefs_df %>%
    left_join(tabla_cine %>% select(all_of(columnas_a_unir)), by = icine_col)
  
  # 6. Calcular y unir la desviación estándar condicional del modelo
  #desviaciones_condicionales_df <- calcular_sd_condicional(modelos_por_cine, sufijo)
  
  #coefs_df <- coefs_df %>%
    #left_join(desviaciones_condicionales_df, by = c(icine_col, icine_col))
  
  # 7. Retornar el data frame final 
  coefs_df <- coefs_df %>%
    select(
      all_of(c(
        cine_col,
        icine_col,
        "nombre_institucion",
        "codigo_institucion",
        "snies_programas",
        "nombres_programas",
        "n_estudiantes",
        "promedio_punt_saberpro",
        nombre_intercepto
      ))
    )  %>% arrange(!!sym(cine_col), desc(!!sym(nombre_intercepto)))


  return(coefs_df)
}

#' Genera los valores agregados de puntaje global, razonamiento cuant, lectura critca
#'
#' Ajusta modelos de efectos mixtos para tres variables dependientes y devuelve los coeficientes por icine,
#' agrupando según el tipo de CINE y filtrando opcionalmente por años.
#'
#' @param data Data frame con los datos originales.
#' @param tipo_cine Tipo de agrupación CINE: "cine_amplio", "cine_especifico" o "cine_detallado".
#' @param anios Vector o valor único de años para filtrar la data; NULL para no filtrar.
#'
#' @return Data frame con coeficientes agregados por institución y, si se indica, columna 'periodo' con los años usados.
generar_valores_agregados <- function(data_filtrado, tipo_cine , anios = NULL) {
  
  # Verificar que el tipo_cine sea válido
  if (!tipo_cine %in% names(nivel_agregacion)) {
    stop("El tipo de cine especificado no es válido.")
  }
  
  # Extraer columnas desde la constante global
  cine_col <- nivel_agregacion[[tipo_cine]]$cine_col
  icine_col <- nivel_agregacion[[tipo_cine]]$icine_col
  
  # Ejecutar modelos mixtos para tres variables distintas
  resultados_pg <- ajustar_efectos_mixtos_por_cine(
    data = data_filtrado,
    cine_col = cine_col,
    icine_col = icine_col,
    dep_var = "punt_global_bdsaberpro",
    nombre_intercepto = "coeficiente_PG",
    sufijo = "pg"
  )
  
  resultados_rc <- ajustar_efectos_mixtos_por_cine(
    data = data_filtrado,
    cine_col = cine_col,
    icine_col = icine_col,
    dep_var = "mod_razona_cuantitat_punt",
    nombre_intercepto = "coeficiente_RC",
    sufijo = "rc"
  )
  
  resultados_lc <- ajustar_efectos_mixtos_por_cine(
    data = data_filtrado,
    cine_col = cine_col,
    icine_col = icine_col,
    dep_var = "mod_lectura_critica_punt",
    nombre_intercepto = "coeficiente_LC",
    sufijo = "lc"
  )
  
  # Unir los resultados de efectos aleatorios por institución
  resumen_coefs <- resultados_pg %>%
    inner_join(resultados_rc %>% select(!!icine_col, coeficiente_RC), by = icine_col) %>%
    inner_join(resultados_lc %>% select(!!icine_col, coeficiente_LC), by = icine_col) %>%
    relocate(coeficiente_PG, coeficiente_LC, coeficiente_RC, .after = last_col())
  
  if (!is.null(anios)) {
    resumen_coefs <- resumen_coefs %>%
      mutate(periodo = paste(anios, collapse = "-"))
  }
  
  return(resumen_coefs)
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

#Resumen de los datos por tipo de dato y Nans
data_summary_raw <- resumen_nans(data)

##########################################
#2. APLICAR FILTROS ICFES
# Deprecated (ya lo hace la funcion generar_valores_agregados)
##########################################
# 
# #Considera todas las observaciones del saber pro
# data_filtrado <- filtros_icfes(data)
# 
# #Resumen de los datos por tipo de dato y Nans
# data_filtrado_summary <- resumen_nans(data_filtrado)

##########################################
#3. OBSERVACIONES
# Deprecated
##########################################

# #numero observaciones por icine
# #346 icine distintos
# observaciones_icine <- data_filtrado %>%
#   group_by(icine) %>%
#   summarise(count = n()) %>%
#   arrange(desc(count))
# 
# #Quedan 50 NBC unicos
# #Recordar que hay una categoria demas llamada "sin clasificar".
# #El problema con el campo NBC es que no es un id sino un string, entonces no es fiable
# length(unique(data_filtrado$nucleo_basico_del_conocimiento))
# length(unique(data_filtrado$estu_nucleo_pregrado))
# 
# #Existen 16 CINE especificos unicos para este ejercicio
# length(unique(data_filtrado$cine_f_2013_ac_campo_especific))
# 
# #instituciones consideradas: 79
# n_distinct(data_filtrado$inst_nombre_institucion)
# n_distinct(data_filtrado$codigo_institucion)
# 
# #numero programas: 909
# n_distinct(data_filtrado$estu_snies_prgmacademico)


##########################################
#3. OBSERVACIONES POR ANIDACION CINE
##########################################

#Numero de icines y personas por CINE especifico
n_observaciones_cine =  observaciones_por_cine(data_filtrado, "cine_f_2013_ac_campo_especific")

#Numero de icines y personas por CINE especifico
n_observaciones_cine_2023 =  observaciones_por_cine(data_filtrado_2023, "cine_f_2013_ac_campo_especific")

##########################################
#4. CORRELACION de las variables
# TODO : Reformular
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
#5. Generar y exportar valores agregados
##########################################

# Definir los periodos, incluyendo el total 2016-2023
periodos <- list(
  #`2016_2023` = 2016:2023,
  `2022_2023` = c(2022, 2023),
  `2021_2022` = c(2021, 2022),
  `2020_2021` = c(2020, 2021),
  `2019_2020` = c(2019, 2020),
  `2018_2019` = c(2018, 2019),
  `2017_2018` = c(2017, 2018),
  `2016_2017` = c(2016, 2017)
)

# Definir tipos de cine a iterar
#niveles_cine <- c("cine_amplio", "cine_especifico", "cine_detallado", "nbc")

niveles_cine <- c("nbc")

periodos <- list(
  `2021_2022` = c(2021, 2022)
)

guardar_valores_agregados <- function(data, periodos, niveles_cine) {
  for (nombre_periodo in names(periodos)) {
    periodo <- periodos[[nombre_periodo]]
    print(periodo)
    
    for (nivel_cine in niveles_cine) {
      print(nivel_cine)
      
      data_filtrado <- filtros_icfes(data, nivel_cine, periodo)
      
      # Generar los valores agregados
      va_resultado <- generar_valores_agregados(data_filtrado, nivel_cine, anios = periodo)
      
      # Construir la ruta de guardado
      ruta_directorio <- file.path("data/Resultados_VA", nivel_cine, "bogota_region")
      dir.create(ruta_directorio, recursive = TRUE, showWarnings = FALSE)
      
      # Ruta del archivo
      ruta_csv <- file.path(ruta_directorio, paste0("va_", nivel_cine, "_", nombre_periodo, ".csv"))
      
      # Guardar el resultado
      write_csv(va_resultado, ruta_csv)
    }
  }
}

# Ejecutar la función
guardar_valores_agregados(data, periodos, niveles_cine)



