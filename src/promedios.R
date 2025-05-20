#La informacion del saber tyt proviene de dataicfes
#la informacion de los programas de tyt proviene de https://hecaa.mineducacion.gov.co/consultaspublicas/programas

##################################
#Set working directory
##################################
setwd("/home/alejandro/Documentos/ATENEA/Despacho/ICFES")


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

##################################
#Constantes
##################################

# Lista con los pares de columnas y nombres para cada resumen
pares_cines <- list(
  list(cine = "cine_f_2013_ac_campo_amplio", icine = "icine_amplio"),
  list(cine = "cine_f_2013_ac_campo_especific", icine = "icine_spec"),
  list(cine = "cine_f_2013_ac_campo_detallado", icine = "icine_detall")
)

##################################
#Funciones
##################################

# Rsumir nans por cada variables
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

# ------------------------------------------------------------
# Función: detectar_valores_no_emparejados
# Descripción: 
# Compara dos columnas de distintas tablas y detecta los valores 
# únicos en la columna de referencia que no tienen correspondencia 
# en la columna de comparación. Opcionalmente, retorna las filas 
# completas de la tabla de referencia afectadas.
# ------------------------------------------------------------
detectar_valores_no_emparejados <- function(
    tabla_referencia,          # Ej: base_sabertyt
    tabla_comparacion,         # Ej: programas_tyt
    columna_referencia,        # Ej: "estu_snies_prgmacademico"
    columna_comparacion,       # Ej: "codigo_snies_del_programa"
    incluir_filas_afectadas = FALSE
) {
  # Obtener valores únicos
  valores_referencia <- unique(tabla_referencia[[columna_referencia]])
  valores_comparacion <- unique(tabla_comparacion[[columna_comparacion]])
  
  # Detectar valores sin correspondencia en cada dirección
  valores_sin_match_ref <- setdiff(valores_referencia, valores_comparacion)
  valores_sin_match_comp <- setdiff(valores_comparacion, valores_referencia)
  
  # Construir resultado básico
  resultado <- list(
    valores_sin_correspondencia_referencia = valores_sin_match_ref,
    cantidad_valores_sin_correspondencia_referencia = length(valores_sin_match_ref),
    
    valores_sin_correspondencia_comparacion = valores_sin_match_comp,
    cantidad_valores_sin_correspondencia_comparacion = length(valores_sin_match_comp)
  )
  
  # Si se quiere incluir las filas completas afectadas de tabla_referencia
  if (incluir_filas_afectadas) {
    filas_afectadas_ref <- tabla_referencia[tabla_referencia[[columna_referencia]] %in% valores_sin_match_ref, ]
    filas_afectadas_comp <- tabla_comparacion[tabla_comparacion[[columna_comparacion]] %in% valores_sin_match_comp, ]
    
    resultado$cantidad_filas_afectadas_referencia <- nrow(filas_afectadas_ref)
    resultado$filas_afectadas_referencia <- filas_afectadas_ref
    
    resultado$cantidad_filas_afectadas_comparacion <- nrow(filas_afectadas_comp)
    resultado$filas_afectadas_comparacion <- filas_afectadas_comp
  }
  
  return(resultado)
}


# ------------------------------------------------------------
# Función: resumir_por_cine
# Descripción: 
# Resume el dataframe agrupando por dos columnas dadas, calculando
# el número de estudiantes, el promedio de 'punt_global' y 
# valores representativos de columnas clave.
# Se filtra por nivel de formacion:Tecnologico o Formacion Tecnica Profesional
# ------------------------------------------------------------
resumir_por_cine <- function(df, cine, icine, nivel_filtrado = NULL) {
  if (!is.null(nivel_filtrado)) {
    df <- df %>% filter(nivel_de_formacion %in% nivel_filtrado)
  }
  
  df %>%
    group_by(across(all_of(c(cine, icine)))) %>%
    summarise(
      n_estudiantes = n(),
      promedio_punt_sabertyt = mean(punt_global, na.rm = TRUE),
      nombre_institucion = first(nombre_institucion),
      snies_programa = paste(unique(estu_snies_prgmacademico), collapse = "; "),
      nombre_del_programa = paste(unique(nombre_del_programa), collapse = "; "),
      .groups = "drop"
    )
}

resumir_por_programa <- function(df) {
  df %>%
    group_by(across(all_of(c("estu_snies_prgmacademico", "nombre_del_programa", "nivel_de_formacion")))) %>%
    summarise(
      n_estudiantes = n(),
      promedio_punt_sabertyt = mean(punt_global, na.rm = TRUE),
      nombre_institucion = first(nombre_institucion),
      periodo = first(año),
      .groups = "drop"
    )
}

# ------------------------------------------------------------
# Función para exportar lista de dataframes con sufijo
# ------------------------------------------------------------
exportar_resumenes <- function(lista_resumenes, grupo_sufijo) {
  carpeta_salida <- "data/Promedios"   # sin barra al inicio ni al final
  
  for (nombre in names(lista_resumenes)) {
    df <- lista_resumenes[[nombre]]
    filename <- paste0("promedios_cine", nombre, "_", grupo_sufijo, ".csv")
    ruta_completa <- file.path(carpeta_salida, filename)
    readr::write_csv(df, ruta_completa)
    message("Exportado: ", ruta_completa)
  }
}

##################################
#LECTURA DE DATOS
##################################

#Leer la base consolidada saber tyt para bogota region
base_sabertyt <- read_delim("data/BD/saber_tyt_bogota_region.csv", escape_double = FALSE, trim_ws = TRUE)
#Leer la base consolidada de los programas de tyt en bogota region
programas_tyt <- read_delim("data/SNIES_CINE_cleaned/base_cine_tyt.csv", escape_double = FALSE, trim_ws = TRUE)

#Cruzar informacion del saber tyt con la informacion de los programas de tyt
data <- left_join(
  base_sabertyt,
  programas_tyt,
  by = join_by(estu_snies_prgmacademico==codigo_snies_del_programa) 
) %>% filter(!(is.na(estu_snies_prgmacademico) | is.na(codigo_institucion)))

#Nota: 372 codigos snies en la bd icfes no encontraron match en la bd del snies
#Codigos snies de programas en la base del saber tyt que no encontraron
#match en la base de los programas tyt.
no_emparejados <- detectar_valores_no_emparejados(
  base_sabertyt,
  programas_tyt,
  "estu_snies_prgmacademico",
  "codigo_snies_del_programa"
  )

#Programas en la base del saber tyt sin correspondencia
no_emparejados$cantidad_valores_sin_correspondencia_referencia
#Programas en la base de programas tyt sin correspondencia
no_emparejados$cantidad_valores_sin_correspondencia_comparacion

##################################
#Resumen datos
##################################
data_summary <- resumen_nans(data)

##################################
#Crear la variable icine
##################################

#Crear la variable ICINE que pretende ser el analogo al INBC
data <- data %>%
  mutate(icine_spec = paste(codigo_institucion, cine_f_2013_ac_campo_especific, sep = "_"),
         icine_detall = paste(codigo_institucion, cine_f_2013_ac_campo_detallado, sep = "_"),
         icine_amplio = paste(codigo_institucion, cine_f_2013_ac_campo_amplio, sep = "_")
  )

##################################
# Para cada icine calculamos: 
# promedio saber tyt
# numero estudiantes por icine
# los programas dentro del icine
##################################

años <- c(2020, 2021, 2022, 2023)

for (año in años) {
  # Filtrar los datos por año
  data_temp <- data %>% filter(año_presentacion == año)
  
  # Resúmenes para nivel "Tecnologico"
  resumenes_tecnologico <- lapply(pares_cines, function(par) {
    resumir_por_cine(data_temp, par$cine, par$icine, nivel_filtrado = "Tecnologico")
  })
  names(resumenes_tecnologico) <- c("amplio", "especifico", "detallado")
  
  # Resúmenes para nivel "Formacion Tecnica Profesional"
  resumenes_tecnica_profesional <- lapply(pares_cines, function(par) {
    resumir_por_cine(data_temp, par$cine, par$icine, nivel_filtrado = "Formacion Tecnica Profesional")
  })
  names(resumenes_tecnica_profesional) <- c("amplio", "especifico", "detallado")
  
  # Exportar resultados con sufijo del año
  exportar_resumenes(resumenes_tecnologico, paste0("tecnologico_", año))
  exportar_resumenes(resumenes_tecnica_profesional, paste0("tecnico_", año))
}


##################################
# Para cada **programa** calculamos: 
# promedio saber tyt
# numero estudiantes
##################################

for (año in años){
  data_temp <- data %>% filter(año_presentacion == año) 
  promedios_programa <- resumir_por_programa(data_temp)
  write_csv(promedios_programa, file = paste0("data/Promedios/promedios_programa_", año, ".csv"))
}




