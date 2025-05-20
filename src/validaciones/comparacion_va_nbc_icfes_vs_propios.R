##################################
#Set working directory
##################################
setwd("/home/alejandro/Documentos/ATENEA/Despacho/ICFES")

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
         "gridExtra",
         "lme4",
         "data.tree",
         "jsonlite",
         "openxlsx",
         "purrr",
         "patchwork"),
       library,
       character.only = TRUE
)

##########################################
# CONSTANTES
##########################################
#columnas de interes en el excel con los resultados del icfes del va
columnas_va_icfes <-c(
  "nbc",
  "inst_cod_institucion",
  "inst_nbc",
  "va_lectura_critica",
  "va_razona_cuantitat"
)

##########################################
# FUNCIONES
##########################################


limpiar_texto_nbc <- function(texto) {
  texto %>%
    stringr::str_to_lower() %>%                     # Convertir a minúsculas
    stringi::stri_trans_general("latin-ascii") %>%  # Eliminar tildes
    stringr::str_squish() %>%                       # Eliminar espacios extra
    stringr::str_trim() %>%                         # Eliminar espacios al inicio/final
    stringr::str_to_title()                         # Capitalizar palabras
}


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

#' Resumen de inbcs y numero estudiantes por campo CINE
#'
#' Esta función agrupa los datos por un campo NBC
#' y resume el número total de personas y la cantidad de inbc distintos.
#'
#' @param data Dataframe a analizar
#' @param columna_nbc Nombre de la columna de agrupación NBC
#' @return Un dataframe con los totales de personas y de instituciones distintas por campo NBC
observaciones_por_nbc <- function(data, columna_nbc, columna_inbc) {
  data %>%
    group_by(.data[[columna_nbc]]) %>%
    summarise(
      conteo_inbc = n_distinct(.data[[columna_inbc]]),
      .groups = "drop"
    ) %>%
    arrange(desc(conteo_inbc))
}


##########################################
#1. LECTURA DE LA DATA
##########################################
#Valor agregado inbc (calculos propios)
va_propio <- read_delim("data/Resultados_VA/nbc/bogota_region/va_nbc_2021_2022.csv", escape_double = FALSE, trim_ws = TRUE)
va_icfes <- read_excel("data/Resultados_ICFES_VA/VA_INBCs_Bogota.xlsx")


#Seleccionar las columnas de interes
va_icfes <- va_icfes %>%
  select(all_of(columnas_va_icfes))

##########################################
#2. HOMOGENEIZAR COLUMNAS
##########################################

#se eliminan tildes y mayusculas en columna de inbc
va_icfes$inst_nbc <- limpiar_texto_nbc(va_icfes$inst_nbc)
va_propio$inbc <- limpiar_texto_nbc(va_propio$inbc)

#se eliminan tildes y mayusculas en columna de nucleo basico del conocimiento
va_icfes$nbc <- limpiar_texto_nbc(va_icfes$nbc)
va_propio$nucleo_basico_del_conocimiento <- limpiar_texto_nbc(va_propio$nucleo_basico_del_conocimiento)


#codigo institucion se trata como char
va_icfes <- va_icfes %>%
  mutate(inst_cod_institucion = as.character(inst_cod_institucion))
va_propio <- va_propio %>%
  mutate(codigo_institucion = as.character(codigo_institucion))

#cambiar el nombre del nbc Arquitectura y Afines
va_icfes <- va_icfes %>%
  mutate(nbc = if_else(nbc == "Arquitectura", "Arquitectura Y Afines", nbc))

##########################################
#2. UNIR DFs
##########################################

va_merged <- full_join(
  va_icfes,
  va_propio,
  by = c(
    "inst_cod_institucion" = "codigo_institucion",
    "nbc" = "nucleo_basico_del_conocimiento"
  )
)

va_merged <- va_merged %>%
  mutate(
    va_lectura_critica = round(va_lectura_critica, 2),
    va_razona_cuantitat = round(va_razona_cuantitat, 2),
    coeficiente_LC = round(coeficiente_LC, 2),
    coeficiente_RC = round(coeficiente_RC, 2),
    diferencia_lc = abs(va_lectura_critica - coeficiente_LC),
    diferencia_rc = abs(va_razona_cuantitat - coeficiente_RC),
    diferencia_signo_lc = sign(va_lectura_critica) != sign(coeficiente_LC),
    diferencia_signo_rc = sign(va_razona_cuantitat) != sign(coeficiente_RC)
  )


va_merged_summary <- resumen_nans(va_merged)

va_merged_sin_na <- va_merged %>% drop_na()


##########################################
#3 NBC e instituciones que no coincidieron
##########################################

conteo_nbc_propio <- observaciones_por_nbc(va_propio, "nucleo_basico_del_conocimiento", "inbc")
conteo_nbc_icfes <- observaciones_por_nbc(va_icfes, "nbc", "inst_nbc")

conteos <- full_join(
  conteo_nbc_icfes,
  conteo_nbc_propio,
  by = c(
    "nbc" = "nucleo_basico_del_conocimiento"
  ),
  suffix = c("_icfes", "_propio")
)

#cambiar na por 0
conteos <- conteos %>%
  mutate(
    across(where(is.numeric), ~replace_na(., 0)),
    diferencia = conteo_inbc_propio - conteo_inbc_icfes
    )


##########################################
#4 Ranking IES por cada NBC
##########################################
attach(va_merged_sin_na)

va_merged_sin_na <- va_merged_sin_na %>%
  group_by(nbc) %>%
  mutate(
    ranking_coeficiente_rc = percent_rank(desc(coeficiente_RC)),
    ranking_coeficiente_lc = percent_rank(desc(coeficiente_LC)),
    ranking_va_lectura_critica = percent_rank(desc(va_lectura_critica)),
    ranking_va_razona_cuantitat = percent_rank(desc(va_razona_cuantitat))
  ) %>%
  ungroup()


va_merged_sin_na <- va_merged_sin_na %>%
  mutate(
    diferencia_ranking_rc = ranking_va_razona_cuantitat - ranking_coeficiente_rc,
    diferencia_ranking_lc = ranking_va_lectura_critica - ranking_coeficiente_lc,
  )

# Histograma para diferencia_ranking_rc
p1 <- ggplot(va_merged_sin_na, aes(x = diferencia_ranking_rc)) +
  geom_histogram(fill = "darkblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Diferencia Ranking VA-RC ",
    x = "Diferencia de Ranking RC",
    y = "Frecuencia"
  )

# Histograma para diferencia_ranking_lc
p2 <-ggplot(va_merged_sin_na, aes(x = diferencia_ranking_lc)) +
  geom_histogram(fill = "darkblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Diferencia Ranking VA-LC ",
    x = "Diferencia de Ranking LC",
    y = "Frecuencia"
  )

p1 + p2 
##########################################
#5 correlacion entre los resultados del icfes y resultados propios
##########################################

# Función con tidy eval para graficar scatter con correlación
plot_correlation <- function(df, xvar, yvar) {
  # Calcular correlación y pendiente
  modelo <- lm(df[[yvar]] ~ df[[xvar]])
  #pendiente <- coef(modelo)[2] %>% round(2)
  cor_val <- cor(df[[xvar]], df[[yvar]], use = "complete.obs") %>% round(2)
  
  # Etiqueta que muestra correlación y pendiente
  #etiqueta <- paste0("Cor = ", cor_val, "\nPendiente = ", pendiente)
  etiqueta <- paste0("Cor = ", cor_val)
  # Gráfico
  ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    annotate("text", x = Inf, y = -Inf, label = etiqueta,
             hjust = 1.1, vjust = -0.5, size = 5, color = "red") +
    theme_minimal() +
    labs(
      title = "Valor Agregado Razonamiento Cuantitativo",
      x = "VA Propio",
      y = "VA ICFES"
    )
}

# Crear gráficos
p1 <- plot_correlation(va_merged_sin_na, "coeficiente_LC", "va_lectura_critica")
p2 <- plot_correlation(va_merged_sin_na, "coeficiente_RC", "va_razona_cuantitat")

# Mostrar juntos
grid.arrange(p2, p1, ncol = 1)

