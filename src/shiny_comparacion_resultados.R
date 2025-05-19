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
         "purrr",
         "shiny",
         "DT"
         ),
       library,
       character.only = TRUE
)

##########################################
# FUNCIONES
##########################################

#' Calcular estadísticas de boxplot agrupadas por categorías
#'
#' Esta función calcula los valores estadísticos necesarios para construir un boxplot
#' (cuartil inferior, mediana, cuartil superior, IQR, y límites inferior y superior)
#' agrupando por las columnas `cine_f_2013_ac_campo_especific` e `icine`.
#'
#' @param data Un dataframe que contiene al menos una variable numérica y las columnas categóricas mencionadas.
#' @param var_numerica La variable numérica (sin comillas) sobre la cual calcular los estadísticos del boxplot.
#'
#' @return Un dataframe con una fila por cada combinación de `cine_f_2013_ac_campo_especific` e `icine`,
#'         y columnas con los valores de: lower (Q1), middle (mediana), upper (Q3),
#'         iqr (rango intercuartílico), ymin y ymax (límites del bigote).
#'
#' @examples
#' resumen_boxplot_por_grupo(data_filtrado, mod_lectura_critica_punt)
resumen_boxplot_por_grupo <- function(data, var_numerica) {
  var_numerica <- enquo(var_numerica)
  data %>%
    group_by(cine_f_2013_ac_campo_especific, icine) %>%
    summarise(
      lower = quantile(!!var_numerica, 0.25, na.rm = TRUE),
      middle = quantile(!!var_numerica, 0.5, na.rm = TRUE),
      upper = quantile(!!var_numerica, 0.75, na.rm = TRUE),
      iqr = IQR(!!var_numerica, na.rm = TRUE),
      ymin = lower - 1.5 * iqr,
      ymax = upper + 1.5 * iqr,
      .groups = "drop"
    )
}

#' marcar_extremos_por_cine
#'
#' Esta función filtra un conjunto de datos para un valor específico de la variable `cine`,
#' calcula la distancia al origen usando los coeficientes LC y RC, asigna el cuadrante correspondiente
#' a cada punto en el plano cartesiano, y marca como extremos los 3 puntos más alejados del origen
#' dentro de cada cuadrante (permitiendo empates).
#' 
#' Además, añade las columnas:
#' - `distancia_origen`: distancia euclidiana desde el origen (0,0),
#' - `cuadrante`: cuadrante en el plano según signos de los coeficientes,
#' - `es_extremo`: indicador binario (1 = extremo, 0 = no extremo),
#' - `color`: etiqueta "Puntos Extremos" para los extremos, útil para graficar.
#'
#' @param data Un data.frame con las columnas: coeficiente_LC, coeficiente_RC y cine.
#' @param cine_valor El valor de la variable `cine` a filtrar (ej. "Derecho").
#'
#' @return Un data.frame con nuevas columnas que identifican los puntos extremos en cada cuadrante.

marcar_extremos_por_cine <- function(data, cine_valor) {
  data %>%
    filter(cine_f_2013_ac_campo_especific == cine_valor) %>%
    mutate(
      # Calcular la distancia al origen
      distancia_origen = sqrt(coeficiente_LC^2 + coeficiente_RC^2),
      
      # Redondear las coordenadas a dos decimal
      coeficiente_LC = round(coeficiente_LC, 2),
      coeficiente_RC = round(coeficiente_RC, 2),
      
      # Determinar el cuadrante
      cuadrante = case_when(
        coeficiente_LC >= 0 & coeficiente_RC >= 0 ~ "Q1",
        coeficiente_LC <  0 & coeficiente_RC >= 0 ~ "Q2",
        coeficiente_LC <  0 & coeficiente_RC <  0 ~ "Q3",
        coeficiente_LC >= 0 & coeficiente_RC <  0 ~ "Q4"
      )
    ) %>%
    group_by(cuadrante) %>%
    mutate(
      # Marcar los extremos según la distancia al origen
      es_extremo = if_else(min_rank(desc(distancia_origen)) <= 3, 1, 0)
    ) %>%
    ungroup() %>%
    mutate(
      # Definir el color de los puntos extremos
      color = if_else(es_extremo == 1, "Puntos Extremos", NA_character_)
    )
}


#' Función para graficar el Valor Agregado de Lectura Crítica vs Razonamiento Cuantitativo
#' para un valor específico de cine. La función filtra los datos, marca los puntos extremos
#' en función de las distancias al origen y genera un gráfico con cuadrantes, puntos normales y extremos.
#' Parámetros:
#' - @data: dataframe con los datos a graficar.
#' - @cine_valor: valor de cine para filtrar los datos y personalizar el título del gráfico.
graficar_valor_agregado_comparacion <- function(data, cine_valor) {
  lim <- max(abs(c(data$coeficiente_LC, data$coeficiente_RC)), na.rm = TRUE)
  
  # Colores dinámicos según los valores en periodo_origen
  periodos <- unique(data$periodo_origen)
  colores_periodo <- setNames(
    c("#1f77b4", "red")[seq_along(periodos)],
    periodos
  )
  
  ggplot(data, aes(x = coeficiente_LC, y = coeficiente_RC)) +
    geom_rect(aes(xmin = 0, xmax = lim, ymin = 0, ymax = lim),
              fill = "palegreen", alpha = 0.2, inherit.aes = FALSE) +
    geom_rect(aes(xmin = -lim, xmax = 0, ymin = -lim, ymax = 0),
              fill = "khaki", alpha = 0.2, inherit.aes = FALSE) +
    
    geom_point(
      data = filter(data, es_extremo != 1),
      aes(color = periodo_origen, text = paste0(nombre_institucion, " (n=", n_estudiantes, ")")),
      alpha = 0.3, size = 3
    ) +
    geom_point(
      data = filter(data, es_extremo == 1),
      aes(color = periodo_origen, text = paste0(nombre_institucion, " (n=", n_estudiantes, ")")),
      alpha = 0.9, size = 3
    ) +
    
    geom_vline(xintercept = 0, linetype = "solid", color = "black") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    annotate("text", x = 0, y = 0, label = "(0, 0)", hjust = -0.2, vjust = -0.5, size = 3) +
    
    labs(
      title = "Valor Agregado\nLectura Crítica vs Razonamiento Cuantitativo",
      subtitle = paste0("CINE ", cine_valor),
      x = "Lectura Crítica",
      y = "Razonamiento Cuantitativo",
      color = "Periodo"
    ) +
    scale_color_manual(values = colores_periodo) +
    xlim(-lim, lim) +
    ylim(-lim, lim) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "top"
    )
}





##########################################
# LECTURA DATOS
##########################################
# Directorio donde están los CSV
carpeta <- "data/Resultados_VA/cine_especifico/bogota_region"

# Obtener todos los archivos .csv
archivos <- list.files(carpeta, pattern = "\\.csv$", full.names = TRUE)

# Cargar todos los archivos como una lista de dataframes
resultados_va_list <- lapply(archivos, read_delim, escape_double = FALSE, trim_ws = TRUE)

# Asignar nombres con base en el nombre del archivo sin extensión
names(resultados_va_list) <- archivos |>
  basename() |>
  str_remove("\\.csv$")

resultados_va <- bind_rows(resultados_va_list, .id = "origen_archivo")
##########################################
# 1. GRAFICAS VA_LC vs VA_RC
# por cada CINE
##########################################

##########################################
# 1.1 GRAFICAS VA_LC vs VA_RC 
# por cada CINE
# Todos los periodos del Saber Pro
##########################################

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Gráficos Interactivos por CINE"),
  
  # Inputs en fila horizontal
  fluidRow(
    column(width = 4,
           selectInput("periodo_input", "Selecciona un Periodo:", 
                       choices = sort(unique(resultados_va$periodo)))
    ),
    column(width = 4,
           selectInput("cine_input", "Selecciona un CINE:", choices = NULL)
    ),
    column(width = 4,
           selectInput("periodo_comparar_input", "Periodo a comparar (opcional):", 
                       choices = c("Ninguno" = "", sort(unique(resultados_va$periodo))))
    )
  ),
  
  # Tablas y gráfica
  fluidRow(
    column(width = 6,
           fluidRow(
             column(width = 6,
                    uiOutput("titulo_tabla_principal"),
                    dataTableOutput("universidades_q1q3")
             ),
             column(width = 6,
                    conditionalPanel(
                      condition = "input.periodo_comparar_input != ''",
                      uiOutput("titulo_tabla_comparar"),
                      dataTableOutput("universidades_q1q3_comparar")
                    )
             )
           )
    ),
    column(width = 6,
           plotlyOutput("grafico_interactivo", height = "700px")
    )
  )
)



# Lógica del servidor
server <- function(input, output, session) {
  
  # Data reactiva filtrada por periodo principal
  data_por_periodo <- reactive({
    req(input$periodo_input)
    resultados_va %>%
      filter(periodo == input$periodo_input)
  })
  
  # Actualizar CINEs disponibles según periodo principal
  observeEvent(input$periodo_input, {
    req(data_por_periodo())
    updateSelectInput(session, "cine_input",
                      choices = sort(unique(data_por_periodo()$cine_f_2013_ac_campo_especific)),
                      selected = character(0))
  })
  
  # Datos filtrados para tabla (solo del periodo principal)
  data_filtrada <- reactive({
    req(input$cine_input)
    marcar_extremos_por_cine(data_por_periodo(), input$cine_input)
  })
  
  # Gráfico interactivo con opción de comparación
  output$grafico_interactivo <- renderPlotly({
    req(input$cine_input)
    
    # Data principal
    df_principal <- marcar_extremos_por_cine(data_por_periodo(), input$cine_input) %>%
      mutate(periodo_origen = input$periodo_input)
    
    # Data de comparación (si existe)
    df_comparar <- NULL
    if (!is.null(input$periodo_comparar_input) && input$periodo_comparar_input != "") {
      df_comparar <- marcar_extremos_por_cine(
        resultados_va %>% filter(periodo == input$periodo_comparar_input),
        input$cine_input
      ) %>%
        mutate(periodo_origen = input$periodo_comparar_input)
    }
    
    # Combinar datos
    df_combined <- bind_rows(df_principal, df_comparar)
    
    # Graficar
    p <- graficar_valor_agregado_comparacion(df_combined, input$cine_input)
    
    ggplotly(p, tooltip = c("x", "y", "text")) %>%
      layout(title = list(
        text = paste0("Valor Agregado<br><sub>Periodo(s): ",
                      input$periodo_input,
                      if (!is.null(input$periodo_comparar_input) && input$periodo_comparar_input != "") {
                        paste0(" vs ", input$periodo_comparar_input)
                      } else "", 
                      " | CINE: ", input$cine_input, "</sub>"),
        x = 0.5
      ))
  })
  
  # Tabla de universidades en Q1 y Q3 (solo periodo principal)
  output$universidades_q1q3 <- renderDataTable({
    data_filtrada() %>%
      filter(cuadrante %in% c("Q1", "Q3")) %>%
      mutate(magnitud = round(distancia_origen, 1)) %>%
      select(nombre_institucion, cuadrante, magnitud) %>%
      arrange(cuadrante, desc(magnitud)) %>%
      datatable(
        options = list(
          pageLength = 10,
          columnDefs = list(
            list(targets = 1, visible = FALSE)  # Oculta la columna 'cuadrante' (índice 1 empieza en 0)
          )
        )
      ) %>%
      formatStyle(
        'cuadrante',
        target = 'row',
        backgroundColor = styleEqual(
          c("Q1", "Q3"),
          c("rgba(152, 251, 152, 0.3)",  # palegreen con alpha 0.3
            "rgba(240, 230, 140, 0.3)")  # khaki con alpha 0.3
        )
      )
  })
  
  # Tabla de universidades en Q1 y Q3 (periodo de comparación)
  output$universidades_q1q3_comparar <- renderDataTable({
    req(input$periodo_comparar_input)
    req(input$periodo_comparar_input != "")
    req(input$cine_input)
    
    data_comparar <- marcar_extremos_por_cine(
      resultados_va %>% filter(periodo == input$periodo_comparar_input),
      input$cine_input
    )
    
    data_comparar %>%
      filter(cuadrante %in% c("Q1", "Q3")) %>%
      mutate(magnitud = round(distancia_origen, 1)) %>%
      select(nombre_institucion, cuadrante, magnitud) %>%
      arrange(cuadrante, desc(magnitud)) %>%
      datatable(
        options = list(
          pageLength = 10,
          columnDefs = list(
            list(targets = 1, visible = FALSE)  # Oculta la columna 'cuadrante'
          )
        )
      ) %>%
      formatStyle(
        'cuadrante',
        target = 'row',
        backgroundColor = styleEqual(
          c("Q1", "Q3"),
          c("rgba(152, 251, 152, 0.3)",  # palegreen con alpha 0.3
            "rgba(240, 230, 140, 0.3)")  # khaki con alpha 0.3
        )
      )
  })
  
  
  # Título dinámico para tabla principal
  output$titulo_tabla_principal <- renderUI({
    req(input$periodo_input)
    h4(paste0("Universidades en Q1 y Q3 (Periodo: ", input$periodo_input, ")"))
  })
  
  # Título dinámico para tabla de comparación
  output$titulo_tabla_comparar <- renderUI({
    req(input$periodo_comparar_input)
    if (input$periodo_comparar_input == "") return(NULL)
    h4(paste0("Universidades en Q1 y Q3 (Periodo: ", input$periodo_comparar_input, ")"))
  })
  
}


# Ejecutar la aplicación
shinyApp(ui = ui, server = server)


########################################
#PENDIENTE
########################################

# library(patchwork)
# #Ejemplo de una grafica
# graficar_valor_agregado(resultados_va,"Derecho")
# #Figura con todas las graficas
# # Crear una lista vacía para almacenar las gráficas
# graficas <- list()
# # Loop para crear las 15 gráficas, una por cada valor de cine
# for (cine_valor in unique(resultados_va$cine)) {
#   # Crear cada gráfico usando la función definida previamente
#   graficas[[cine_valor]] <- graficar_valor_agregado(resultados_va, cine_valor)
# }
# # Unir todas las gráficas en una sola figura, distribuidas en 3 filas por 5 columnas
# grilla_valores_agregados <- wrap_plots(graficas, ncol = 5)
# # Mostrar la figura final
# #ggsave("output/grilla_valores_agregados.png", plot = grilla_valores_agregados, width = 15, height = 10)