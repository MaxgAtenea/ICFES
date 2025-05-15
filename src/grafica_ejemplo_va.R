library(ggplot2)
library(dplyr)
library(ggrepel)

setwd("/home/alejandro/Documentos/ATENEA/Despacho/ICFES")
library(lme4)
library(tidyverse)
library(ggplot2)

# 1. Leer y filtrar datos
data_filtrado_2023 <- read_delim("data/BD/data_estimacion_2023.csv", escape_double = FALSE, trim_ws = TRUE)

datos_arq <- data_filtrado_2023 %>%
  filter(cine_f_2013_ac_campo_especific == "Arquitectura y construcción")

# 2. Ajustar el modelo mixto
modelo_arq <- lmer(
  punt_global_bdsaberpro ~ punt_global_bdsaber11_conciliado + (1 | icine),
  data = datos_arq
)
library(plotly)
library(dplyr)
library(broom.mixed)

# 1. Obtener coeficientes por grupo
interceptos <- ranef(modelo_arq)$icine %>%
  as.data.frame() %>%
  rename(intercepto = `(Intercept)`) %>%
  mutate(icine = rownames(ranef(modelo_arq)$icine)) %>%
  arrange(intercepto)

icine_min <- interceptos$icine[1]
icine_max <- interceptos$icine[nrow(interceptos)]

# 2. Obtener pendiente global
pendiente_global <- fixef(modelo_arq)["punt_global_bdsaber11_conciliado"]
intercepto_global <- fixef(modelo_arq)["(Intercept)"]

# 3. Crear grid de valores de X
x_vals <- seq(200, 400, length.out = 100)

# 4. Funciones para las tres líneas
linea_global <- data.frame(
  x = x_vals,
  y = intercepto_global + pendiente_global * x_vals,
  tipo = "Global"
)

linea_min <- data.frame(
  x = x_vals,
  y = (intercepto_global + interceptos$intercepto[1]) + pendiente_global * x_vals,
  tipo = paste0("ICINE: ", icine_min)
)

linea_max <- data.frame(
  x = x_vals,
  y = (intercepto_global + interceptos$intercepto[nrow(interceptos)]) + pendiente_global * x_vals,
  tipo = paste0("ICINE: ", icine_max)
)

# 5. Puntos individuales solo para los dos grupos extremos
datos_extremos <- datos_arq %>%
  filter(icine %in% c(icine_min, icine_max))

# 6. Plotly: agregar líneas y puntos
plot_ly() %>%
  add_trace(data = datos_extremos,
            x = ~punt_global_bdsaber11_conciliado,
            y = ~punt_global_bdsaberpro,
            color = ~icine,
            type = 'scatter',
            mode = 'markers',
            marker = list(opacity = 0.5),
            showlegend = FALSE) %>%
  add_trace(data = linea_global,
            x = ~x,
            y = ~y,
            type = 'scatter',
            mode = 'lines',
            name = 'Global',
            line = list(color = 'black', dash = 'dashdot')) %>%
  add_trace(data = linea_min,
            x = ~x,
            y = ~y,
            type = 'scatter',
            mode = 'lines',
            name = "Universidad Nacional",
            line = list(color = 'khaki')) %>%
  add_trace(data = linea_max,
            x = ~x,
            y = ~y,
            type = 'scatter',
            mode = 'lines',
            name = "Universidad Católica",
            line = list(color = 'palegreen')) %>%
  layout(
    title = list(
      text = "Regresiones por ICINE (extremos)",   # Título del gráfico
      font = list(size = 20)                       # Tamaño de la fuente del título (puedes ajustarlo)
    ),
    xaxis = list(title = "Saber 11", range = c(200, 300)),
    yaxis = list(title = "Saber PRO", range = "auto"),
    legend = list(x = 0.05, y = 0.95),
    width = 1000,  # Aumenta el ancho del gráfico
    height = 800   # Aumenta la altura del gráfico
  ) 
