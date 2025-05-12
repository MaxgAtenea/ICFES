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

#Lectura de los valores agregados por icine
#Cada observación es un ICINE 
data_icine_unico <- read_delim("data/Resultados/va_icine.csv", escape_double = FALSE, trim_ws = TRUE)


##########################################
#2 BOX PLOTS ICINE
##########################################

##########################################
#2.1 TOP Y BOTTOM 10 ICINE 
##########################################

##########################################
#2.1.1 PUNTAJE GLOBAL 
##########################################


# Ordenar y colorear
data_bar <- data_icine_unico %>%
  arrange(coeficiente_PG) %>%
  mutate(color = ifelse(coeficiente_PG < 0, "tomato", "steelblue"))

# Seleccionar top y bottom 10
data_bar <- bind_rows(
  data_bar %>% top_n(10, coeficiente_PG),
  data_bar %>% top_n(-10, coeficiente_PG)
)

data_bar$icine_descrp <- reorder(data_bar$icine_descrp, data_bar$coeficiente_PG)

# Gráfico ggplot
p_barras <- ggplot(data_bar, aes(x = coeficiente_PG, y = icine_descrp, fill = color)) +
  geom_col() +
  geom_text(aes(label = round(coeficiente_PG, 2)),
            hjust = ifelse(data_bar$coeficiente_PG >= 0, -0.1, 1.1),
            size = 3) +
  scale_fill_identity() +
  labs(
    title = "Valor Agregado (Puntaje Global) por ICINE\n Top y Bottom 10",
    x = "coeficiente_PG",
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  coord_cartesian(xlim = c(min(data_bar$coeficiente_PG) - 0.1, 
                           max(data_bar$coeficiente_PG) + 0.1))  # Espacio para etiquetas
print(p_barras)

#guardar el ggplot
ggsave(
  filename = "output/ICINE/va_pg_icine.png",
  plot = p_barras,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 10,
  dpi = 300,
  bg = "white" 
)

##########################################
#2.1.2 LECTURA CRITICA
##########################################

# Ordenar y colorear
data_bar <- data_icine_unico %>%
  arrange(coeficiente_LC) %>%
  mutate(color = ifelse(coeficiente_LC < 0, "tomato", "steelblue"))

# Seleccionar top y bottom 10
data_bar <- bind_rows(
  data_bar %>% top_n(10, coeficiente_LC),
  data_bar %>% top_n(-10, coeficiente_LC)
)

data_bar$icine_descrp <- reorder(data_bar$icine_descrp, data_bar$coeficiente_LC)

# Gráfico ggplot
p_barras <- ggplot(data_bar, aes(x = coeficiente_LC, y = icine_descrp, fill = color)) +
  geom_col() +
  geom_text(aes(label = round(coeficiente_LC, 2)),
            hjust = ifelse(data_bar$coeficiente_LC >= 0, -0.1, 1.1),
            size = 3) +
  scale_fill_identity() +
  labs(
    title = "Valor Agregado (Lectura Crítica) por ICINE\n Top y Bottom 10",
    x = "coeficiente_LC",
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  coord_cartesian(xlim = c(min(data_bar$coeficiente_LC) - 0.1, 
                           max(data_bar$coeficiente_LC) + 0.1))  # Espacio para etiquetas
print(p_barras)

#guardar el ggplot
ggsave(
  filename = "output/ICINE/va_lc_icine.png",
  plot = p_barras,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 10,
  dpi = 300,
  bg = "white" 
)

##########################################
#2.1.3 RAZONAMIENTO CUANTITATIVO 
##########################################
# Ordenar y colorear
data_bar <- data_icine_unico %>%
  arrange(coeficiente_RC) %>%
  mutate(color = ifelse(coeficiente_RC < 0, "tomato", "steelblue"))

# Seleccionar top y bottom 10
data_bar <- bind_rows(
  data_bar %>% top_n(10, coeficiente_RC),
  data_bar %>% top_n(-10, coeficiente_RC)
)

data_bar$icine_descrp <- reorder(data_bar$icine_descrp, data_bar$coeficiente_RC)

# Gráfico ggplot
p_barras <- ggplot(data_bar, aes(x = coeficiente_RC, y = icine_descrp, fill = color)) +
  geom_col() +
  geom_text(aes(label = round(coeficiente_RC, 2)),
            hjust = ifelse(data_bar$coeficiente_RC >= 0, -0.1, 1.1),
            size = 3) +
  scale_fill_identity() +
  labs(
    title = "Valor Agregado (Razonamiento Cuantitativo) por ICINE\n Top y Bottom 10",
    x = "coeficiente_RC",
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  coord_cartesian(xlim = c(min(data_bar$coeficiente_RC) - 0.1, 
                           max(data_bar$coeficiente_RC) + 0.1))  # Espacio para etiquetas
print(p_barras)

#guardar el ggplot
ggsave(
  filename = "output/ICINE/va_rc_icine.png",
  plot = p_barras,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 10,
  dpi = 300,
  bg = "white" 
)
##########################################
#2.2 TODOS LOS ICINE 
##########################################

##########################################
#2.2.1 PUNTAJE GLOBAL
##########################################

# Ordenar y preparar etiquetas
data_bar <- data_icine_unico %>%
  arrange(coeficiente_PG) %>%
  mutate(
    etiqueta = factor(icine_descrp, levels = unique(icine_descrp)),
    color = ifelse(coeficiente_PG < 0, "tomato", "steelblue")
  )

# Crear gráfico de barras
p_barras <- plot_ly(
  data_bar,
  x = ~coeficiente_PG,
  y = ~etiqueta,
  type = "bar",
  orientation = "h",
  marker = list(color = ~color),  # color definido por variable
  height = 30 * nrow(data_bar)
) %>%
  layout(
    title = "Valor Agregado (Puntaje Global) por ICINE",
    xaxis = list(title = "coeficiente_PG"),
    yaxis = list(title = "", tickfont = list(size = 9)),
    margin = list(l = 150)
  )

p_barras

#3. Guardar el boxplot
#guardar el plotly
htmlwidgets::saveWidget(p_barras, file = "output/ICINE/va_pg_icine.html")


##########################################
#2.2.1 LECTURA CRITICA
##########################################
# Ordenar y preparar etiquetas
data_bar <- data_icine_unico %>%
  arrange(coeficiente_LC) %>%
  mutate(
    etiqueta = factor(icine_descrp, levels = unique(icine_descrp)),
    color = ifelse(coeficiente_LC < 0, "tomato", "steelblue")
  )

# Crear gráfico de barras
p_barras <- plot_ly(
  data_bar,
  x = ~coeficiente_LC,
  y = ~etiqueta,
  type = "bar",
  orientation = "h",
  marker = list(color = ~color),  # color definido por variable
  height = 30 * nrow(data_bar)
) %>%
  layout(
    title = "Valor Agregado (Lectura Crítica) por ICINE",
    xaxis = list(title = "coeficiente_LC"),
    yaxis = list(title = "", tickfont = list(size = 9)),
    margin = list(l = 150)
  )

p_barras

#3. Guardar el boxplot
#guardar el plotly
htmlwidgets::saveWidget(p_barras, file = "output/ICINE/va_lc_icine.html")


##########################################
#2.2.1 RAZONAMIENTO CUANTITATIVO
##########################################

# Ordenar y preparar etiquetas
data_bar <- data_icine_unico %>%
  arrange(coeficiente_RC) %>%
  mutate(
    etiqueta = factor(icine_descrp, levels = unique(icine_descrp)),
    color = ifelse(coeficiente_RC < 0, "tomato", "steelblue")
  )

# Crear gráfico de barras
p_barras <- plot_ly(
  data_bar,
  x = ~coeficiente_RC,
  y = ~etiqueta,
  type = "bar",
  orientation = "h",
  marker = list(color = ~color),  # color definido por variable
  height = 30 * nrow(data_bar)
) %>%
  layout(
    title = "Valor Agregado (Razonamiento Cuantitativo) por ICINE",
    xaxis = list(title = "coeficiente_RC"),
    yaxis = list(title = "", tickfont = list(size = 9)),
    margin = list(l = 150)
  )

p_barras

#3. Guardar el boxplot
#guardar el plotly
htmlwidgets::saveWidget(p_barras, file = "output/ICINE/va_rc_icine.html")
